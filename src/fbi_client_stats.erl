%%% vim: set ts=4 sts=4 sw=4 et:

-module(fbi_client_stats).
-export([
    start_link/1,
    submit/3
    ]).

-behavior(gen_server).
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
]).

-record(udp_server_info, { sock, my_ip = <<0,0,0,0>> }).
-record(state, { tab, server_info, sequence = 0, realm }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Realm) ->
    gen_server:start_link({local, fbi_ccfg:client_stats_proc(Realm)}, ?MODULE, Realm, []).

submit(Realm, Keys, Event) -> 
    lists:foreach(fun(Key) -> raw_submit(Realm, Key, Event) end, Keys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Realm) ->
    erlang:start_timer(flush_timeout(), self(), flush_to_server),
    {ok, Sock} = gen_udp:open(0),
    random:seed(now()),
    put(sequence, 0),
    {ok, #state{
        server_info = #udp_server_info{ sock = Sock },
        tab = ets:new(fbi_ccfg:client_stats_tab(Realm), [set, public]),
        realm = Realm
    }}.

handle_call(_, _From, #state{} = State) ->
    {noreply, State}.

handle_cast({submit, {activity, _, _} = Metric, Amount, ElapsedMS}, #state{ tab = T } = State) ->
    case ets:lookup(T, Metric) of
        [] -> ets:insert(T, {Metric, Amount, ElapsedMS});
        [{_, Seen, LatencySum}] ->
            ets:insert(T, {Metric, Seen+Amount, LatencySum + ElapsedMS})
    end,
    {noreply, State};
handle_cast({submit, {value, _} = Metric, Value}, #state{ tab = T } = State) ->
    ets:insert(T, {Metric, Value}),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, _, flush_to_server}, #state{tab = T, server_info = ServerInfo, realm = Realm} = State) ->
    send_all_metrics(Realm, ServerInfo, T),
    ets:delete_all_objects(T),
    erlang:start_timer(flush_timeout(), self(), flush_to_server),
    {noreply, State#state{ sequence = get(sequence) }};
handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% INTERNAL FUNCTIONS

raw_submit(Realm, Key, {activity, Outcome, Amount, ElapsedMS}) when byte_size(Key) < 250, is_integer(ElapsedMS) ->
    Code = code_of_outcome(Outcome),
    Metric = {activity, Code, Key},
    try
        % Fast path; small potential to lose fast-moving counters
        ets:update_counter(fbi_ccfg:client_stats_tab(Realm), Metric, [{2,Amount},{3,ElapsedMS}])
    catch _:_ ->
        % Slow path
        gen_server:cast(fbi_ccfg:client_stats_proc(Realm), {submit, Metric, Amount, ElapsedMS})
    end;
raw_submit(Realm, Key, {value, V}) when byte_size(Key) < 250, is_number(V) ->
    Value = float(V),
    Metric = {value, Key},
    gen_server:cast(fbi_ccfg:client_stats_proc(Realm), {submit, Metric, Value}).

code_of_outcome(hit) -> 1;
code_of_outcome(miss) -> 0.

send_all_metrics(Realm, ServerInfo, T) ->
    RemainderBuf = ets:foldl(fun
        ({{activity, Code, Key}, N, LatencySum}, Buf) ->
            Latency = case N of
                0 -> LatencySum;
                _ -> LatencySum div N
            end,
            append_metric_and_maybe_send(Realm, ServerInfo, Buf, {{activity, Code, Key}, N, Latency});
        (Metric, Buf) ->
            append_metric_and_maybe_send(Realm, ServerInfo, Buf, Metric)
    end, <<>>, T),
    send_udp(Realm, ServerInfo, RemainderBuf).

append_metric_and_maybe_send(Realm, ServerInfo, Buf, {{activity, Code, Key}, N, Latency}) when N > 65535 ->
    RemainderBuf = append_metric_and_maybe_send(Realm, ServerInfo, Buf,
        {{activity, Code, Key}, 65535, Latency}),
    append_metric_and_maybe_send(Realm, ServerInfo, RemainderBuf,
        {{activity, Code, Key}, N - 65535, Latency});
append_metric_and_maybe_send(Realm, ServerInfo, Buf, {{activity, Code, Key}, N, Latency}) ->
    pack_and_maybe_send(Realm, ServerInfo, Buf, 0, Key, <<Code:8, N:16, Latency:16>>);
append_metric_and_maybe_send(Realm, ServerInfo, Buf, {{value, Key}, Value}) ->
    pack_and_maybe_send(Realm, ServerInfo, Buf, 1, Key, <<Value/float>>).

pack_and_maybe_send(Realm, ServerInfo, Buf, Type, Key, Params) ->
    {FitKey, KeySize} = case byte_size(Key) of
        KS when KS =< 255 -> {Key, KS};
        _ -> {erlang:binary_part(Key, 0, 255), 255}
    end,
    ToAdd = <<Type:8, KeySize:8, FitKey/binary, Params/binary>>,
    if
        (byte_size(Buf) + byte_size(ToAdd)) > 1400 ->
            send_udp(Realm, ServerInfo, Buf), ToAdd;
        true ->
            <<Buf/binary, ToAdd/binary>>
    end.

% @spec send_udp(ServerInfo, Buf) -> ServerInfo
send_udp(_Realm, _ServerInfo, <<>>) -> ok;
send_udp(Realm, #udp_server_info{sock = Sock, my_ip = MyIP}, Buf) ->
    Seq = get(sequence),
    Packet = <<(_Magic=42):8, (_Version=2):8, (Seq rem 65536):16, MyIP/binary,
                Buf/binary>>,
    {Host, Port} = fbi_ccfg:server_hostport(Realm),
    gen_udp:send(Sock, Host, Port, Packet),
    % Duplicate packet sending for debugging purposes
    % Remove when completed (ask aovchinn@aboutecho.com)
    % gen_udp:send(Sock, "nfbi2", 1811, Packet),
    put(sequence, Seq + 1).

flush_timeout() ->
    Misc = case application:get_env(fbi_client, misc) of
        {ok, M} -> M;
        _ -> []
    end,
    Base = proplists:get_value(client_stats_flush_timeout_base_ms, Misc, 1000),
    Base + random:uniform(Base).
