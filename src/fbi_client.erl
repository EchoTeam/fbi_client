%%% vim: set ts=4 sts=4 sw=4 expandtab:
-module(fbi_client).
-export([
    start_link/1,
    flag/2,
    data_of_blob/1,

    % use for tests only
    add_flag/2,
    delete_flag/2,
    clear_cache/1,

    % temp
    add_event_handler/2,
    delete_event_handler/2
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

-record(state, { flagmap, fbi_server, awaiting_pong = false, realm }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Realm) ->
    gen_server:start_link({local, fbi_ccfg:client_proc(Realm)}, ?MODULE, Realm, []).

flag(Realm, Flag) -> fbi_sdb_cache:flag(fbi_ccfg:client_tab(Realm), Flag).

add_flag(Realm, Flag) ->
    fbi_sdb_cache:add_flag(fbi_ccfg:client_tab(Realm), Flag).

delete_flag(Realm, Flag) ->
    fbi_sdb_cache:delete_flag(fbi_ccfg:client_tab(Realm), Flag).

clear_cache(Realm) ->
    fbi_sdb_cache:clear(fbi_ccfg:client_tab(Realm)).

add_event_handler(Handler, Args) ->
    gen_event:add_sup_handler(fbi_client_events, Handler, Args). 

delete_event_handler(Handler, Args) ->
    gen_event:delete_handler(fbi_client_events, Handler, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Realm) ->
    erlang:send_after(100, self(), reconnect),
    erlang:send_after(1000, self(), heartbeat),
    random:seed(now()),
    FlagMap = fbi_sdb_cache:init_flags_map(fbi_ccfg:client_tab(Realm)),
    {ok, #state{ flagmap = FlagMap, realm = Realm }}.

% TODO: debug only, remove
handle_call(disconnect, _From, #state{fbi_server = Socket} = State) ->
    gen_tcp:close(Socket),
    {reply, ok, State};
handle_call(start_heartbeat, _From, State) ->
    erlang:send_after(1000, self(), heartbeat),
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, #state{fbi_server = Socket, realm = Realm} = State) ->
    NewState = case data_of_blob(Data) of
        {ok, {flagged, Flag, Label}} ->
            fbi_sdb_cache:add_flag(fbi_ccfg:client_tab(Realm), Flag),
            rise_flagged_event(Realm, Flag, Label),
            State;
        {ok, {unflagged, Flag, Label}} ->
            fbi_sdb_cache:delete_flag(fbi_ccfg:client_tab(Realm), Flag),
            rise_unflagged_event(Realm, Flag, Label),
            State;
        pong ->
            State#state{awaiting_pong = false};
        error ->
            lager:info("FBI client parse data failed: ~p", [Data]),
            State
    end,
    {noreply, NewState};
handle_info({tcp_closed, Socket}, #state{fbi_server = Socket} = State) ->
    erlang:send_after(100, self(), reconnect),
    {noreply, State#state{ fbi_server = undefined }};
handle_info(reconnect, #state{ fbi_server = undefined, realm = Realm } = State) ->
    {Host, Port} = fbi_ccfg:server_hostport(Realm),
    case gen_tcp:connect(Host, Port, [list, {active, true}, {packet, line}], 1000) of
        {ok, Socket} ->
            clear_cache(Realm),
            {noreply, State#state{ fbi_server = Socket }};
        {error, Reason} ->
            lager:error("FBI server ~p connect failed: ~p",
                [Host, Reason]),
            erlang:send_after(3000 + random:uniform(5000), self(), reconnect),
            {noreply, State}
    end;
handle_info(heartbeat, #state{fbi_server = undefined} = State) ->
    erlang:send_after(3000, self(), heartbeat),
    {noreply, State#state{awaiting_pong = false}};
handle_info(heartbeat, #state{fbi_server = Socket, awaiting_pong = true} = State) ->
    gen_tcp:close(Socket),
    erlang:send_after(100, self(), reconnect),
    erlang:send_after(3000, self(), heartbeat),
    {noreply, State#state{fbi_server = undefined, awaiting_pong = false}};
handle_info(heartbeat, #state{fbi_server = Socket, awaiting_pong = false} = State) ->
    erlang:send_after(3000, self(), heartbeat),
    case gen_tcp:send(Socket, "ping\n") of
        ok -> {noreply, State#state{awaiting_pong = true}};
        _ ->
            erlang:send_after(100, self(), reconnect),
            {noreply, State#state{fbi_server = undefined}}
    end;
handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

data_of_blob("pong\n") -> pong;
data_of_blob([Action | Rest]) ->
    case string:tokens(Rest, ";\n") of
        [Flag, Label | _] when Action == $+; Action == $- -> {ok, {
                                case Action of $+ -> flagged; $- -> unflagged end,
                                Flag,
                                binary_to_term(base64:decode(Label))}};
        _ -> error
    end.

rise_flagged_event(Realm, Flag, Label) ->
    rise_fbi_event({flagged, Realm, Flag, Label}).

rise_unflagged_event(Realm, Flag, Label) ->
    rise_fbi_event({unflagged, Realm, Flag, Label}).

rise_fbi_event(Event) ->
    gen_event:notify(fbi_client_events, Event).

