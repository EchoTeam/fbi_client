%%% vim: set ts=4 sts=4 sw=4 et:

-module(fbi).

-export([
    ev_action/0,
    ev_actions/1,
    ev_activity/1,
    ev_value/1,
    flag/2,     % Return true if flagged with a given tag, false otherwise.
    flagmap/1,  % Return the current state of the flags.
    key_of/1,
    multi_submit/3,
    submit/3,   % Submit stat
    sdb_query/2,
    sdb_query/3
]).

ev_action() -> ev_actions(1).
ev_actions(N) -> {activity, N, fun() -> ok end}.
ev_activity(F) -> {activity, 1, F}.
ev_value(V) -> {value, V}.

% For the given keychain [A, B, C] submits data for A/B/C key only
submit(Realm, KeyChain, Event) ->
    Key = key_of(KeyChain),
    multi_submit(Realm, [Key], Event).

multi_submit(Realm, Keys, {activity, N, F}) ->
    Start = now(),
    try F() of
        V ->
            Stop = now(),
            ElapsedMS = timer:now_diff(Stop, Start) div 1000,
            fbi_client_stats:submit(Realm, Keys, {activity, hit, N, ElapsedMS}),
            V
    catch Class:Reason ->
        Stop = now(),
        ElapsedMS = timer:now_diff(Stop, Start) div 1000,
        Stacktrace = erlang:get_stacktrace(),
        fbi_client_stats:submit(Realm, Keys, {activity, miss, N, ElapsedMS}),
        erlang:raise(Class, Reason, Stacktrace)
    end;
multi_submit(Realm, Keys, {value, V}) ->
    fbi_client_stats:submit(Realm, Keys, {value, V}),
    V.

% @spec flag(FlagKeyChain) -> boolean()
flag(Realm, FlagKeyChain) ->
    fbi_client:flag(Realm, binary_to_list(key_of(FlagKeyChain))).

flagmap(Realm) ->
    Tab = fbi_ccfg:client_tab(Realm),
    [printable_binary_key(Flag) || {Flag, _} <- fbi_sdb_cache:get_flags_map(Tab)].

printable_binary_key(Key) ->
    [case S of "<" ++ StartBin -> list_to_binary(to_bin(StartBin)); _ -> S end
        || S <- string:tokens(Key, "/")].
to_bin(">") -> [];
to_bin([H,L|Rest]) ->
    C = (echo_json:digit_hex(H) bsl 4) + echo_json:digit_hex(L),
    [C | to_bin(Rest)].

%
% fbi:key_of([wapo, "dev.espn", <<12,34>>]) -> <<"wapo/dev.espn/<0c22>">>
%
key_of(List) -> iolist_to_binary(key_of_list(List)).

key_of_list([H|[]]) -> [key_ll(H)];
key_of_list([H|T]) -> [key_ll(H), "/" |key_of_list(T)];
key_of_list([]) -> [].

key_ll(H) when is_list(H) -> key_sanitize(H);
key_ll(H) when is_atom(H) -> key_sanitize(atom_to_list(H));
key_ll(H) when is_binary(H) -> "<"++hex(H,[])++">".

key_sanitize([C|T]) when C < $  orelse C > 127 -> key_sanitize(T);
key_sanitize([C|T]) when C == $< orelse C == $> -> key_sanitize(T);
key_sanitize([C|T]) when C == $/ -> key_sanitize(T);
key_sanitize([C|T]) -> [C|key_sanitize(T)];
key_sanitize([]) -> [].

hex(<<A:4/integer,B:4/integer,Rest/binary>>, Accum) -> hex(Rest, [dig(B), dig(A) | Accum]);
hex(<<>>, Accum) -> lists:reverse(Accum).

dig(N) when N < 10 -> $0 + N;
dig(N) -> $W + N.


% Use carefully! Can be CPU-intensive
sdb_query(Realm, Q) ->
    sdb_query(Realm, Q, 5000).

% Use carefully! Can be CPU-intensive
sdb_query(Realm, Q, Timeout) ->
    Node = fbi_ccfg:server_node(Realm),
    erpc:call(Node, fbi_sdb, sdb_query, [Realm, Q, Timeout], Timeout).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

key_of_test() ->
    ?assertEqual(<<"wapo/dev.espn/<0c22>">>, key_of([wapo, "dev.espn", <<12,34>>])),
    ?assertEqual(<<"wapo">>, key_of(["<w\1apo>/"])),
    ok.

-endif.
