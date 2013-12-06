-module(fbi_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    load_config(),
    fbi_client_sup:start_link().

stop(_State) ->
    ok.

load_config() ->
    {ok, CommonSpec} = application:get_env(fbi_client, common),
    {ok, SpecialSpec} = application:get_env(fbi_client, special),

    Realms = [R || {R, _} <- CommonSpec],
    RealmsS = [R || {R, _} <- SpecialSpec],
    true = lists:sort(Realms) == lists:sort(RealmsS),
    RealmsSpec = [{R, lists:concat([proplists:get_value(R, S) || S <- [CommonSpec, SpecialSpec]])} || R <- Realms],

    Get = fun(Realm, Key) ->
        RS = proplists:get_value(Realm, RealmsSpec, []),
        proplists:get_value(Key, RS)
    end,
    W = fun(L) ->
        lists:zip(L, lists:duplicate(length(L) - 1, ";") ++ ["."])
    end,
    IO = fun(What) ->
        io_lib:format("~p", [What])
    end,
    Mod = fbi_ccfg,
    ModSpec = [
            ["-module(", atom_to_list(Mod), ")."],
            ["-export([client_proc/1, client_tab/1, client_stats_proc/1, client_stats_tab/1, server_node/1, server_hostport/1, enumerate/0])."],
            ["enumerate() -> ", IO(Realms), "."],
            [["client_proc(", IO(R), ") -> ", IO(Get(R, client_proc)), D] || {R, D} <- W(Realms)],
            [["client_tab(", IO(R), ") -> ", IO(Get(R, client_tab)), D] || {R, D} <- W(Realms)],
            [["client_stats_proc(", IO(R), ") -> ", IO(Get(R, client_stats_proc)), D] || {R, D} <- W(Realms)],
            [["client_stats_tab(", IO(R), ") -> ", IO(Get(R, client_stats_tab)), D] || {R, D} <- W(Realms)],
            [["server_node(", IO(R), ") -> ", IO(Get(R, server_node)), D] || {R, D} <- W(Realms)],
            [["server_hostport(", IO(R), ") -> {", IO(Get(R, server_host)), ",", IO(Get(R, server_port)), "}", D] || {R, D} <- W(Realms)]
    ],
    mod_gen:go(ModSpec).
