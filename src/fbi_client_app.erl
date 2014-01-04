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
    {ok, InitialRealmsSpec} = application:get_env(fbi_client, realms),
    RealmsSpec = extend_realms_spec(InitialRealmsSpec),
    Realms = [R || {R, _} <- RealmsSpec],

    Get = fun(Realm, Key) ->
        RS = proplists:get_value(Realm, RealmsSpec, []),
        proplists:get_value(Key, RS)
    end,
    GetNode = fun(Realm) ->
        case Get(Realm, server_node) of
            local ->
                node();
            Value ->
                Value
        end
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
            [["server_node(", IO(R), ") -> ", IO(GetNode(R)), D] || {R, D} <- W(Realms)],
            [["server_hostport(", IO(R), ") -> {", IO(Get(R, server_host)), ",", IO(Get(R, server_port)), "}", D] || {R, D} <- W(Realms)]
    ],
    mod_gen:go(ModSpec).

extend_realms_spec(RealmsSpec) ->
    %% These options will be added to a realm spec (for each realms in the spec)
    %% These options will look in the spec as {option_name, prefix_<realm_short_name>}
    OptionsForExtend = [
        {client_proc, fbi_client_},
        {client_tab, fbi_client_flagmap_},
        {client_stats_proc, fbi_client_stats_},
        {client_stats_tab, fbi_client_stats_flagmap_}
    ],
    lists:map(fun(R) -> extend_realm_spec(R, OptionsForExtend) end, RealmsSpec).
          
extend_realm_spec({R, Spec}, OptionsForExtend) ->
    ShortName = proplists:get_value(realm_short_name, Spec),
    ConcatAtoms = fun(A1, A2) ->
        list_to_atom(atom_to_list(A1) ++ atom_to_list(A2))
    end,
    GetOption = fun({OptionName, Prefix}) ->
                     {OptionName, ConcatAtoms(Prefix, ShortName)}
    end,
    AdditionalOptions = lists:map(GetOption, OptionsForExtend),
    {R, Spec ++ AdditionalOptions}.


