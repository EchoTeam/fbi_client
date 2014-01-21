-module(fbi_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, all_spec() } }.

all_spec() ->
    specs_for_realms() ++ static_specs().
    
specs_for_realms() ->
    lists:flatten([specs_for_realm(Realm) || Realm <- fbi_ccfg:enumerate()]).
    
specs_for_realm(Realm) -> [
    { fbi_utils:atom_for_realm(fbi_client, Realm), { fbi_client, start_link, [Realm] },
        permanent, 10000, worker, [fbi_client]},
    { fbi_utils:atom_for_realm(fbi_client_stats, Realm), { fbi_client_stats, start_link, [Realm] },
        permanent, 10000, worker, [fbi_client_stats]}
].
    
static_specs() -> [
    {fbi_client_events, {gen_event, start_link, [{local, fbi_client_events}]},
        permanent, 10000, worker, dynamic}
].

