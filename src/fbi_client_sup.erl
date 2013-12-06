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
    {ok, { {one_for_one, 5, 10}, lists:concat([
            children_for_realm(Realm) || Realm <- fbi_ccfg:enumerate()
        ])
    } }.

children_for_realm(Realm) -> [
        { fbi_utils:atom_for_realm(fbi_client, Realm), { fbi_client, start_link, [Realm] },
            permanent, 10000, worker, [fbi_client]},
        { fbi_utils:atom_for_realm(fbi_client_stats, Realm), { fbi_client_stats, start_link, [Realm] },
            permanent, 10000, worker, [fbi_client_stats]}
    ].
