%%% vim: set ts=4 sts=4 sw=4 expandtab:

-module(fbi_client_handler_watcher).
-behaviour(gen_server).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% API
-export([
    start_link/2
]).

-record(state, {
    handler, 
    args
}).

-define(REINSTALL_TIMEOUT, 1000).
-define(HANDLER_MSG, 'gen_event_EXIT').

%% =============================
%% API
%% =============================

start_link(Handler, Args) ->
    gen_server:start_link(?MODULE, [Handler, Args], []).

%% =============================
%% Behaviour callbacks
%% =============================

init([Handler, Args]) ->
    install_handler(Handler, Args),
    {ok, #state{handler=Handler, args=Args}}.
    
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({?HANDLER_MSG, Handler, normal}, S=#state{handler=Handler}) ->
    {stop, normal, S};
handle_info({?HANDLER_MSG, Handler, shutdown}, S=#state{handler=Handler}) ->
    {stop, normal, S};
handle_info({?HANDLER_MSG, Handler, Reason}, S=#state{handler=Handler, args=Args}) ->
    lager:error("FBI event handler '~p' failed with reason: '~p'", [Handler, Reason]),
    install_handler(Handler, Args),
    {noreply, S};
handle_info(install_handler, S=#state{handler=Handler, args=Args}) ->
    install_handler(Handler, Args),
    {noreply, S};
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================
%% Internal functions
%% =============================

install_handler(Handler, Args) ->
    ReinstallByTimeout = fun() ->
        erlang:send_after(?REINSTALL_TIMEOUT, self(), install_handler)
    end,
        
    try fbi_client:add_event_handler(Handler, Args) of
        ok ->
            ok;
        {_Error, Reason} ->
            lager:error("FBI client install events handler '~p' error: ~p~n", [Handler, Reason]),
            ReinstallByTimeout(),
            ok
    catch 'exit':noproc ->
        ReinstallByTimeout(),
        ok
    end.

