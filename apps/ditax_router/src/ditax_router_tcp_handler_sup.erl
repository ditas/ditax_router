%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2017 13:22
%%%-------------------------------------------------------------------
-module(ditax_router_tcp_handler_sup).
-author("pravosudov").

-behaviour(supervisor).

%% API
-export([
    start_link/1,
    start_child/4
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link(SupName) ->
    supervisor:start_link({local, SupName}, ?MODULE, []).

start_child(SupName, Name, ConnectionConfig, ControllerPid) ->
    supervisor:start_child(SupName, [{Name, ConnectionConfig, ControllerPid}]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
          [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {tcp_handler, {ditax_router_tcp_handler, start_link, []},
              Restart, Shutdown, Type, [ditax_router_tcp_handler]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
