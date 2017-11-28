%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2017 12:46
%%%-------------------------------------------------------------------
-module(ditax_router_sup).
-author("pravosudov").

-behaviour(supervisor).

-include_lib("../include/common.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, Destinations} = application:get_env(ditax_router, destinations),
    SupSpecs = lists:foldl(fun(D, Acc) ->
        Handler = D#destination.module,
        Specs = Handler:initialize(D#destination.name, D#destination.type,
                                   D#destination.connection_config),
        Acc ++ Specs
    end, [], Destinations),

    {ok, {SupFlags, SupSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
