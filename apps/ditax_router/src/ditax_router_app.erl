%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2017 15:09
%%%-------------------------------------------------------------------
-module(ditax_router_app).
-author("pravosudov").

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
            StartArgs :: term()) ->
               {ok, pid()} |
               {ok, pid(), State :: term()} |
               {error, Reason :: term()}).
start(_StartType, _StartArgs) ->

    {ok, {source, Port}} = application:get_env(ditax_router, source),

    {ok, Pid} = ditax_router_sup:start_link(),

    {ok, _} = ranch:start_listener(ditax_router,
        ranch_tcp, [{port, Port}, {num_acceptors, 32}, {max_connections, infinity}],
        ditax_router_controller, []
    ),

    {ok, Pid}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
