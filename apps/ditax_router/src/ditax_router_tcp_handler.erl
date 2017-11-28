%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2017 17:40
%%%-------------------------------------------------------------------
-module(ditax_router_tcp_handler).
-behaviour(gen_server).
-behaviour(gen_ditax_router_handler).
-author("pravosudov").
-vsn("0.1.2").

%% API
-export([
    supervisor_id/1
]).

-export([
    initialize/3,
    start/4,
    start_link/1,
    stop/1,
    handle_data/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {type,
                connection_config,
                socket,
                controller_pid
               }).

%%%===================================================================
%%% API
%%%===================================================================
supervisor_id(Name) ->
    erlang:list_to_atom(atom_to_list(Name) ++ "_sup").

%% Returns supervisor or module's own specs if necessary
initialize(Name,  _Type, _ConnectionConfig) ->
    SupName = supervisor_id(Name),
    [{Name, {ditax_router_tcp_handler_sup, start_link, [SupName]},
      permanent, 2000, supervisor, [ditax_router_tcp_handler_sup]}].

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%% Стартуем сам модуль, как процесс через его супервизор
start(Name, Type, ConnectionConfig, ControllerPid) ->
    SupName = supervisor_id(Name),
    ditax_router_tcp_handler_sup:start_child(SupName, Type, ConnectionConfig, ControllerPid).

%% Стартуем процесс по команде супервизора
start_link({Type, ConnectionConfig, ControllerPid}) ->
    gen_server:start_link(?MODULE, {Type, ConnectionConfig, ControllerPid}, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

handle_data(Pid, Data) ->
    From = self(),
    gen_server:cast(Pid, {handle_data, From, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Type, ConnectionConfig, Pid}) ->
    self() ! init,
    {ok, #state{type = Type, connection_config = ConnectionConfig, controller_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({handle_data, From, Data}, State) when State#state.controller_pid =:= From ->
    ok = gen_tcp:send(State#state.socket, Data),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(init, State) ->
    {ok, Socket} = gen_tcp:connect(proplists:get_value(ip, State#state.connection_config),
                                   proplists:get_value(port, State#state.connection_config),
                                   [binary, {active, true}]),
    {noreply, State#state{socket = Socket}};
handle_info({tcp, Socket, Data}, #state{socket = Socket, type = main} = State) ->
    ditax_router_controller:send(State#state.controller_pid, Data),
    {noreply, State};
handle_info({tcp_closed, Port}, State) when Port =:= State#state.socket ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
