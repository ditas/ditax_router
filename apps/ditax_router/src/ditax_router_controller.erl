%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2017 15:30
%%%-------------------------------------------------------------------
-module(ditax_router_controller).
-behaviour(gen_server).
-behaviour(ranch_protocol).
-author("pravosudov").
-vsn("0.1.3").

-include_lib("../include/common.hrl").

%% API
-export([start_link/4, send/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    remote,
    master,
    additionals = []
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @en
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
    %% Не используем {global/local, ?SERVER}, т.к. этот процесс НЕ ДОЛЖЕН быть зарегистрирован по имени. Его
    %% запускает супервизор ранча при установлении tcp соединения и этих процессов может быть МНОГО
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

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
init([Ref, Socket, Transport, Opts]) ->

    process_flag(trap_exit, true),

    self() ! {init, Ref, Socket, Transport, Opts},
    {ok, #state{remote = Socket}}.

send(Pid, Data) ->
    gen_server:cast(Pid, {send, self(), Data}).

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
handle_cast({send, From, Data}, State) when element(1, State#state.master) =:= From ->

    lager:debug("FROM MASTER ~p", [Data]),

    ok = gen_tcp:send(State#state.remote, Data),

    lists:foreach(fun({Pid, Handler}) ->
                          Handler:handle_out_data(Pid, Data)
                  end,
                  State#state.additionals),

    {noreply, State};
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
handle_info({init, Ref, Socket, _Transport, _Opts}, State) ->
    ok = ranch:accept_ack(Ref),
    ok = ranch_tcp:setopts(State#state.remote, [{active, true}, binary]),
    State0 = State#state{remote = Socket},

    lager:info("REMOTE CONNECTED ~p", [Socket]),

    {ok, Destinations} = application:get_env(ditax_router, destinations),
    State1 = lists:foldl(fun(D, S) ->
        Module = D#destination.module,

        %% Старт хендлера через супервизор опосредовано. Контроллер не знает супервизора модуля и поэтому вызывает
        %% функцию на самом модуле, которая стартует модуль, как процесс под супервизором, о котором сам модуль знает
        {ok, Pid} = Module:start(D#destination.name, D#destination.type, D#destination.connection_config, self()),
        %% Делаю явный link/1, чтобы связать процесс контроллера с процессом хендлера, иначе падение хендлера будет обрабатываться только его супервизором и не дойдет до контроллера
        link(Pid),

%%        %% Старт хендлера без супервизора
%%        {ok, Pid} = Module:start_link(D#destination.name, D#destination.type,
%%                                      D#destination.connection_config),

        case D#destination.type of
            main ->
                lager:debug("MAIN STARTED (~p) ~p", [Module, Pid]),
                S#state{master = {Pid, Module}};
            _ ->
                lager:debug("ADDITIONAL STARTED (~p) ~p", [Module, Pid]),
                Additionals = S#state.additionals,
                S#state{additionals = [{Pid, Module}|Additionals]}
        end
    end , State0, Destinations),
    {noreply, State1};
handle_info({tcp, Port, Data}, State) when Port =:= State#state.remote ->

    lager:debug("FROM REMOTE ~p", [Data]),

    {MasterPid, MasterHandler} = State#state.master,
    MasterHandler:handle_data(MasterPid, Data),

    Additionals = State#state.additionals,
    lists:foreach(fun({Pid, Handler}) ->
        Handler:handle_data(Pid, Data)
    end, Additionals),

    {noreply, State};
handle_info({tcp_closed, Port}, State) when Port =:= State#state.remote ->

    lager:info("REMOTE DISCONNECTED"),

    stop_all(State, device_disconnected);
handle_info({'EXIT', Pid, Reason}, State) when element(1, State#state.master) =:= Pid ->

    if
        Reason == normal ->
            lager:info("MASTER STOPPED ~p", [Pid]);
        true ->
            lager:error("MASTER STOPPED ~p ~p", [Pid, Reason])
    end,

    stop_all(State, master_disconnected);
handle_info({'EXIT', Pid, Reason}, State) ->

    Additionals = State#state.additionals,
    Additionals1 = case lists:keyfind(Pid, 1, Additionals) of
        {Pid, _Handler} ->

            lager:warning("ADDITIONAL STOPPED ~p ~p", [Pid, Reason]),

            lists:keydelete(Pid, 1, Additionals);
        false ->
            Additionals
    end,

    {noreply, State#state{additionals = Additionals1}};
handle_info(Info, State) ->

    lager:warning("UNKNOWN MESSAGE ~p", [Info]),

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
code_change(OldVsn, State, _Extra) ->
    lager:debug("UPGRADE from ~p", [OldVsn]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

stop_all(State, Reason) ->
    {MasterPid, MasterHandler} = State#state.master,
    MasterHandler:stop(MasterPid),

    Additionals = State#state.additionals,
    lists:foreach(fun({Pid, Handler}) ->
        Handler:stop(Pid)
    end, Additionals),

    lager:info("STOPPED ~p", [Reason]),

    {stop, normal, Reason}.
