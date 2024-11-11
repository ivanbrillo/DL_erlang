-module(master).
-behaviour(gen_server).

%% API
-export([start_link/0, get_nodes/0, load_db/0, initialize_nodes/0,
         distribute_model/0, distribute_weights/0, train/0, train/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("helper/state.hrl").

%% API functions
start_link() ->
    gen_server:start_link({local, erlang_master}, ?MODULE, [], []),
    gen_server:call(erlang_master, initialize_nodes, 20000),
    gen_server:call(erlang_master, load_db, 20000),
    gen_server:call(erlang_master, distribute_model, 20000),
    gen_server:call(erlang_master, distribute_weights, 20000).


get_nodes() ->
    gen_server:call(erlang_master, get_nodes).

load_db() ->
    gen_server:call(erlang_master, load_db).

initialize_nodes() ->
    gen_server:call(erlang_master, initialize_nodes).

distribute_model() ->
    gen_server:call(erlang_master, distribute_model).

distribute_weights() ->
    gen_server:call(erlang_master, distribute_weights).

train() ->
    gen_server:cast(erlang_master, {train, 1}).

train(NEpochs) ->
    gen_server:cast(erlang_master, {train, NEpochs}).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    PythonModel = python_helper:init_python_process(),
    PythonUI = python_helper:init_python_process(),
    State = #state{pythonModelPID = PythonModel, pythonUiPID = PythonUI},
    
    python_helper:python_register_handler(PythonModel, master, self()),
    python_helper:python_register_handler(PythonUI, ui, self()),
    
    {ok, State}.

handle_call(get_nodes, _From, State) ->
    Nodes = network_helper:get_cluster_nodes(),
    message_primitives:notify_ui(State#state.pythonUiPID, {nodes, Nodes}),
    {reply, Nodes, State};

handle_call(load_db, _From, State) ->
    {PidList, Infos} = master_utils:load_db(State#state.initializedNodes),
    message_primitives:notify_ui(State#state.pythonUiPID, {db_loaded, PidList}),
    {reply, {ok, Infos}, State#state{dbLoadedNodes = PidList}};

handle_call(initialize_nodes, _From, State) ->
    InitializedNodes = network_helper:initialize_nodes(),
    message_primitives:notify_ui(State#state.pythonUiPID, {initialized_nodes, InitializedNodes}),
    {reply, {ok, InitializedNodes}, State#state{initializedNodes = InitializedNodes}};

handle_call(distribute_model, _From, State) ->
    ResponseList = master_utils:distribute_model(State#state.pythonModelPID, State#state.initializedNodes),
    message_primitives:notify_ui(State#state.pythonUiPID, {distributed_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State#state{distributedNodes = ResponseList}};

handle_call(distribute_weights, _From, State) ->
    ResponseList = master_utils:distribute_model_weights(State#state.pythonModelPID, State#state.distributedNodes),
    message_primitives:notify_ui(State#state.pythonUiPID, {weights_updated_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State#state{weightsNodes = ResponseList}}.

handle_cast({train, NEpochs}, State) ->
    Nodes = master_utils:train(NEpochs, State#state.pythonModelPID, State#state.distributedNodes, State#state.pythonUiPID),
    {noreply, State#state{trainNodes = Nodes}}.

handle_info({python_unhandled, Cause}, State) ->
    io:format("Python received unhandled message: ~p~n", [Cause]),
    {noreply, State};


handle_info({'EXIT', Pid, {Ref, return, {ok, NodePid}}}, State) ->
    io:format("Node process ~p-~p returned EXIT successfully with reference ~p~n", [Pid, NodePid, Ref]),
    {noreply, State};


handle_info(_Info, State) ->
    io:format("Master received unhandled message: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.