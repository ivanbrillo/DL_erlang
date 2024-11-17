-module(master).
-behaviour(gen_server).

%% API
-export([start_link/0, get_nodes/0, load_db/0, initialize_nodes/0,
         distribute_model/0, distribute_weights/0, train/0, train/1, load_nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("helper/state.hrl").

%% API functions
start_link() ->
    Response = gen_server:start_link({local, erlang_master}, ?MODULE, [], []),
    initialize_nodes(),
    load_nodes(),
    Response.

load_nodes() ->
    gen_server:call(erlang_master, load_nodes, 10000).

get_nodes() ->
    gen_server:call(erlang_master, get_nodes).

load_db() ->
    gen_server:call(erlang_master, load_db).

initialize_nodes() ->
    gen_server:call(erlang_master, initialize_nodes, 20000).

distribute_model() ->
    gen_server:call(erlang_master, distribute_model).

distribute_weights() ->
    gen_server:call(erlang_master, distribute_weights).

train() ->
    gen_server:cast(erlang_master, {train, 1, 0}).

train(NEpochs) ->
    gen_server:cast(erlang_master, {train, NEpochs, 0}).

%% gen_server callbacks
init([]) ->
    io:format("--- MASTER: Starting erlang process ---~n"),
    PythonModel = python_helper:init_python_process(),
    PythonUI = python_helper:init_python_process(),
    State = #state{pythonModelPID = PythonModel, pythonUiPID = PythonUI},
    
    python_helper:python_register_handler(PythonModel, master, self()),
    python_helper:python_register_handler(PythonUI, ui, self()),
    timer:send_after(10000, self(), check_nodes),
    {ok, State}.

handle_call(get_nodes, _From, State) ->
    Nodes = network_helper:get_cluster_nodes(),
    {reply, Nodes, State};

handle_call(load_nodes, _From, State) ->
    Pids = master_utils:load_nodes(State#state.currentUpNodes, State#state.pythonModelPID),
    message_primitives:notify_ui(State#state.pythonUiPID, {loaded_nodes, Pids}),
    {reply, ok, State};

handle_call(load_db, _From, State) ->
    {PidNodes, _} = lists:unzip(State#state.currentUpNodes),
    {PidList, Infos} = master_utils:load_db(PidNodes, synch),
    message_primitives:notify_ui(State#state.pythonUiPID, {db_loaded, PidList}),
    {reply, {ok, Infos}, State};

handle_call(initialize_nodes, _From, State) ->
    InitializedNodes = network_helper:initialize_nodes(),
    net_kernel:monitor_nodes(true),
    message_primitives:notify_ui(State#state.pythonUiPID, {initialized_nodes, InitializedNodes}),
    {reply, {ok, InitializedNodes}, State#state{initialUpNodes = InitializedNodes, currentUpNodes = InitializedNodes}};

handle_call(distribute_model, _From, State) ->
    {PidNodes, _} = lists:unzip(State#state.currentUpNodes),
    ResponseList = master_utils:distribute_model(State#state.pythonModelPID, PidNodes, synch),
    message_primitives:notify_ui(State#state.pythonUiPID, {distributed_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State};

handle_call(distribute_weights, _From, State) ->
    {PidNodes, _} = lists:unzip(State#state.currentUpNodes),
    ResponseList = master_utils:distribute_model_weights(State#state.pythonModelPID, PidNodes, synch),
    message_primitives:notify_ui(State#state.pythonUiPID, {weights_updated_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State}.

handle_cast({train, EpochsLeft, CurrentEpoch}, State) when EpochsLeft > 0, CurrentEpoch >= 0 ->
    {PidNodes, _} = lists:unzip(State#state.currentUpNodes),
    Nodes = master_utils:train(CurrentEpoch, State#state.pythonModelPID, PidNodes),
    message_primitives:notify_ui(State#state.pythonUiPID, {train_completed, Nodes}),

    case EpochsLeft > 1 of
        true -> gen_server:cast(erlang_master, {train, EpochsLeft - 1, CurrentEpoch + 1});
        false -> ok
    end,

    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    io:format("--- MASTER: Node ~p connected, initializing ---~n", [Node]),
    [{PidNew, Node}] = network_helper:initialize_nodes([Node]),
    [PidNew] = master_utils:load_nodes([{PidNew, Node}], State#state.pythonModelPID),
    PidNodes = [{PidNew, Node} | State#state.currentUpNodes],
    {noreply, State#state{currentUpNodes = PidNodes}};


handle_info({nodedown, Node}, State) ->
    io:format("--- MASTER: Node ~p disconnected ---~n", [Node]),
    UpdatedUpNodes = lists:keydelete(Node, 2, State#state.currentUpNodes),
    {noreply, State#state{currentUpNodes = UpdatedUpNodes}};

handle_info({python_unhandled, Cause}, State) ->   % TODO: to be removed
    io:format("--- MASTER: Python received unhandled message: ~p ---~n", [Cause]),
    {noreply, State};

handle_info(check_nodes, State) ->   % TODO: to be removed
    _Nodes = network_helper:get_cluster_nodes(),
    timer:send_after(10000, self(), check_nodes),
    {noreply, State};


handle_info(Info, State) ->
    io:format("--- MASTER: received unhandled message: ~p ---~n", [Info]),
    {noreply, State}.

% called from supervisor shutdown
terminate(_Reason, State) ->
    io:format("--- MASTER: Terminating Procedure ---~n"),
    lists:foreach(fun({Pid, _Node}) -> node:stop(Pid) end, State#state.currentUpNodes),
    python:stop(State#state.pythonModelPID),      % TODO: possible shutdown procedure (eg. save python model) 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.