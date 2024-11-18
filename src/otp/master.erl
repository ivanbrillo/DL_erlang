-module(master).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("../helper/state.hrl").



init([]) ->
    io:format("--- MASTER: Starting erlang process ---~n"),
    PythonModel = python_helper:init_python_process(),
    PythonUI = python_helper:init_python_process(),
    State = #mstate{pythonModelPID = PythonModel, pythonUiPID = PythonUI},
    
    python_helper:python_register_handler(PythonModel, master, self()),
    python_helper:python_register_handler(PythonUI, ui, self()),
    timer:send_after(10000, self(), check_nodes),
    {ok, State}.


handle_call(get_nodes, _From, State) ->
    Nodes = network_helper:get_cluster_nodes(),
    {reply, Nodes, State};

handle_call(load_nodes, _From, State) ->
    Pids = master_utils:load_nodes(State#mstate.currentUpNodes, State#mstate.pythonModelPID),
    message_primitives:notify_ui(State#mstate.pythonUiPID, {loaded_nodes, Pids}),
    {reply, ok, State};

handle_call(load_db, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    {PidList, Infos} = master_utils:load_db(PidNodes, synch),
    message_primitives:notify_ui(State#mstate.pythonUiPID, {db_loaded, PidList}),
    {reply, {ok, Infos}, State};

handle_call(initialize_nodes, _From, State) ->
    InitializedNodes = network_helper:initialize_nodes(),
    net_kernel:monitor_nodes(true),
    message_primitives:notify_ui(State#mstate.pythonUiPID, {initialized_nodes, InitializedNodes}),
    {reply, {ok, InitializedNodes}, State#mstate{currentUpNodes = InitializedNodes}};

handle_call(distribute_model, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    ResponseList = master_utils:distribute_model(State#mstate.pythonModelPID, PidNodes, synch),
    message_primitives:notify_ui(State#mstate.pythonUiPID, {distributed_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State};

handle_call(distribute_weights, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    ResponseList = master_utils:distribute_model_weights(State#mstate.pythonModelPID, PidNodes, synch),
    message_primitives:notify_ui(State#mstate.pythonUiPID, {weights_updated_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State}.


handle_cast({train, EpochsLeft, CurrentEpoch}, State) when EpochsLeft > 0, CurrentEpoch >= 0 ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    Nodes = master_utils:train(CurrentEpoch, State#mstate.pythonModelPID, PidNodes),
    message_primitives:notify_ui(State#mstate.pythonUiPID, {train_completed, Nodes}),

    case EpochsLeft > 1 of
        true -> gen_server:cast(erlang_master, {train, EpochsLeft - 1, CurrentEpoch + 1});
        false -> ok
    end,
    {noreply, State}.


handle_info({nodeup, Node}, State) ->
    io:format("--- MASTER: Node ~p connected, initializing ---~n", [Node]),
    [{PidNew, Node}] = network_helper:initialize_nodes([Node]),
    [PidNew] = master_utils:load_nodes([{PidNew, Node}], State#mstate.pythonModelPID),
    PidNodes = [{PidNew, Node} | State#mstate.currentUpNodes],
    {noreply, State#mstate{currentUpNodes = PidNodes}};

handle_info({nodedown, Node}, State) ->
    io:format("--- MASTER: Node ~p disconnected ---~n", [Node]),
    UpdatedUpNodes = lists:keydelete(Node, 2, State#mstate.currentUpNodes),
    {noreply, State#mstate{currentUpNodes = UpdatedUpNodes}};

handle_info({python_unhandled, Cause}, State) ->   % TODO: to be removed
    io:format("--- MASTER: Python received unhandled message: ~p ---~n", [Cause]),
    {noreply, State};

handle_info(check_nodes, State) ->   % TODO: to be removed
    _Nodes = network_helper:get_cluster_nodes(),
    timer:send_after(10000, self(), check_nodes),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State) ->
    case lists:keyfind(Pid, 1, State#mstate.currentUpNodes) of
        {Pid, Node} ->
            io:format("--- NODE ~p terminated with reason: ~p ---~n", [Node, Reason]),
            erlang:disconnect_node(Node);   % Leave the restart for a later time to avoid continuous restarts
        false ->
            io:format("--- NODE with PID ~p not found in currentUpNodes, terminated with reason: ~p ---~n", [Pid, Reason])
    end,
    UpdatedUpNodes = lists:keydelete(Pid, 1, State#mstate.currentUpNodes),
    {noreply, State#mstate{currentUpNodes = UpdatedUpNodes}};

handle_info(Info, State) ->
    io:format("--- MASTER: received unhandled message: ~p ---~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->    % called from supervisor shutdown or stop function
    io:format("--- MASTER: Terminating Procedure ---~n"),
    lists:foreach(fun({Pid, _Node}) -> node_api:stop(Pid) end, State#mstate.currentUpNodes),
    python:stop(State#mstate.pythonModelPID),      % TODO: possible shutdown procedure (eg. save python model) 
    ok.