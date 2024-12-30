-module(master).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("../helper/state.hrl").



init([JavaPid]) ->
    io:format("--- MASTER: Starting erlang process, Java pid: ~p ---~n", [JavaPid]),

    PythonModel = python_helper:init_python_process(),
    State = #mstate{pythonModelPID = PythonModel, javaUiPid = JavaPid},   % TODO change names!
    
    python_helper:python_register_handler(PythonModel, master, self()),
    timer:send_after(10000, self(), check_nodes),  % check the connected nodes in 10s, useful for handling reconnection and new nodes
    {ok, State}.


handle_call(get_nodes, _From, State) ->
    Nodes = network_helper:get_cluster_nodes(),
    io:format("--- MASTER: nodes ~p ---~n", [Nodes]),   % TODO remove
    {reply, Nodes, State};

handle_call(get_pid, _From, State) ->
    {reply, self(), State};

handle_call(load_nodes, _From, State) ->
    Pids = master_utils:load_nodes(State#mstate.currentUpNodes, State#mstate.pythonModelPID, State#mstate.javaUiPid),
    message_primitives:notify_ui(State#mstate.javaUiPid, {loaded_nodes, Pids}),
    io:format("--- MASTER: node loaded ---~n"),   % TODO remove

    {reply, ok, State};

handle_call(load_db, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    {PidList, Infos} = master_utils:load_db(PidNodes, State#mstate.javaUiPid, sync),
    message_primitives:notify_ui(State#mstate.javaUiPid, {db_loaded, PidList}),
    {reply, {ok, Infos}, State};

handle_call(initialize_nodes, _From, State) ->
    InitializedNodes = network_helper:initialize_nodes(State#mstate.javaUiPid),
    net_kernel:monitor_nodes(true),  % send messages nodeup/nodedown when a node connects/disconnects
    message_primitives:notify_ui(State#mstate.javaUiPid, {initialized_nodes, InitializedNodes}),
    io:format("--- MASTER: node initialized ~p ---~n", [InitializedNodes]),   % TODO remove

    {reply, {ok, InitializedNodes}, State#mstate{currentUpNodes = InitializedNodes, previousInitializedNodes = InitializedNodes}};

handle_call(distribute_model, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    ResponseList = master_utils:distribute_model(State#mstate.pythonModelPID, PidNodes, State#mstate.javaUiPid, sync),
    message_primitives:notify_ui(State#mstate.javaUiPid, {distributed_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State};

handle_call(distribute_weights, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    ResponseList = master_utils:distribute_model_weights(State#mstate.pythonModelPID, PidNodes, State#mstate.javaUiPid, sync),
    message_primitives:notify_ui(State#mstate.javaUiPid, {weights_updated_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State}.

handle_cast({new_train, EpochsLeft, CurrentEpoch, AccuracyThreshold}, State) ->
    gen_server:cast(erlang_master, {train, EpochsLeft, CurrentEpoch, AccuracyThreshold}),
    {noreply, State#mstate{terminateTraining = false}};   % clear the terminateTraining flag of a possible previous training

handle_cast({train, EpochsLeft, CurrentEpoch, AccuracyThreshold}, State) when EpochsLeft > 0, CurrentEpoch >= 0, length(State#mstate.currentUpNodes) > 0 ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    {Nodes, TrainMeanAccuracy} = master_utils:train(CurrentEpoch, State#mstate.pythonModelPID, PidNodes, State#mstate.javaUiPid),

    case {EpochsLeft > 1, AccuracyThreshold >= TrainMeanAccuracy, length(Nodes) > 0, State#mstate.terminateTraining } of
        {true, true, true, false} ->
           if 
            CurrentEpoch rem 3 == 2 ->
                    io:format("--- AAAAA: model saved ---~n"), 
                gen_server:cast(erlang_master, {save_model, backup}); % Call save_model every 3 epochs
            true -> ok
            end,
            gen_server:cast(erlang_master, {train, EpochsLeft - 1, CurrentEpoch + 1, AccuracyThreshold});
        {_, _, true, _} ->
            message_primitives:notify_ui(State#mstate.javaUiPid, {training_total_completed, TrainMeanAccuracy});
        {_, _, false, _} ->
            message_primitives:notify_ui(State#mstate.javaUiPid, {training_error})
    end,

    {noreply, State};

handle_cast({train, _EpochsLeft, _CurrentEpoch, _AccuracyThreshold}, State) ->
    message_primitives:notify_ui(State#mstate.javaUiPid, {train_refused}),
    io:format("--- MASTER: training refused, possible causes: no nodes connected or illegal param ---~n"),
    {noreply, State};

handle_cast({save_model, Name}, State) ->
    _Ack = message_primitives:synch_message(State#mstate.pythonModelPID, save_model, Name, model_saved, State#mstate.javaUiPid),

    message_primitives:notify_ui(State#mstate.javaUiPid, {model_saved}),
    io:format("--- MASTER: model saved ---~n"),
    {noreply, State};

handle_cast(load_model, State) ->
    Result = message_primitives:synch_message(State#mstate.pythonModelPID, load_model, null, model_loaded, State#mstate.javaUiPid),

    message_primitives:notify_ui(State#mstate.javaUiPid, {model_loaded, Result}),
    io:format("--- MASTER: model loaded ~p ---~n", [Result]),
    {noreply, State};

handle_cast(stop_training, State) ->
    io:format("--- MASTER: train set to stop on next epoch or two ---~n"),
    {noreply, State#mstate{terminateTraining = true}}.

% acks not read in the load_nodes pipeline, useful for UI dashboard
handle_info({db_ack, InfosDB}, State) ->
    message_primitives:notify_ui(State#mstate.javaUiPid, {db_ack, InfosDB}),
    {noreply, State};

handle_info({nodeup, Node}, State) ->
    io:format("--- MASTER: Node ~p connected ---~n", [Node]),

    % Determine the new PID for the node
    PidNew = case lists:keyfind(Node, 2, State#mstate.previousInitializedNodes) of
        {Pid, _Node} ->
            case master_utils:check_node_alive(Pid, Node) of
                true ->
                        io:format("--- MASTER: Node ~p server is still alive ---~n", [Node]),
                        erlang:monitor(process, Pid),   % the monitor is deactivated when a node is disconnected
                        Pid;
                _ ->    io:format("--- MASTER: Node ~p server is dead, initializing ---~n", [Node]),
                        [{PidN, Node}] = network_helper:initialize_nodes([Node], State#mstate.javaUiPid),
                         PidN
            end;
        false -> io:format("--- MASTER: Node ~p is newly connected, initializing ---~n", [Node]), 
                [{PidN, Node}] = network_helper:initialize_nodes([Node], State#mstate.javaUiPid),
                PidN
    end,

    NewPidNodes = lists:keydelete(Node, 2, State#mstate.previousInitializedNodes), % remove to avoid two processes for the same node
    [LoadedPidNew] = master_utils:load_nodes([{PidNew, Node}], State#mstate.pythonModelPID, State#mstate.javaUiPid),
    PidNodes1 = [{LoadedPidNew, Node} | State#mstate.currentUpNodes], % Add the new node to the connected node list
    PidNodes2 = [{LoadedPidNew, Node} | NewPidNodes], % Add the new node to the previous connected node list
    message_primitives:notify_ui(State#mstate.javaUiPid, {node_up, Node}),
    {noreply, State#mstate{currentUpNodes = PidNodes1, previousInitializedNodes = PidNodes2}};

handle_info({nodedown, Node}, State) ->
    io:format("--- MASTER: Node ~p disconnected ---~n", [Node]),
    UpdatedUpNodes = lists:keydelete(Node, 2, State#mstate.currentUpNodes),   % remove from the connected node lists the disconnected node
    message_primitives:notify_ui(State#mstate.javaUiPid, {node_down, Node}),
    {noreply, State#mstate{currentUpNodes = UpdatedUpNodes}};

handle_info({python_unhandled, Cause}, State) ->   % TODO: to be removed
    io:format("--- MASTER: Python received unhandled message: ~p ---~n", [Cause]),
    {noreply, State};

handle_info(check_nodes, State) ->
    _Nodes = network_helper:get_cluster_nodes(),
    timer:send_after(10000, self(), check_nodes),  % periodic node check each 10s
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State) when Reason =/= noconnection ->   % noconnection errors are directly handled by nodedown
    case lists:keyfind(Pid, 1, State#mstate.currentUpNodes) of
        {Pid, Node} ->
            io:format("--- NODE ~p terminated with reason: ~p ---~n", [Node, Reason]),
            erlang:disconnect_node(Node);   % Leave the restart for a later time to avoid continuous tries. It will be handled by nodedown in at most 10s
        false ->
            io:format("--- NODE with PID ~p not found in currentUpNodes, terminated with reason: ~p ---~n", [Pid, Reason])
    end,
    UpdatedUpNodes = lists:keydelete(Pid, 1, State#mstate.currentUpNodes),
    {noreply, State#mstate{currentUpNodes = UpdatedUpNodes}};

handle_info({node_metrics, Metrics}, State) ->
    State#mstate.javaUiPid ! {node_metrics, Metrics},
    {noreply, State};

handle_info(Info, State) ->
    io:format("--- MASTER: received unhandled message: ~p ---~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->    % called from supervisor shutdown or stop function
    io:format("--- MASTER: Terminating Procedure ---~n"),
    lists:foreach(fun({Pid, _Node}) -> gen_server:stop(Pid) end, State#mstate.currentUpNodes),  % send stop signal to all connected nodes
    python:stop(State#mstate.pythonModelPID),      % TODO: possible shutdown procedure (eg. save python model) 
    ok.