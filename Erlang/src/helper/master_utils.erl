-module(master_utils).
-export([distribute_model/4, distribute_model_weights/4, load_db/3, train/4, load_nodes/3, check_node_alive/2, reconnect/2, reconnect/5]).
-include("state.hrl").


distribute_model(PythonModelPid, Pids, JavaUiPid, async) ->
    Model = message_primitives:synch_message(PythonModelPid, get_model, null, model_definition, JavaUiPid),
    lists:foreach(fun(Pid) -> node_api:initialize_model(Pid, Model) end, Pids),
    ok;

distribute_model(PythonModelPid, Pids, JavaUiPid, sync) ->
    distribute_model(PythonModelPid, Pids, JavaUiPid, asyc),
    message_primitives:wait_response(length(Pids), initialize_ack, JavaUiPid).


distribute_model_weights(PythonModelPid, Pids, JavaUiPid, async) ->
    Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights, JavaUiPid),
    lists:foreach(fun(Pid) -> node_api:update_weights(Pid, Weights) end, Pids),
    ok;

distribute_model_weights(PythonModelPid, Pids, JavaUiPid, sync) ->
    distribute_model_weights(PythonModelPid, Pids, JavaUiPid, async),
    NewPids = message_primitives:wait_response(length(Pids), weights_ack, JavaUiPid, Pids),
    lists:map(fun(Pid) -> {Pid, erlang:node(Pid)} end, NewPids).


load_db(Pids, _JavaUiPid, async) ->
    lists:foreach(fun(Pid) -> node_api:load_db(Pid) end, Pids),
    ok;

load_db(Pids, JavaUiPid, sync) ->
    load_db(Pids, JavaUiPid, async),
    ResponseList = message_primitives:wait_response(length(Pids), db_ack, JavaUiPid),
    lists:unzip(ResponseList).



train(CurrentEpoch, PythonModelPid, Nodes, JavaUiPid) ->
    Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights, JavaUiPid),

    lists:foreach(fun(Pid) -> node_api:train_pipeline(Pid, Weights, CurrentEpoch) end, Nodes),
    ResponseList = message_primitives:wait_response(length(Nodes), {train_pipeline_ack, CurrentEpoch}, JavaUiPid, Nodes),

    case ResponseList of
        [] -> {[], 0.0};
        _ ->
            {PidList, Messages} = lists:unzip(ResponseList),
            {NewWeights, Accuracy} = lists:unzip(Messages),
            {TrainAccuracy, TestAccuracy} = lists:unzip(Accuracy),
            message_primitives:synch_message(PythonModelPid, update_weights, NewWeights, update_weights_ack, JavaUiPid),
            io:format("--- MASTER: train completed for epochs: ~p, resulting nodes train accuracy: ~p, resulting nodes test accuracy: ~p,  ---~n", [CurrentEpoch, TrainAccuracy, TestAccuracy]),

            message_primitives:notify_ui(JavaUiPid, {train_epoch_completed, PidList, TrainAccuracy, TestAccuracy}),
            TrainMeanAccuracy = lists:sum(TrainAccuracy) / length(TrainAccuracy),
            {PidList, TrainMeanAccuracy}
    end.


load_nodes(ListsPidNodes, PythonModelPid, JavaUiPid) ->
    {Pids, _Nodes} = lists:unzip(ListsPidNodes),
    load_db(Pids, JavaUiPid, async),  % the acks sent by the nodes will be discarded in the master server loop and the next iteration
    distribute_model(PythonModelPid, Pids, JavaUiPid, async),
    distribute_model_weights(PythonModelPid, Pids, JavaUiPid, sync).


check_node_alive(Pid, Node) ->
  case rpc:call(Node, erlang, is_process_alive, [Pid]) of
    true ->
      case node_api:check_status(Pid) of   % the node could be in its terminating phase so the pid is still alive but the server is unavailable
        true -> true;
        _ -> false
      end;
    _ -> false
  end.


reconnect(Node, State) ->
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

    case master_utils:load_nodes([{PidNew, Node}], State#mstate.pythonModelPID, State#mstate.javaUiPid) of
    [LoadedPidNodeNew] ->
        NewPidNodes = lists:keydelete(Node, 2, State#mstate.previousInitializedNodes), % remove to avoid two processes for the same node
        PidNodes1 = [LoadedPidNodeNew | State#mstate.currentUpNodes],
        PidNodes2 = [LoadedPidNodeNew | NewPidNodes],
        message_primitives:notify_ui(State#mstate.javaUiPid, {node_up, Node}),
    State#mstate{currentUpNodes = PidNodes1, previousInitializedNodes = PidNodes2};
    [] ->
        % List is empty, no PID was loaded -> meaning the node has an error on the initialization routine
        io:format("--- MASTER: Node ~p cannot be initialized ---~n", [Node]),
        message_primitives:flush_msg({nodeup, Node}),  % avoid to retry the initialization if the error is in the startup routine
        State
    end.


reconnect(Node, State, Pid, Reason, _MonitorRef) ->
    NewState = master_utils:reconnect(Node, State),
    message_primitives:flush_msg('DOWN', Pid, Reason),
    NewState.