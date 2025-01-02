-module(master_utils).
-export([distribute_model/4, distribute_model_weights/4, load_db/3, train/4, load_nodes/3, check_node_alive/2]).



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
    message_primitives:wait_response(length(Pids), weights_ack, JavaUiPid).


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
    ResponseList = message_primitives:wait_response(length(Nodes), {train_pipeline_ack, CurrentEpoch}, JavaUiPid, use_error_filtering),

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