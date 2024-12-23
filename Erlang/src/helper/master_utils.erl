-module(master_utils).
-export([distribute_model/3, distribute_model_weights/3, load_db/2, train/3, load_nodes/2, check_node_alive/2]).



distribute_model(PythonModelPid, Pids, async) ->
  Model = message_primitives:synch_message(PythonModelPid, get_model, null, model_definition),
  lists:foreach(fun(Pid) -> node_api:initialize_model(Pid, Model) end, Pids),
  ok;

distribute_model(PythonModelPid, Pids, sync) ->
  distribute_model(PythonModelPid, Pids, asyc),
  message_primitives:wait_response(length(Pids), initialize_ack).


distribute_model_weights(PythonModelPid, Pids, async) ->
  Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights),
  lists:foreach(fun(Pid) -> node_api:update_weights(Pid, Weights) end, Pids),
  ok;

distribute_model_weights(PythonModelPid, Pids, sync) ->
  distribute_model_weights(PythonModelPid, Pids, async),
  message_primitives:wait_response(length(Pids), weights_ack).


load_db(Pids, async) ->
  lists:foreach(fun(Pid) -> node_api:load_db(Pid) end, Pids),
  ok;

load_db(Pids, sync) ->
  load_db(Pids, async),
  ResponseList = message_primitives:wait_response(length(Pids), db_ack),
  lists:unzip(ResponseList).


train(CurrentEpoch, PythonModelPid, Nodes) ->
  Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights),
  lists:foreach(fun(Pid) -> node_api:train_pipeline(Pid, Weights) end, Nodes),
  ResponseList = message_primitives:wait_response(length(Nodes), train_pipeline_ack),

  case ResponseList of
    [] -> {[], 0};
    _ ->
      {PidList, Messages} = lists:unzip(ResponseList),
      {NewWeights, Accuracy} = lists:unzip(Messages),
      message_primitives:synch_message(PythonModelPid, update_weights, NewWeights, update_weights_ack),
      io:format("--- MASTER: train completed for epochs: ~p, resulting nodes accuracy: ~p ---~n", [CurrentEpoch, Accuracy]),
      {PidList, lists:sum(Accuracy) / length(Accuracy)}
  end.


load_nodes(ListsPidNodes, PythonModelPid) ->
  {Pids, _Nodes} = lists:unzip(ListsPidNodes),
  load_db(Pids, async),  % the acks sended by the nodes will be discarded in the master server loop and the next iteration
  distribute_model(PythonModelPid, Pids, async),
  distribute_model_weights(PythonModelPid, Pids, sync).


check_node_alive(Pid, Node) ->
  case rpc:call(Node, erlang, is_process_alive, [Pid]) of
    true ->
      case node_api:check_status(Pid) of   % the node could be in its terminating phase so the pid is still alive but the server is unavailable
        true -> true;
        _ -> false
      end;
    _ -> false
  end.