-module(master_utils).
-export([distribute_model/3, distribute_model_weights/3, load_db/2, train/4, load_nodes/2]).


distribute_model(PythonModelPid, Pids, asynch) ->
    Model = message_primitives:synch_message(PythonModelPid, get_model, null, model_definition),
    lists:foreach(fun(Pid) -> node:initialize_model(Pid, Model) end, Pids),
    ok;

distribute_model(PythonModelPid, Pids, synch) ->
    distribute_model(PythonModelPid, Pids, asynch),
    message_primitives:wait_response(length(Pids), initialize_ack).


distribute_model_weights(PythonModelPid, Pids, asynch) ->
    Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights),
    lists:foreach(fun(Pid) -> node:update_weights(Pid, Weights) end, Pids),
    ok;

distribute_model_weights(PythonModelPid, Pids, synch) ->
    distribute_model_weights(PythonModelPid, Pids, asynch),
    message_primitives:wait_response(length(Pids), weights_ack).


load_db(Pids, asynch) ->
    lists:foreach(fun(Pid) -> node:load_db(Pid) end, Pids),
    ok;

load_db(Pids, synch) ->
    load_db(Pids, asynch),
    ResponseList = message_primitives:wait_response(length(Pids), db_ack),
    lists:unzip(ResponseList).


send_nodes_weights(PythonModelPid, Nodes) ->
    lists:foreach(fun(Pid) -> node:get_weights(Pid) end, Nodes),
    ResponseList = message_primitives:wait_response(length(Nodes), node_weights),
    {PidList, Weights} = lists:unzip(ResponseList),
    message_primitives:synch_message(PythonModelPid, update_weights, Weights, update_weights_ack),
    PidList.


train(NEpochs, PythonModelPid, Nodes, UiPid) ->
    train(NEpochs, 0, PythonModelPid, Nodes, UiPid).

train(TotEpochs, TotEpochs, _, Nodes, _) ->
    Nodes;

train(TotEpochs, CurrentEpoch, PythonModelPid, Nodes, UiPid) ->
    % Start training on all nodes
    lists:foreach(fun(Pid) -> node:train(Pid) end, Nodes),
    ResponseList1 = message_primitives:wait_response(length(Nodes), train_ack),
    {TrainNodes, Accuracy} = lists:unzip(ResponseList1),
    io:format("Finish training epoch ~p, accuracy: ~p~n", [CurrentEpoch, Accuracy]),
    message_primitives:notify_ui(UiPid, {training_completed, TrainNodes}),

    PidList = send_nodes_weights(PythonModelPid, TrainNodes),
    io:format("Model update the weights correctly for epoch: ~p~n", [CurrentEpoch]),
    message_primitives:notify_ui(UiPid, {weights_model_updated, PidList}),

    PidList2 = distribute_model_weights(PythonModelPid, PidList, synch),

    io:format("Nodes update the weights correctly for epoch: ~p~n", [CurrentEpoch]),
    message_primitives:notify_ui(UiPid, {weights_updated_nodes, PidList2}),

    train(TotEpochs, CurrentEpoch+1, PythonModelPid, PidList2, UiPid).


load_nodes(ListsPidNodes, PythonModelPid) ->
    {Pids, _Nodes} = lists:unzip(ListsPidNodes),
    load_db(Pids, asynch),
    distribute_model(PythonModelPid, Pids, asynch),
    distribute_model_weights(PythonModelPid, Pids, synch).
