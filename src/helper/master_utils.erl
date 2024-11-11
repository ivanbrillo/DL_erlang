-module(master_utils).
-export([distribute_model/2, distribute_model_weights/2, load_db/1, train/4]).

distribute_model(PythonModelPid, Nodes) ->
    Model = message_primitives:synch_message(PythonModelPid, get_model, null, model_definition),
    lists:foreach(fun(Node) -> node:initialize_model(Node, Model) end, Nodes),
    message_primitives:wait_response(length(Nodes), initialize_ack).

distribute_model_weights(PythonModelPid, Nodes) ->
    Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights),
    lists:foreach(fun(Node) -> node:update_weights(Node, Weights) end, Nodes),
    message_primitives:wait_response(length(Nodes), weights_ack).

send_nodes_weights(PythonModelPid, Nodes) ->
    lists:foreach(fun(Node) -> node:get_weights(Node) end, Nodes),
    ResponseList = message_primitives:wait_response(length(Nodes), node_weights),
    {_PidList, Weights} = lists:unzip(ResponseList),
    message_primitives:synch_message(PythonModelPid, update_weights, Weights, update_weights_ack),
    Nodes.

load_db(Nodes) ->
    lists:foreach(fun(Node) -> node:load_db(Node) end, Nodes),
    ResponseList = message_primitives:wait_response(length(Nodes), db_ack),
    {PidList, Infos} = lists:unzip(ResponseList),
    {PidList, Infos}.

train(NEpochs, PythonModelPid, Nodes, UiPid) ->
    train(NEpochs, 0, PythonModelPid, Nodes, UiPid).

train(TotEpochs, TotEpochs, _, Nodes, _) ->
    Nodes;

train(TotEpochs, CurrentEpoch, PythonModelPid, Nodes, UiPid) ->
    % Start training on all nodes
    lists:foreach(fun(Node) -> node:train(Node) end, Nodes),
    ResponseList1 = message_primitives:wait_response(length(Nodes), train_ack),
    {TrainNodes, Accuracy} = lists:unzip(ResponseList1),
    io:format("Finish training epoch ~p, accuracy: ~p~n", [CurrentEpoch, Accuracy]),
    message_primitives:notify_ui(UiPid, {training_completed, TrainNodes}),

    PidList = send_nodes_weights(PythonModelPid, TrainNodes),
    io:format("Model update the weights correctly for epoch: ~p~n", [CurrentEpoch]),
    message_primitives:notify_ui(UiPid, {weights_model_updated, PidList}),

    ResponseList2 = distribute_model_weights(PythonModelPid, Nodes),
    io:format("Nodes update the weights correctly for epoch: ~p~n", [CurrentEpoch]),
    message_primitives:notify_ui(UiPid, {weights_updated_nodes, ResponseList2}),

    train(TotEpochs, CurrentEpoch+1, PythonModelPid, ResponseList2, UiPid).