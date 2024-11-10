-module(master_utils).
-export([distribute_model/2, distribute_model_weights/2, load_db/1, train/4]).


distribute_model(PythonModelPid, Nodes) ->
    Model = message_primitives:synch_message(PythonModelPid, get_model, null, model_definition),
    ResponseList = message_primitives:synch_message(Nodes, initialize_model, Model, initialize_ack),
    ResponseList.


distribute_model_weights(PythonModelPid, Nodes) ->
    Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights),
    ResponseList = message_primitives:synch_message(Nodes, update_weights, Weights, weights_ack),
    ResponseList.    


send_nodes_weights(PythonModelPid, Nodes) ->
    ResponseList = message_primitives:synch_message(Nodes, get_weights, null, node_weights),
    {PidList, Weights} = lists:unzip(ResponseList),
    message_primitives:synch_message(PythonModelPid, update_weights, Weights, update_weights_ack),
    PidList.


load_db(Nodes) ->
    ResponseList = message_primitives:synch_message(Nodes, load_db, null, db_ack),
    {PidList, Infos} = lists:unzip(ResponseList),
    {PidList, Infos}.
            

train(NEpochs, PythonModelPid, Nodes, UiPid) ->
    train(NEpochs, 0, PythonModelPid, Nodes, UiPid).

train(TotEpochs, TotEpochs, _, Nodes, _) ->
    Nodes;

train(TotEpochs, CurrentEpoch, PythonModelPid, Nodes, UiPid) ->
    ResponseList1 = message_primitives:synch_message(Nodes, train, null, train_ack),
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
