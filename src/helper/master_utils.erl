-module(master_utils).
-export([distribute_model/2, distribute_model_weights/2, load_db/1, train/4]).

distribute_model(PythonModelPid, Nodes) ->
    Model = message_primitives:synch_message(PythonModelPid, get_model, null, model_definition),
    % Instead of sending messages directly, use gen_server calls
    ResponseList = [node:initialize_model(Node, Model) || Node <- Nodes],
    ResponseList.

distribute_model_weights(PythonModelPid, Nodes) ->
    Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights),
    % Use gen_server calls to update weights
    ResponseList = [node:update_weights(Node, Weights) || Node <- Nodes],
    ResponseList.    

send_nodes_weights(PythonModelPid, Nodes) ->
    % Get weights using gen_server calls
    ResponseList = [node:get_weights(Node) || Node <- Nodes],
    {_PidList, Weights} = lists:unzip(ResponseList),
    message_primitives:synch_message(PythonModelPid, update_weights, Weights, update_weights_ack),
    Nodes.

% load_db(Nodes) ->
%     % Use gen_server calls to load DB
%     ResponseList = [node:load_db(Node) || Node <- Nodes],
%     {Nodes, ResponseList}.

load_db(Nodes) ->
    ResponseList = [node:load_db(Node) || Node <- Nodes],
    {PidList, Infos} = lists:unzip(ResponseList),
    {PidList, Infos}.


train(NEpochs, PythonModelPid, Nodes, UiPid) ->
    train(NEpochs, 0, PythonModelPid, Nodes, UiPid).

train(TotEpochs, TotEpochs, _, Nodes, _) ->
    Nodes;

train(TotEpochs, CurrentEpoch, PythonModelPid, Nodes, UiPid) ->
    % Train using gen_server calls
    ResponseList1 = [node:train(Node) || Node <- Nodes],
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