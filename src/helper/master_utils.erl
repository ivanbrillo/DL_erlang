-module(master_utils).
-export([distribute_model/3, distribute_model_weights/3, load_db/2, train/3, load_nodes/2]).


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


train(CurrentEpoch, PythonModelPid, Nodes) ->
    Weights = message_primitives:synch_message(PythonModelPid, get_weights, null, model_weights),
    lists:foreach(fun(Pid) -> node:train_pipeline(Pid, Weights) end, Nodes),
    ResponseList = message_primitives:wait_response(length(Nodes), train_pipeline_ack),
    {PidList, Messages} = lists:unzip(ResponseList),
    {NewWeights, Accuracy} = lists:unzip(Messages),

    message_primitives:synch_message(PythonModelPid, update_weights, NewWeights, update_weights_ack),
    io:format("--- MASTER: train completed for epochs: ~p, resulting nodes accuracy: ~p ---~n", [CurrentEpoch, Accuracy]),
    PidList.


load_nodes(ListsPidNodes, PythonModelPid) ->
    {Pids, _Nodes} = lists:unzip(ListsPidNodes),
    load_db(Pids, asynch),
    distribute_model(PythonModelPid, Pids, asynch),
    distribute_model_weights(PythonModelPid, Pids, synch).
