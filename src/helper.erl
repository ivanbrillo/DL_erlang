-module(helper).
-export([get_cluster_nodes/0, initialize_nodes/0, distribute_object/4, init_python_process/0, python_register_handler/3, model_get_and_distribute/7, distribute_command/4, get_nodes_send_model/7, train/2, notify_ui/2]).
-include("state.hrl").



get_cluster_nodes() ->
    Nodes = net_adm:world(),
    MasterNode = node(), % Get the current node (master)
    lists:filter(fun(N) -> N =/= MasterNode end, Nodes). % Exclude the current node (master) from the list


initialize_nodes() ->
    Active_nodes = get_cluster_nodes(),
    [spawn(Node, node, start_node, [self()]) || Node <- Active_nodes].


% form {Code, Object} wait for AckCode
distribute_object(PidList, Code, AckCode,  Object) ->
    lists:foreach(fun(Pid) ->
        Pid ! {Code, Object}
    end, PidList),

    wait_response(length(PidList),  [], AckCode).
    % TODO: handle the lack of response


wait_response(N, RespList, _) when N == 0 ->
    RespList;

wait_response(N, RespList, AckCode) ->
    receive
        {AckCode, Response} -> wait_response(N-1, RespList ++ [Response], AckCode)
    end.

init_python_process() ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, PythonPid} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    PythonPid.

python_register_handler(PythonPid, Module, MasterPid) ->
    Result = python:call(PythonPid, Module, register_handler, [MasterPid]),
    io:format("~p~n", [Result]).

model_get_and_distribute(PidReq, CodeReq, MsgReq, CodeResp, SendCode, Ack, ListPid) ->
    PidReq ! {CodeReq, MsgReq},

    receive
        {CodeResp, Response} -> 
            ResponseList = helper:distribute_object(ListPid, SendCode, Ack, Response)
    end,

    ResponseList.

distribute_command(PidNodes, Request, ResponseCode, Payload) ->
    ResponseList = helper:distribute_object(PidNodes, Request, ResponseCode, Payload),
    {PidList, Response} = lists:unzip(ResponseList),
    {PidList, Response}.


get_nodes_send_model(PidNodes, ModelPid, Request, ResponseCode, Payload, ModelCode, ModelAck) ->
    {PidList, Response} = distribute_command(PidNodes, Request, ResponseCode, Payload),
    helper:distribute_object([ModelPid], ModelCode, ModelAck, Response),
    PidList.


train(NEpochs, State) ->
    train(NEpochs, 0, State).

train(TotEpochs, TotEpochs, State) ->
    State;

train(TotEpochs, CurrentEpoch, State) ->
    {TrainNodes, Accuracy} = helper:distribute_command(State#state.distributedNodes, train, train_ack, ""),
    io:format("Finish training epoch ~p, accuracy: ~p~n", [CurrentEpoch, Accuracy]),

    notify_ui(State, {training_completed, TrainNodes}),
    PidList = helper:get_nodes_send_model(TrainNodes, State#state.pythonModelPID, get_weights, weights_updated, "", update_weights, update_weights_ack),

    io:format("Model update the weights correctly for epoch: ~p~n", [CurrentEpoch]),
    notify_ui(State, {weights_model_updated, PidList}),

    ResponseList = helper:model_get_and_distribute(State#state.pythonModelPID, get_weights, "", model_weights, update_weights, weights_ack, PidList),

    io:format("Nodes update the weights correctly for epoch: ~p~n", [CurrentEpoch]),
    notify_ui(State, {weights_updated_nodes, ResponseList}),

    NewState = State#state{trainNodes = ResponseList},
    train(TotEpochs, CurrentEpoch+1, NewState).



notify_ui(State, Message) ->
    State#state.pythonUiPID ! Message.
