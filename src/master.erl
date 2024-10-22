%%%-------------------------------------------------------------------
%% @doc ds_proj public API
%% @end
%%%-------------------------------------------------------------------

-module(master).
-export([start_master/0, loop_master/1]).

-record(state, {
    pythonModelPID :: pid(),
    pythonUiPID :: pid(),
    initializedNodes = [] :: [pid()],
    distributedNodes = [] :: [pid()],
    weightsNodes = [] :: [pid()],
    trainNodes = [] :: [pid()]
}).


start_master() ->
    PythonModel = helper:init_python_process(),
    PythonUI = helper:init_python_process(),
    State = #state{pythonModelPID = PythonModel, pythonUiPID = PythonUI},
    MasterPid = spawn(?MODULE, loop_master, [State]),

    helper:python_register_handler(PythonModel, master, MasterPid),
    helper:python_register_handler(PythonUI, ui, MasterPid),
    MasterPid.


notify_ui(State, Message) ->
    State#state.pythonUiPID ! Message.


loop_master(State) ->

    receive
        get_nodes ->
            Nodes = helper:get_cluster_nodes(),
            io:format("get_nodes. ~n"),
            notify_ui(State, {nodes, Nodes}),
            loop_master(State);

        initialize_nodes -> 
            InitializedNodes = helper:initialize_nodes(),
            NewState = State#state{initializedNodes = InitializedNodes},
            notify_ui(State, {initialized_nodes, InitializedNodes}),
            loop_master(NewState);

        distribute_model ->
            ResponseList = helper:model_get_and_distribute(State#state.pythonModelPID, get_model, "model definition", model_definition, initialize, distribution_ack, State#state.initializedNodes),

            NewState = State#state{distributedNodes = ResponseList},
            notify_ui(State,{distributed_nodes, ResponseList}),
            loop_master(NewState);

        distribute_weights ->
            ResponseList = helper:model_get_and_distribute(State#state.pythonModelPID, get_weights, "model weights", model_weights, update_weights, weights_ack, State#state.distributedNodes),

            NewState = State#state{weightsNodes = ResponseList},
            notify_ui(State, {weights_updated_nodes, ResponseList}),        
            loop_master(NewState);

        train -> 
            TrainNodes = helper:distribute_object(State#state.distributedNodes, train, train_ack, "train"),
            io:format("Get all train ack.~p~n", [TrainNodes]),  % TODO to be changed

            notify_ui(State, {training_completed, TrainNodes}),
            io:format("All the nodes are finished train, I can get the weights~n"),

            PidList = helper:get_nodes_send_model(TrainNodes, State#state.pythonModelPID, get_weights, weights_updated, "", update_weights, update_weights_ack),

            io:format("Model update the weights correctly~n"),
            notify_ui(State, {weights_model_updated, PidList}),

            NewState = State#state{trainNodes = PidList},      
            loop_master(NewState);

        [python_unhandled, Cause] ->
            io:format("Python received unhandled message: ~p~n", [Cause]),
            loop_master(State);

        _Invalid ->
            io:format("Master received unhandled message. ~p~n", [_Invalid]),
            loop_master(State)
    end.



