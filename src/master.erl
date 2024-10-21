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
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, PythonModel} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    io:format("Python model started correctly~n"),

    {ok, PythonUI} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    io:format("Python UI started correctly~n"),  % admits modularity (eg. javascript for UI in the future)

    State = #state{pythonModelPID = PythonModel, pythonUiPID = PythonUI},
    MasterPid = spawn(?MODULE, loop_master, [State]),
    io:format("Starting local process on master~n"),

    Result1 = python:call(PythonModel, master, register_handler, [MasterPid]),
    io:format("~p~n", [Result1]),

    Result2 = python:call(PythonUI, ui, register_handler, [MasterPid]),
    io:format("~p~n", [Result2]),

    % set return value to ok.
    MasterPid.



loop_master(State) ->

    receive
        get_nodes ->
            Nodes = get_cluster_nodes(),
            io:format("get_nodes. ~n"),
            State#state.pythonUiPID ! {nodes, Nodes},
            loop_master(State);

        initialize_nodes -> 
            InitializedNodes = initialize_nodes(),
            NewState = State#state{initializedNodes = InitializedNodes},
            State#state.pythonUiPID ! {initialized_nodes, InitializedNodes},
            loop_master(NewState);

        distribute_model ->
            State#state.pythonModelPID ! {get_model, ""},
            io:format("Send request for the model definition. ~p~n", [State#state.pythonModelPID]),

            receive
                [model_definition, ModelDefinition] -> 
                    ResponseList = distribute_object(State#state.initializedNodes, initialize, distribution_ack, ModelDefinition)
            end,

            io:format("Get all model distribution ack.~p~n", [ResponseList]),  % TODO to be changed
            NewState = State#state{distributedNodes = ResponseList},
            State#state.pythonUiPID ! {distributed_nodes, ResponseList},
            loop_master(NewState);

        distribute_weights ->
            State#state.pythonModelPID ! {get_weights, ""},
            io:format("Send request for the model weights. ~p~n", [State#state.pythonModelPID]),

            receive
                [model_weights, Weights] -> 
                    ResponseList = distribute_object(State#state.distributedNodes, update_weights, weights_ack, Weights)
            end,

            io:format("Get all weights update ack.~p~n", [ResponseList]),  % TODO to be changed
            NewState = State#state{weightsNodes = ResponseList},
            State#state.pythonUiPID ! {weights_updated_nodes, ResponseList},
            loop_master(NewState);

        train -> 
   
            TrainNodes = distribute_object(State#state.distributedNodes, train, train_ack, "train"),
            io:format("Get all train ack.~p~n", [TrainNodes]),  % TODO to be changed
            State#state.pythonUiPID ! {training_completed, TrainNodes},

            io:format("All the nodes are finished train, I can get the weights~n"),
            ResponseList = distribute_object(TrainNodes, get_weights, weights_updated, "get_weights"),
            {PidList, NewWeightsNodes} = lists:unzip(ResponseList),

            distribute_object([State#state.pythonModelPID], update_weights, update_weights_ack, NewWeightsNodes),
            io:format("Model update the weights correctly~n"),

            State#state.pythonUiPID ! {weights_model_updated, "weights updated correctly"},

            NewState = State#state{trainNodes = PidList},      
            loop_master(NewState);

        [python_unhandled, Cause] ->
            io:format("Python received unhandled message: ~p~n", [Cause]),
            loop_master(State);

        _Invalid ->
            io:format("Master received unhandled message. ~p~n", [_Invalid]),
            loop_master(State)
    end.





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
