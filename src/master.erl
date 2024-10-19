%%%-------------------------------------------------------------------
%% @doc ds_proj public API
%% @end
%%%-------------------------------------------------------------------

-module(master).
-export([start_master/0, loop_master/1]).

-record(state, {
    pythonModelPID = 0,
    pythonUiPID = 0,
    initializedNodes = [],
    distributedNodes = []
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
                    ResponseList = distribute_model(State#state.initializedNodes, ModelDefinition)
            end,

            io:format("Get all ack.~p~n", [ResponseList]),  % TODO to be changed
            NewState = State#state{distributedNodes = ResponseList},
            State#state.pythonUiPID ! {distributed_nodes, ResponseList},
            loop_master(NewState);

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



distribute_model(PidList, Model) ->
    lists:foreach(fun(Pid) ->
        Pid ! {initialize, Model}
    end, PidList),

    wait_ack(length(PidList),  []).
    % TODO: handle the lack of response


wait_ack(N, RespList) when N == 0 ->
    RespList;

wait_ack(N, RespList) ->
    receive
        {distribution_ack, Pid} -> wait_ack(N-1, RespList ++ [Pid])
    end.



% get_model(PythonPID) ->


%     RawModel = python:call(PythonPID, master, get_model_definition, []),
%     Model = list_to_binary(RawModel),
%     io:format("Model retrieved correctly from Master~n"),
%     Model.
%     % ModelData = jsx:decode(Model, [return_maps]),
%     % ProcessedModel = jsx:encode(ModelData).






get_weights(Master) ->
    RawWeights = python:call(Master, master, get_model_weights, []),
    % io:format("Raw data from Python (first 100 chars):~n~p~n", [string:slice(RawWeights, 0, 100)]),
    Weights = list_to_binary(RawWeights),
    Weights. 


update_weights(Master, SlavePid, Slave) ->
    Weights = get_weights(Master),
    SlavePid ! {update, Slave, Weights},
    io:format("Slave 1 weights updated correctly~n"),
    ok.





train(SlavePid, Slave) ->
    SlavePid ! {train, Slave},
    io:format("Slave 1 train completed~n"),
    ok.
