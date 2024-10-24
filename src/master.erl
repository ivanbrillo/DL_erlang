%%%-------------------------------------------------------------------
%% @doc ds_proj public API
%% @end
%%%-------------------------------------------------------------------

-module(master).
-export([start_master/0, loop_master/1]).
-include("state.hrl").



start_master() ->
    PythonModel = helper:init_python_process(),
    PythonUI = helper:init_python_process(),
    State = #state{pythonModelPID = PythonModel, pythonUiPID = PythonUI},
    MasterPid = spawn(?MODULE, loop_master, [State]),

    helper:python_register_handler(PythonModel, master, MasterPid),
    helper:python_register_handler(PythonUI, ui, MasterPid),
    MasterPid.




loop_master(State) ->

    receive
        get_nodes ->
            Nodes = helper:get_cluster_nodes(),
            io:format("get_nodes. ~n"),
            helper:notify_ui(State, {nodes, Nodes}),
            loop_master(State);

        load_db ->
            {PidList, Infos} = helper:distribute_command(State#state.initializedNodes, load_db, db_ack, ""),
            io:format("DB loaded: ~p~n", [Infos]),
            NewState = State#state{dbLoadedNodes = PidList},
            helper:notify_ui(State, {db_loaded, PidList}),
            loop_master(NewState);

        initialize_nodes -> 
            InitializedNodes = helper:initialize_nodes(),
            NewState = State#state{initializedNodes = InitializedNodes},
            helper:notify_ui(State, {initialized_nodes, InitializedNodes}),
            loop_master(NewState);

        distribute_model ->
            ResponseList = helper:model_get_and_distribute(State#state.pythonModelPID, get_model, "", model_definition, initialize_model, initialize_ack, State#state.initializedNodes),

            NewState = State#state{distributedNodes = ResponseList},
            helper:notify_ui(State,{distributed_nodes, ResponseList}),
            loop_master(NewState);

        distribute_weights ->
            ResponseList = helper:model_get_and_distribute(State#state.pythonModelPID, get_weights, "", model_weights, update_weights, weights_ack, State#state.distributedNodes),

            NewState = State#state{weightsNodes = ResponseList},
            helper:notify_ui(State, {weights_updated_nodes, ResponseList}),        
            loop_master(NewState);

        train -> 
            NewState = helper:train(1, State),
            loop_master(NewState);

        {train, NEpochs} ->
            NewState = helper:train(NEpochs, State),
            loop_master(NewState);

        {python_unhandled, Cause} ->
            io:format("Python received unhandled message: ~p~n", [Cause]),
            loop_master(State);

        _Invalid ->
            io:format("Master received unhandled message. ~p~n", [_Invalid]),
            loop_master(State)
    end.
