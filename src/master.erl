%%%-------------------------------------------------------------------
%% @doc ds_proj public API
%% @end
%%%-------------------------------------------------------------------

-module(master).
-export([start_master/0, loop_master/1]).
-include("helper/state.hrl").



start_master() ->
    PythonModel = python_helper:init_python_process(),
    PythonUI = python_helper:init_python_process(),
    State = #state{pythonModelPID = PythonModel, pythonUiPID = PythonUI},
    MasterPid = spawn(?MODULE, loop_master, [State]),

    python_helper:python_register_handler(PythonModel, master, MasterPid),
    python_helper:python_register_handler(PythonUI, ui, MasterPid),
    MasterPid.




loop_master(State) ->

    receive
        get_nodes ->
            Nodes = network_helper:get_cluster_nodes(),
            io:format("get_nodes. ~n"),
            message_primitives:notify_ui(State#state.pythonUiPID, {nodes, Nodes}),
            loop_master(State);

        load_db ->
            {PidList, Infos} = master_utils:load_db(State#state.initializedNodes),
            io:format("DB loaded: ~p~n", [Infos]),
            NewState = State#state{dbLoadedNodes = PidList},
            message_primitives:notify_ui(State#state.pythonUiPID, {db_loaded,  PidList}),
            loop_master(NewState);

        initialize_nodes -> 
            InitializedNodes = network_helper:initialize_nodes(),
            NewState = State#state{initializedNodes = InitializedNodes},
            message_primitives:notify_ui(State#state.pythonUiPID, {initialized_nodes, InitializedNodes}),
            loop_master(NewState);

        distribute_model ->
            ResponseList = master_utils:distribute_model(State#state.pythonModelPID, State#state.initializedNodes),
            NewState = State#state{distributedNodes = ResponseList},
            message_primitives:notify_ui(State#state.pythonUiPID,{distributed_nodes, ResponseList}),
            loop_master(NewState);

        distribute_weights ->
            ResponseList = master_utils:distribute_model_weights(State#state.pythonModelPID, State#state.distributedNodes),
            NewState = State#state{weightsNodes = ResponseList},
            message_primitives:notify_ui(State#state.pythonUiPID, {weights_updated_nodes, ResponseList}),        
            loop_master(NewState);

        train -> 
            Nodes = master_utils:train(1, State#state.pythonModelPID, State#state.distributedNodes, State#state.pythonUiPID),
            NewState = State#state{trainNodes = Nodes},
            loop_master(NewState);

        {train, NEpochs} ->
            Nodes = master_utils:train(NEpochs, State#state.pythonModelPID, State#state.distributedNodes, State#state.pythonUiPID),
            NewState = State#state{trainNodes = Nodes},
            loop_master(NewState);

        {python_unhandled, Cause} ->
            io:format("Python received unhandled message: ~p~n", [Cause]),
            loop_master(State);

        _Invalid ->
            io:format("Master received unhandled message. ~p~n", [_Invalid]),
            loop_master(State)
    end.
