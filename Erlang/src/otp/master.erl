-module(master).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("../helper/state.hrl").



init([JavaPid]) ->
    io:format("--- MASTER: Starting erlang process, Java pid: ~p ---~n", [JavaPid]),

    PythonModel = python_helper:init_python_process(),
    State = #mstate{pythonModelPID = PythonModel, javaUiPid = JavaPid}, 
    
    python_helper:python_register_handler(PythonModel, master, self()),
    timer:send_after(10000, self(), check_nodes),  % check the connected nodes in 10s, useful for handling reconnection and new nodes
    {ok, State}.


handle_call(get_nodes, _From, State) ->
    Nodes = network_helper:get_cluster_nodes(),
    io:format("--- MASTER: nodes ~p ---~n", [Nodes]),
    {reply, Nodes, State};

handle_call(get_pid, _From, State) ->
    {reply, self(), State};

handle_call(load_nodes, _From, State) ->
    ActiveNodes = master_utils:load_nodes(State#mstate.currentUpNodes, State#mstate.pythonModelPID, State#mstate.javaUiPid),
    message_primitives:notify_ui(State#mstate.javaUiPid, {loaded_nodes, ActiveNodes}),
    io:format("--- MASTER: nodes loaded ~p ---~n", [ActiveNodes]),
    {reply, ok, State#mstate{currentUpNodes = ActiveNodes}};

handle_call(load_db, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    {PidList, Infos} = master_utils:load_db(PidNodes, State#mstate.javaUiPid, sync),
    message_primitives:notify_ui(State#mstate.javaUiPid, {db_loaded, PidList}),
    {reply, {ok, Infos}, State};

handle_call(initialize_nodes, _From, State) ->
    InitializedNodes = network_helper:initialize_nodes(State#mstate.javaUiPid),
    net_kernel:monitor_nodes(true),  % send messages nodeup/nodedown when a node connects/disconnects
    message_primitives:notify_ui(State#mstate.javaUiPid, {initialized_nodes, InitializedNodes}),
    io:format("--- MASTER: node initialized ~p ---~n", [InitializedNodes]),

    {reply, {ok, InitializedNodes}, State#mstate{currentUpNodes = InitializedNodes, previousInitializedNodes = InitializedNodes}};

handle_call(distribute_model, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    ResponseList = master_utils:distribute_model(State#mstate.pythonModelPID, PidNodes, State#mstate.javaUiPid, sync),
    message_primitives:notify_ui(State#mstate.javaUiPid, {distributed_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State};

handle_call(distribute_weights, _From, State) ->
    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    ResponseList = master_utils:distribute_model_weights(State#mstate.pythonModelPID, PidNodes, State#mstate.javaUiPid, sync),
    message_primitives:notify_ui(State#mstate.javaUiPid, {weights_updated_nodes, ResponseList}),
    {reply, {ok, ResponseList}, State};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast({new_train, EpochsLeft, CurrentEpoch, AccuracyThreshold}, State) ->
    message_primitives:notify_ui(State#mstate.javaUiPid, {new_train, {EpochsLeft,AccuracyThreshold}}),
    gen_server:cast(erlang_master, {train, EpochsLeft, CurrentEpoch, AccuracyThreshold}),
    {noreply, State#mstate{terminateTraining = false}};   % clear the terminateTraining flag of a possible previous training

handle_cast({train, EpochsLeft, CurrentEpoch, AccuracyThreshold}, State) when EpochsLeft > 0, CurrentEpoch >= 0, length(State#mstate.currentUpNodes) > 0 ->

    {PidNodes, _} = lists:unzip(State#mstate.currentUpNodes),
    io:format("--- MASTER: new train epochs, with nodes: ~p ---~n", [PidNodes]),
    {Nodes, TrainMeanAccuracy} = master_utils:train(CurrentEpoch, State#mstate.pythonModelPID, PidNodes, State#mstate.javaUiPid),

    case {EpochsLeft > 1, AccuracyThreshold >= TrainMeanAccuracy, length(Nodes) > 0, State#mstate.terminateTraining } of
        {true, true, true, false} ->
           if 
            CurrentEpoch rem 3 == 2 ->
                gen_server:cast(erlang_master, {save_model, backup}); % Call save_model every 3 epochs
            true -> ok
            end,
            gen_server:cast(erlang_master, {train, EpochsLeft - 1, CurrentEpoch + 1, AccuracyThreshold});
        {_, _, true, _} ->
            io:format("--- MASTER: training completed ---~n"),
            message_primitives:notify_ui(State#mstate.javaUiPid, {training_total_completed, TrainMeanAccuracy});
        {_, _, false, _} ->
            io:format("--- MASTER: training error ---~n"),
            message_primitives:notify_ui(State#mstate.javaUiPid, {training_error})
    end,

    {noreply, State};

handle_cast({train, _EpochsLeft, _CurrentEpoch, _AccuracyThreshold}, State) ->
    message_primitives:notify_ui(State#mstate.javaUiPid, {train_refused}),
    io:format("--- MASTER: training refused, possible causes: no nodes connected or illegal param ---~n"),
    {noreply, State};

handle_cast({save_model, Name}, State) ->
    Result = message_primitives:synch_message(State#mstate.pythonModelPID, save_model, Name, model_saved, State#mstate.javaUiPid),

    message_primitives:notify_ui(State#mstate.javaUiPid, {model_saved, Result}),
    io:format("--- MASTER: model saved ---~n"),
    {noreply, State};

handle_cast({load_model, Name}, State) ->
    Result = message_primitives:synch_message(State#mstate.pythonModelPID, load_model, Name, model_loaded, State#mstate.javaUiPid),

    message_primitives:notify_ui(State#mstate.javaUiPid, {model_loaded, Result}),
    io:format("--- MASTER: model loaded ~p ---~n", [Result]),
    {noreply, State};

handle_cast(stop_training, State) ->
    io:format("--- MASTER: train set to stop on next epoch or two ---~n"),
    {noreply, State#mstate{terminateTraining = true}}.

% acks not read in the load_nodes pipeline, useful for UI dashboard
handle_info({db_ack, _Pid, InfosDB}, State) ->
    message_primitives:notify_ui(State#mstate.javaUiPid, {db_ack, InfosDB}),
    {noreply, State};

handle_info({nodeup, Node}, State) ->
    io:format("--- MASTER: Node ~p connected ---~n", [Node]),
    NewState = master_utils:reconnect(Node, State),
    {noreply, NewState};


handle_info({nodedown, Node}, State) ->
    io:format("--- MASTER: Node ~p disconnected ---~n", [Node]),
    UpdatedUpNodes = lists:keydelete(Node, 2, State#mstate.currentUpNodes),   % remove from the connected node lists the disconnected node
    message_primitives:notify_ui(State#mstate.javaUiPid, {node_down, Node}),
    {noreply, State#mstate{currentUpNodes = UpdatedUpNodes}};

handle_info({python_unhandled, Cause}, State) ->
    io:format("--- MASTER: Python received unhandled message: ~p ---~n", [Cause]),
    {noreply, State};

handle_info(check_nodes, State) ->
    _Nodes = network_helper:get_cluster_nodes(),
    timer:send_after(10000, self(), check_nodes),  % periodic node check each 10s
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State) when Reason =/= noconnection ->   % noconnection errors are directly handled by nodedown
    message_primitives:notify_ui(State#mstate.javaUiPid, {node_down, node(Pid)}),
    NewState = master_utils:reconnect(node(Pid), State, Pid, Reason, _MonitorRef),
    UpdatedUpNodes = lists:keydelete(Pid, 1, NewState#mstate.currentUpNodes),
    {noreply, NewState#mstate{currentUpNodes = UpdatedUpNodes}};

handle_info({node_metrics, Metrics}, State) ->
    State#mstate.javaUiPid ! {node_metrics, Metrics},
    {noreply, State};

handle_info(Info, State) ->
    io:format("--- MASTER: received unhandled message: ~p ---~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->    % called from supervisor shutdown or stop function
    io:format("--- MASTER: Terminating Procedure ---~n"),
    message_primitives:notify_ui(State#mstate.javaUiPid, {master_terminating, "possible restarting by supervisor"}),
    python:stop(State#mstate.pythonModelPID), 
    lists:foreach(fun({Pid, _Node}) -> node_api:stop(Pid) end, State#mstate.previousInitializedNodes),  % send stop signal to all connected nodes
    ok.