-module(node).
-behaviour(gen_server).

%% API
-export([start_link/2, load_db/1, initialize_model/2, update_weights/2,
         train/1, get_weights/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, train_pipeline/2]).

%% API functions
start_link(MasterPid, MasterNode) ->
    {ok, Pid} = gen_server:start(?MODULE, [MasterPid, MasterNode], []),
    MasterPid ! {ok, {Pid, node()}},
    {ok, Pid}.

load_db(Pid) ->
    gen_server:cast(Pid, load_db).

initialize_model(Pid, Model) ->
    gen_server:cast(Pid, {initialize_model, Model}).

update_weights(Pid, Weights) ->
    gen_server:cast(Pid, {update_weights, Weights}).

train(Pid) ->
    gen_server:cast(Pid, train).

train_pipeline(Pid, Weights) ->
    gen_server:cast(Pid, {train_pipeline, Weights}).

get_weights(Pid) ->
    gen_server:cast(Pid, get_weights).

stop(Pid) ->
    gen_server:stop(Pid).


%% gen_server callbacks
init([MasterPid, MasterNode]) ->
    PythonPid = python_helper:init_python_process(),
    {ok, _Name} = python:call(PythonPid, node, register_handler, [self(), node()]),
    net_kernel:monitor_nodes(true),

    io:format("--- NODE ~p: Initialized correctly ---~n", [node()]),
    {ok, #{master_pid => MasterPid, master_node => MasterNode, python_pid => PythonPid}}.

handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(load_db, State = #{master_pid := MasterPid, python_pid := PythonPid}) ->
    Response = message_primitives:synch_message(PythonPid, load_db, null, db_ack),
    MasterPid ! {db_ack, {self(), Response}},
    io:format("--- NODE ~p: Load DB completed ---~n", [node()]),
    {noreply, State};

handle_cast({initialize_model, Model}, State = #{master_pid := MasterPid, python_pid := PythonPid}) ->
    message_primitives:synch_message(PythonPid, initialize, Model, initialize_ack),
    MasterPid ! {initialize_ack, self()},
    io:format("--- NODE ~p: Initialization completed ---~n", [node()]),
    {noreply, State};

handle_cast({update_weights, Weights}, State = #{master_pid := MasterPid, python_pid := PythonPid}) ->
    message_primitives:synch_message(PythonPid, update, Weights, weights_ack),
    MasterPid ! {weights_ack, self()},
    io:format("--- NODE ~p: Weights updated successfully ---~n", [node()]),
    {noreply, State};

handle_cast(train, State = #{master_pid := MasterPid, python_pid := PythonPid}) ->
    Response = message_primitives:synch_message(PythonPid, train, null, train_ack, 30000),
    
    MasterPid ! {train_ack, {self(), Response}},
    io:format("--- NODE ~p: Training completed ---~n", [node()]),
    {noreply, State};


handle_cast({train_pipeline, Weights}, State = #{master_pid := MasterPid, python_pid := PythonPid}) ->
    NewWeights = message_primitives:synch_message(PythonPid, train_pipeline, Weights, train_pipeline_ack),
    MasterPid ! {train_pipeline_ack, {self(), NewWeights}},
    io:format("--- NODE ~p: Training completed ---~n", [node()]),
    {noreply, State};


handle_cast(get_weights, State = #{master_pid := MasterPid, python_pid := PythonPid}) ->
    Response = message_primitives:synch_message(PythonPid, get_weights, null, node_weights),
    MasterPid ! {node_weights, {self(), Response}},
    io:format("--- NODE ~p: Weights returned ---~n", [node()]),
    {noreply, State}.

handle_info({nodedown, MasterNode}, State = #{master_node := MasterNode}) ->
    {stop, normal, State};


handle_info(Info, State) ->
    io:format("--- NODE ~p: Received unhandled message ~p ---", [node(), Info]),
    {noreply, State}.


terminate(_Reason, #{python_pid := PythonPid}) ->
    python:stop(PythonPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.