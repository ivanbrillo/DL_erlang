-module(master_api).
-export([start_link/1, get_nodes/0, get_server_pid/0, load_db/0, initialize_nodes/0, distribute_model/0, distribute_weights/0, train/0, train/1, train/2, load_nodes/0]).


% start the master server and the python model and initialize the nodes with the model, weights and db
start_link(JavaPid) ->
    Response = gen_server:start_link({local, erlang_master}, master, [JavaPid], []),
    initialize_nodes(),
    load_nodes(),
    Response.

% get the list of connected nodes to master
get_nodes() ->
    gen_server:call(erlang_master, get_nodes).

get_server_pid() ->
    gen_server:call(erlang_master, get_pid).

% start the node servers to the connected nodes in the network specified by the file .hosts.erlang
initialize_nodes() ->
    gen_server:call(erlang_master, initialize_nodes, 20000).

% load dataset, model and weights in each connected node
load_nodes() ->
    gen_server:call(erlang_master, load_nodes, 20000).

% load the dataset in each connected node
load_db() ->
    gen_server:call(erlang_master, load_db).

% distribute the model in each connected node
distribute_model() ->
    gen_server:call(erlang_master, distribute_model).

% distribute the weights in each connected node
distribute_weights() ->
    gen_server:call(erlang_master, distribute_weights).

% update the nodes weights, perform one epoch of training and update the master model weights
train() ->
    gen_server:cast(erlang_master, {train, 1, 0, 1}).

train(NEpochs) ->
    gen_server:cast(erlang_master, {train, NEpochs, 0, 1}).

% train for at most NEPochs if the mean accuracy of the models are less than the AccuracyThreshold
train(NEpochs, AccuracyThreshold) ->
    gen_server:cast(erlang_master, {train, NEpochs, 0, AccuracyThreshold}).

