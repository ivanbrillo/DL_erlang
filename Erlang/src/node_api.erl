-module(node_api).
-export([start_link/2, load_db/1, initialize_model/2, update_weights/2, train/1, get_weights/1, stop/1, train_pipeline/3, check_status/1]).


% initialize the node server and send to master {ok, {Pid, Node}}
start_link(MasterPid, MasterNode) ->
    {ok, Pid} = gen_server:start(node, [MasterPid, MasterNode], []),
    MasterPid ! {ok, {Pid, node()}},   % allows asynchronous calls to the function
    {ok, Pid}.

% load the specified pickle-encoded model to python, send to master {initialize_ack, Pid}
initialize_model(Pid, Model) ->
    gen_server:cast(Pid, {initialize_model, Model}).

% load the dataset in the path priv/Dataset and send to master {db_ack, {Pid, dataset_sizes_JSON}}
load_db(Pid) ->
    gen_server:cast(Pid, load_db).

% update the local weights with the specified ones and send to master {weights_ack, Pid}
update_weights(Pid, Weights) ->
    gen_server:cast(Pid, {update_weights, Weights}).

% get the current weights and send to master {node_weights, {Pid, Weights}}
get_weights(Pid) ->
    gen_server:cast(Pid, get_weights).

% perform one epoch of training
train(Pid) ->
    gen_server:cast(Pid, train).

% update the local weights and perform one epoch of training. It also send to master the updated weights
train_pipeline(Pid, Weights, Epoch) ->
    gen_server:cast(Pid, {train_pipeline, Weights, Epoch}).

% return true if the node is alive otherwise timeout or other exception exit codes (nodedown, shutdown)
check_status(Pid) ->
    gen_server:call(Pid, check_status).

% stop the gen server and the python process. Disconnect the node from the master
stop(Pid) ->
    gen_server:stop(Pid).

