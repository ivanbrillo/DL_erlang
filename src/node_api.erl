-module(node_api).
-export([start_link/2, load_db/1, initialize_model/2, update_weights/2, train/1, get_weights/1, stop/1, train_pipeline/2]).



start_link(MasterPid, MasterNode) ->
    {ok, Pid} = gen_server:start(node, [MasterPid, MasterNode], []),
    MasterPid ! {ok, {Pid, node()}},
    {ok, Pid}.


initialize_model(Pid, Model) ->
    gen_server:cast(Pid, {initialize_model, Model}).


load_db(Pid) ->
    gen_server:cast(Pid, load_db).


update_weights(Pid, Weights) ->
    gen_server:cast(Pid, {update_weights, Weights}).


get_weights(Pid) ->
    gen_server:cast(Pid, get_weights).


train(Pid) ->
    gen_server:cast(Pid, train).


train_pipeline(Pid, Weights) ->
    gen_server:cast(Pid, {train_pipeline, Weights}).


stop(Pid) ->
    gen_server:stop(Pid).

