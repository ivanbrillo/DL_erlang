-module(master_api).
-export([start_link/0, get_nodes/0, load_db/0, initialize_nodes/0, distribute_model/0, distribute_weights/0, train/0, train/1, load_nodes/0]).



start_link() ->
    Response = gen_server:start_link({local, erlang_master}, master, [], []),
    initialize_nodes(),
    load_nodes(),
    Response.


get_nodes() ->
    gen_server:call(erlang_master, get_nodes).


initialize_nodes() ->
    gen_server:call(erlang_master, initialize_nodes, 20000).


load_nodes() ->
    gen_server:call(erlang_master, load_nodes, 20000).


load_db() ->
    gen_server:call(erlang_master, load_db).


distribute_model() ->
    gen_server:call(erlang_master, distribute_model).


distribute_weights() ->
    gen_server:call(erlang_master, distribute_weights).


train() ->
    gen_server:cast(erlang_master, {train, 1, 0}).

train(NEpochs) ->
    gen_server:cast(erlang_master, {train, NEpochs, 0}).

