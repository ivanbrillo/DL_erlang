-module(node).
-export([start_node/1]).



start_node(MasterPid) ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, PythonPid} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    Response = python:call(PythonPid, node, register_handler, [self(), node()]),
    io:format("~p~n", [Response]),
    loop_node(MasterPid, PythonPid).


loop_node(MasterPid, PythonPid) ->

    receive
        {load_db, _} ->
            Response = message_primitives:synch_message(PythonPid, load_db, null, db_ack),
            MasterPid ! {db_ack, {self(), Response}},
            io:format("NODE ~p, Load DB completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {initialize_model, Model} ->
            message_primitives:synch_message(PythonPid, initialize, Model, initialize_ack),
            MasterPid ! {initialize_ack, self()},
            io:format("NODE ~p,  Initialization completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {update_weights, Weights} ->  
            message_primitives:synch_message(PythonPid, update, Weights, weights_ack),
            MasterPid ! {weights_ack, self()},
            io:format("NODE ~p,  Weights updated successfully~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {train, _} ->
            Response = message_primitives:synch_message(PythonPid, train, null, train_ack),
            MasterPid ! {train_ack, {self(), Response}},
            io:format("NODE ~p,  Training completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {get_weights, _} ->
            Response = message_primitives:synch_message(PythonPid, get_weights, null, node_weights),
            MasterPid ! {node_weights, {self(), Response}},
            io:format("NODE ~p,  Weights returned~n", [node()]),
            loop_node(MasterPid, PythonPid);

        % Invalid message (discard it)
        _Invalid ->
            io:format("Invalid message discarded in nodo.~n"),
            loop_node(MasterPid, PythonPid)
    end.