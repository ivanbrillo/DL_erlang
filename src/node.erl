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
            request_python(load_db, "", db_ack, db_ack, PythonPid, MasterPid),

            io:format("NODE ~p, Load DB completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {initialize_model, Model} ->
            send_python(initialize, Model, initialize_ack, PythonPid, MasterPid),

            io:format("NODE ~p,  Initialization completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {update_weights, Weights} ->  
            send_python(update, Weights, weights_ack, PythonPid, MasterPid),

            io:format("NODE ~p,  Weights updated successfully~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {train, _} ->
            request_python(train, "", train_ack, train_ack, PythonPid, MasterPid),

            io:format("NODE ~p,  Training completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {get_weights, _} ->
            request_python(get_weights, "", node_weights, weights_updated, PythonPid, MasterPid),

            io:format("NODE ~p,  Weights returned~n", [node()]),
            loop_node(MasterPid, PythonPid);

        % Invalid message (discard it)
        _Invalid ->
            io:format("Invalid message discarded in nodo.~n"),
            loop_node(MasterPid, PythonPid)
    end.



request_python(ReqCode, Payload, RespCode, SendCode, PythonPid, MasterPid) ->
    PythonPid ! {ReqCode, Payload},

    receive
        {RespCode, Response} -> 
            MasterPid ! {SendCode, {self(), Response}}
    end.

send_python(ReqCode, Payload, AckCode, PythonPid, MasterPid) ->
    PythonPid ! {ReqCode, Payload},

    receive 
        AckCode -> MasterPid ! {AckCode, self()}
    end.