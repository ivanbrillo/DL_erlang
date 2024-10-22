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
            PythonPid ! {load_db, ""},

            receive 
                [db_ack, Infos] -> MasterPid ! {db_ack, {self(), Infos}}
            end,

            io:format("NODE ~p, Load DB completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {initialize, Model} ->
            PythonPid ! {initialize, Model},

            receive 
                initialize_ack -> MasterPid ! {distribution_ack, self()}
            end,

            io:format("NODE ~p,  Initialization completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {update_weights, Weights} ->
            PythonPid ! {update, Weights},

            receive 
                weights_ack -> MasterPid ! {weights_ack, self()}
            end,

            io:format("NODE ~p,  Weights updated successfully~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {train, _} ->
            PythonPid ! {train, ""},  

            receive 
                [train_ack, Result] -> MasterPid ! {train_ack, {self(), Result}}
            end,        

            io:format("NODE ~p,  Training completed~n", [node()]),
            loop_node(MasterPid, PythonPid);

        {get_weights, _} ->
            PythonPid ! {get_weights, ""},

            receive
                [node_weights, Weights] -> 
                    MasterPid ! {weights_updated, {self(), Weights}}
            end,

            io:format("NODE ~p,  Weights returned~n", [node()]),
            loop_node(MasterPid, PythonPid);

        % Invalid message (discard it)
        _Invalid ->
            io:format("Invalid message discarded in nodo.~n"),
            loop_node(MasterPid, PythonPid)
    end.