-module(node).
-export([start_node/2, loop_node/2]).



start_node(MasterPid, NodeId) ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, Slave} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    python:call(Slave, node, register_handler, [MasterPid, NodeId]),

    io:format("Slave 1 start correctly~n"),

    loop_node(MasterPid, Slave).



loop_node(MasterPid, Slave) ->
    receive
        {initialize, Model} ->
            Slave ! {initialize, Model},
            io:format("Initialization completed~n"),
            loop_node(MasterPid, Slave);

        {update, Weights} ->
            Slave ! {update, Weights},
            io:format("Weight update completed~n"),
            loop_node(MasterPid, Slave);
        
        {train, _} ->
            Slave ! {train, "Dataset"},
            io:format("Train completed~n"),
            loop_node(MasterPid, Slave);

        % Invalid message (discard it)
        _Invalid ->
            io:format("Invalid message discarded in nodo.~n"),
            loop_node(MasterPid, Slave)
    end.