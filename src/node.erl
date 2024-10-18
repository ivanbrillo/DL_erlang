-module(node).
-export([start_node/2, loop_node/1]).



start_node(MasterPid, NodeId) ->
    SlavePid = spawn(?MODULE, loop_node, [MasterPid]),
    io:format("Starting local process on slave node~n"),

    PythonCodePath = code:priv_dir(ds_proj),
    {ok, Slave} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    python:call(Slave, node, register_handler, [MasterPid, NodeId]),

    io:format("Slave 1 start correctly~n"),

    {SlavePid, Slave}.



loop_node(MasterPid) ->
    receive
        {initialize, Slave, Model} ->
            Slave ! {initialize, Model},
            io:format("Initialization completed~n"),
            loop_node(MasterPid);

        {update, Slave, Weights} ->
            Slave ! {update, Weights},
            io:format("Weight update completed~n"),
            loop_node(MasterPid);
        
        {train, Slave} ->
            Slave ! {train, "Dataset"},
            io:format("Train completed~n"),
            loop_node(MasterPid);

        % Invalid message (discard it)
        _Invalid ->
            io:format("Invalid message discarded in nodo.~n"),
            loop_node(MasterPid)
    end.