-module(python_helper).
-export([init_python_process/0, python_register_handler/3]).


init_python_process() ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, PythonPid} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    PythonPid.

python_register_handler(PythonPid, Module, MasterPid) ->
    Result = python:call(PythonPid, Module, register_handler, [MasterPid]),
    io:format("~p~n", [Result]).

