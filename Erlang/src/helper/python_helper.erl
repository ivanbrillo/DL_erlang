-module(python_helper).
-export([init_python_process/0, python_register_handler/3]).


% initialize a python process and return its pid
init_python_process() ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, PythonPid} = python:start_link([{python_path, PythonCodePath}, {python, "python3"}]),
    PythonPid.

% register the python function responsible to handle communication between erlang and python
python_register_handler(PythonPid, Module, MasterPid) ->
    {ok, Name} = python:call(PythonPid, Module, register_handler, [MasterPid]),
    io:format("--- MASTER: python process ~s register correctly ---~n", [Name]),
    ok.

