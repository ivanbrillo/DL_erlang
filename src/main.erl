%%%-------------------------------------------------------------------
%% @doc myproj public API
%% @end
%%%-------------------------------------------------------------------

-module(main).
-export([call_python/0, pass_model/0]).

call_python() ->
  PythonCodePath = code:priv_dir(ds_proj),
  {ok, P} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
  python:call(P, prova, add, [2, 4]).


pass_model() ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, P} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    RawModel = python:call(P, master, get_model, []),
    io:format("Raw data from Python (first 100 chars):~n~p~n", [string:slice(RawModel, 0, 100)]),
    Model = list_to_binary(RawModel),
    ModelData = jsx:decode(Model, [return_maps]),
    python:stop(P),

    ProcessedModel = jsx:encode(ModelData),
    {ok, Slave} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    python:call(Slave, handler, register_handler, [self()]),
    Slave ! ProcessedModel.

