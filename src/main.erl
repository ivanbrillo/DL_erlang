%%%-------------------------------------------------------------------
%% @doc myproj public API
%% @end
%%%-------------------------------------------------------------------

-module(main).
-export([call_python/0]).

call_python() ->
  PythonCodePath = code:priv_dir(ds),
  {ok, P} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
  python:call(P, prova, add, [2, 4]).
%% internal functions
