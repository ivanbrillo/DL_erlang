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

    % try jsx:decode(Model, [return_maps]) of
    %     ModelData ->
    %         io:format("Successfully decoded JSON. Keys:~n~p~n", [maps:keys(ModelData)])
    % catch
    %     error:Error ->
    %         io:format("Failed to decode JSON. Error: ~p~nFirst 100 bytes of Model:~n~p~n", 
    %                   [Error, binary:part(Model, 0, min(100, byte_size(Model)))])
    % end,
    python:stop(P),

    ProcessedModel = jsx:encode(ModelData),
    {ok, Slave} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    python:call(Slave, handler, register_handler, [self()]),
    Slave ! ProcessedModel.


    % % Parse the JSON
    % ,
    % % Process the model structure as needed
    % % For example, you might want to modify some parameters
    % ProcessedModel = process_layers(ModelData),
    % % Re-encode to JSON
    % 


%% internal functions
