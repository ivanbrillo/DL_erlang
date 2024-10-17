%%%-------------------------------------------------------------------
%% @doc ds_proj public API
%% @end
%%%-------------------------------------------------------------------

-module(main).
-export([initialize_model/3, update_weights/2, start_master_slave/0]).


start_master_slave() ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, Master} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    {ok, Slave} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    {Master, Slave}.


get_model(Master) ->
    RawModel = python:call(Master, master, get_model_definition, []),
    io:format("Raw data from Python (first 100 chars):~n~p~n", [string:slice(RawModel, 0, 100)]),
    Model = list_to_binary(RawModel),
    Model.
    % ModelData = jsx:decode(Model, [return_maps]),
    % ProcessedModel = jsx:encode(ModelData),


get_weights(Master) ->
    RawWeights = python:call(Master, master, get_model_weights, []),
    io:format("Raw data from Python (first 100 chars):~n~p~n", [string:slice(RawWeights, 0, 100)]),
    Weights = list_to_binary(RawWeights),
    Weights. 


initialize_model(Master, Slave, NodeId) ->
    Model = get_model(Master),
    python:call(Slave, node, register_handler, [self(), NodeId]),
    Slave ! {initialize, Model}.


update_weights(Master, Slave) ->
    Weights = get_weights(Master),
    Slave ! {update, Weights}.