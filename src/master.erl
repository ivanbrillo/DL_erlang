%%%-------------------------------------------------------------------
%% @doc ds_proj public API
%% @end
%%%-------------------------------------------------------------------

-module(master).
-export([initialize_model/3, update_weights/2, start_master_slave/0]).


start_master_slave() ->
    PythonCodePath = code:priv_dir(ds_proj),
    {ok, Master} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    io:format("Master start correctly~n"),

    {ok, Slave} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    io:format("Slave 1 start correctly~n"),

    {Master, Slave}.


get_model(Master) ->
    RawModel = python:call(Master, master, get_model_definition, []),
    Model = list_to_binary(RawModel),
    io:format("Model retrieved correctly from Master~n"),
    Model.
    % ModelData = jsx:decode(Model, [return_maps]),
    % ProcessedModel = jsx:encode(ModelData),


get_weights(Master) ->
    RawWeights = python:call(Master, master, get_model_weights, []),
    % io:format("Raw data from Python (first 100 chars):~n~p~n", [string:slice(RawWeights, 0, 100)]),
    Weights = list_to_binary(RawWeights),
    Weights. 


initialize_model(Master, Slave, NodeId) ->
    Model = get_model(Master),
    python:call(Slave, node, register_handler, [self(), NodeId]),
    Slave ! {initialize, Model},
    io:format("Slave ~p model initialized correctly~n", [NodeId]),
    ok.


update_weights(Master, Slave) ->
    Weights = get_weights(Master),
    Slave ! {update, Weights},
    io:format("Slave 1 weights updated correctly~n"),
    ok.