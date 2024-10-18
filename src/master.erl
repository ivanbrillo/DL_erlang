%%%-------------------------------------------------------------------
%% @doc ds_proj public API
%% @end
%%%-------------------------------------------------------------------

-module(master).
-export([start_master/0, loop_master/0, initialize_model/3, update_weights/3, train/2]).


start_master() ->
    MasterPid = spawn(?MODULE, loop_master, []),
    io:format("Starting local process on master~n"),

    PythonCodePath = code:priv_dir(ds_proj),
    {ok, Master} = python:start([{python_path, PythonCodePath}, {python, "python3"}]),
    io:format("Master start correctly~n"),

    {MasterPid, Master}.



loop_master() ->
    receive

        _Invalid ->
            io:format("Master received message.~n"),
            loop_master()
    end.




get_model(Master) ->
    RawModel = python:call(Master, master, get_model_definition, []),
    Model = list_to_binary(RawModel),
    io:format("Model retrieved correctly from Master~n"),
    Model.
    % ModelData = jsx:decode(Model, [return_maps]),
    % ProcessedModel = jsx:encode(ModelData).


initialize_model(Master, SlavePid, Slave) ->
    Model = get_model(Master),
    SlavePid ! {initialize, Slave, Model},
    io:format("Slave 1 model initialized correctly~n"),
    ok.




get_weights(Master) ->
    RawWeights = python:call(Master, master, get_model_weights, []),
    % io:format("Raw data from Python (first 100 chars):~n~p~n", [string:slice(RawWeights, 0, 100)]),
    Weights = list_to_binary(RawWeights),
    Weights. 


update_weights(Master, SlavePid, Slave) ->
    Weights = get_weights(Master),
    SlavePid ! {update, Slave, Weights},
    io:format("Slave 1 weights updated correctly~n"),
    ok.





train(SlavePid, Slave) ->
    SlavePid ! {train, Slave},
    io:format("Slave 1 train completed~n"),
    ok.
