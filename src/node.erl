-module(node).
-export([start/2]).


start(MasterPid, NodeId) ->
    spawn(?MODULE, loop, [MasterPid, NodeId, not_initialized]).


loop(MasterPid, NodeId, Initialized) ->
    receive
        {message, List} when is_list(List) ->
            io:format("Received valid message: ~p~n", [{message, List}]),
            loop(MasterPid, NodeId, not_initialized);

        % Invalid message (discard it)
        _Invalid ->
            io:format("Invalid message discarded.~n"),
            loop(MasterPid, NodeId, not_initialized)
    end.