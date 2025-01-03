-module(message_primitives).
-export([synch_message/5, notify_ui/2, wait_response/3, wait_response/4, flush_msg/1, flush_msg/3]).
-define(TIMEOUT, 60000).   % default time-out


synch_message(PidList, Code, Message, AckCode, DestinationNodeMetrics) when is_list(PidList) ->
    lists:foreach(fun(Pid) ->
        Pid ! {Code, self(), Message}
    end, PidList),

    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(length(PidList),  [], AckCode, DestinationNodeMetrics, EndTime, [], []);

synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics) ->
    List = synch_message([Pid], Code, Message, AckCode, DestinationNodeMetrics),
    case List of
        [] -> [];
        _ -> hd(List)
    end.


wait_response(N, AckCode, DestinationNodeMetrics)  ->
    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(N, [], AckCode, DestinationNodeMetrics, EndTime, [], []).

wait_response(N, AckCode, DestinationNodeMetrics, Pids)  ->
    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(N, [], AckCode, DestinationNodeMetrics, EndTime, [], Pids).
    

wait_response(N, RespList, _, _, _EndTime, Crashed, _Pids) when N == 0 ->
    lists:foreach(fun(CrashedMsg) -> self() ! CrashedMsg end, Crashed),  % retransmit exceptions for proper handling
    RespList;

wait_response(N, RespList, AckCode, DestinationNodeMetrics, EndTime, Crashed, Pids) ->
    Now = erlang:monotonic_time(millisecond),
    TimeLeft = case EndTime - Now < 0 of
        true -> 0;
        false -> EndTime - Now
    end,

    receive
        {AckCode, Pid, Response} -> 
            NewPids = lists:delete(Pid, Pids),
            wait_response(N-1, RespList ++ [Response], AckCode, DestinationNodeMetrics, EndTime, Crashed, NewPids);
        {node_metrics, Metrics} ->   % has high frequency and will interfere with normal timeout handling
            DestinationNodeMetrics ! {node_metrics, Metrics},
            wait_response(N, RespList, AckCode, DestinationNodeMetrics, EndTime, Crashed, Pids);
        {'DOWN', _MonitorRef, process, Pid, Reason} ->
            handleErrorMsg({'DOWN', _MonitorRef, process, Pid, Reason}, N, RespList, AckCode, DestinationNodeMetrics, EndTime, Crashed, Pids)

    after TimeLeft ->
        io:format("--- ERROR: timeout occurs ---~n"),
        lists:foreach(fun(CrashedMsg) -> self() ! CrashedMsg end, Crashed),  % retransmit exceptions for proper handling
        RespList
    end.


handleErrorMsg(Msg, N, RespList, AckCode, DestinationNodeMetrics, EndTime, Crashed, Pids) ->
    {'DOWN', _MonitorRef, process, Pid, _Reason} = Msg,

    case lists:member(Pid, Pids) of
        true -> 
            io:format("--- WARNING: nodedown or exception occurs during waiting the responses, Pid = ~p ---~n", [Pid]),
            NewPids = lists:delete(Pid, Pids),
            wait_response(N-1, RespList, AckCode, DestinationNodeMetrics, EndTime, Crashed ++ [Msg], NewPids);
        _ ->
            wait_response(N, RespList, AckCode, DestinationNodeMetrics, EndTime, Crashed ++ [Msg], Pids)
    end.


notify_ui(UiPid, Message) ->
    UiPid ! Message.


flush_msg(Msg) ->
    receive
        Msg -> flush_msg(Msg)
    after 0 -> ok
    end.

flush_msg('DOWN', Pid, Reason) ->
    receive
        {'DOWN', _MonitorRef, process, Pid2, Reason} when node(Pid) == node(Pid2) -> flush_msg('DOWN', Pid2, Reason)
    after 0 -> ok
    end.