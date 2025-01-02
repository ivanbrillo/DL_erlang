-module(message_primitives).
-export([synch_message/5, notify_ui/2, wait_response/3, flush_msg/1]).
-define(TIMEOUT, 60000).   % default time-out


synch_message(PidList, Code, Message, AckCode, DestinationNodeMetrics) when is_list(PidList) ->
    lists:foreach(fun(Pid) ->
        Pid ! {Code, Message}
    end, PidList),

    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(length(PidList),  [], AckCode, DestinationNodeMetrics, EndTime);

synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics) ->
    List = synch_message([Pid], Code, Message, AckCode, DestinationNodeMetrics),
    case List of
        [] -> [];
        _ -> hd(List)
    end.


wait_response(N, AckCode, DestinationNodeMetrics)  ->
    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(N, [], AckCode, DestinationNodeMetrics, EndTime).

wait_response(N, RespList, _, _, _EndTime) when N == 0 ->
    RespList;

wait_response(N, RespList, AckCode, DestinationNodeMetrics, EndTime) ->
    Now = erlang:monotonic_time(millisecond),
    TimeLeft = case EndTime - Now < 0 of
        true -> 0;
        false -> EndTime - Now
    end,

    receive
        {AckCode, Response} -> 
            wait_response(N-1, RespList ++ [Response], AckCode, DestinationNodeMetrics, EndTime);
        {node_metrics, Metrics} ->   % has high frequency and will interfere with normal timeout handling
            DestinationNodeMetrics ! {node_metrics, Metrics},
            wait_response(N, RespList, AckCode, DestinationNodeMetrics, EndTime)
    after TimeLeft ->
        io:format("--- ERROR: timeout occurs ---~n"),
        RespList
    end.


notify_ui(UiPid, Message) ->
    UiPid ! Message.


flush_msg(Msg) ->
    receive
        Msg -> flush_msg(Msg)
    after 0 -> ok
    end.
