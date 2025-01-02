-module(message_primitives).
-export([synch_message/5, synch_message/6, notify_ui/2, wait_response/3, wait_response/4]).
-define(TIMEOUT, 60000).   % default time-out


synch_message(PidList, Code, Message, AckCode, DestinationNodeMetrics, Error) when is_list(PidList) ->
    lists:foreach(fun(Pid) ->
        Pid ! {Code, Message}
    end, PidList),

    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(length(PidList),  [], AckCode, DestinationNodeMetrics, [], Error, EndTime);

synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics, use_error_filtering) -> 
    synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics, error);


synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics, Error) ->
    List = synch_message([Pid], Code, Message, AckCode, DestinationNodeMetrics, Error),
    case List of
        [] -> [];
        _ -> hd(List)
    end.

synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics) -> 
    synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics, dont_use_filtering).





wait_response(N, AckCode, DestinationNodeMetrics)  ->
    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(N, [], AckCode, DestinationNodeMetrics, [], dont_use_filtering, EndTime).


wait_response(N, AckCode, DestinationNodeMetrics, use_error_filtering)  ->
    EndTime = erlang:monotonic_time(millisecond) + ?TIMEOUT,
    wait_response(N, [], AckCode, DestinationNodeMetrics, [], use_error_filtering, EndTime).


wait_response(N, RespList, _, _, Crashed, _Error, _EndTime) when N == 0 ->
    lists:foreach(fun(CrashedMsg) -> self() ! CrashedMsg end, Crashed),  % retransmit nodedown for proper disconnection handling or DOWN for proper crash handling
    RespList;

wait_response(N, RespList, AckCode, DestinationNodeMetrics, Crashed, Error, EndTime) ->
    Now = erlang:monotonic_time(millisecond),
    TimeLeft = case EndTime - Now < 0 of
        true -> 0;
        false -> EndTime - Now
    end,

    receive
        {AckCode, Response} -> 
            wait_response(N-1, RespList ++ [Response], AckCode, DestinationNodeMetrics, Crashed, Error, EndTime);
        {node_metrics, Metrics} ->   % has high frequency and will interfere with normal timeout handling
            DestinationNodeMetrics ! {node_metrics, Metrics},
            wait_response(N, RespList, AckCode, DestinationNodeMetrics, Crashed, Error, EndTime);
        {'DOWN', _MonitorRef, process, Pid, Reason} ->
            handleErrorMsg({'DOWN', _MonitorRef, process, Pid, Reason}, N, RespList, AckCode, DestinationNodeMetrics, Crashed, Error, EndTime)
            
    after TimeLeft ->
        io:format("--- ERROR: timeout occurs ---~n"),
        RespList
    end.


handleErrorMsg(Msg, N, RespList, AckCode, DestinationNodeMetrics, Crashed, Error, EndTime) ->
    case {node(), Error} of
        {'master@master', use_error_filtering} -> 
            io:format("--- WARNING: nodedown or exception occurs during waiting the responses, Msg = ~p ---~n", [Msg]),
            wait_response(N-1, RespList, AckCode, DestinationNodeMetrics, Crashed ++ [Msg], Error, EndTime);
        {_, _} ->
            % also other nodes receives the nodedown msg from other nodes, but this should interfere
            wait_response(N, RespList, AckCode, DestinationNodeMetrics, Crashed ++ [Msg], Error, EndTime)
    end.


notify_ui(UiPid, Message) ->
    UiPid ! Message.

