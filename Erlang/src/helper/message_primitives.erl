-module(message_primitives).
-export([synch_message/5, synch_message/6, notify_ui/2, wait_response/3, wait_response/4]).
-define(TIMEOUT, 60000).   % default time-out


synch_message(PidList, Code, Message, AckCode, DestinationNodeMetrics, Timeout) when is_list(PidList) ->
    lists:foreach(fun(Pid) ->
        Pid ! {Code, Message}
    end, PidList),

    wait_response(length(PidList),  [], AckCode, DestinationNodeMetrics, Timeout);

synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics, Timeout) ->
    List = synch_message([Pid], Code, Message, AckCode, DestinationNodeMetrics, Timeout),
    case List of
        [] -> [];
        _ -> hd(List)
    end.

synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics) -> 
    synch_message(Pid, Code, Message, AckCode, DestinationNodeMetrics, ?TIMEOUT).


wait_response(N, AckCode, DestinationNodeMetrics) ->
    wait_response(N, [], AckCode, DestinationNodeMetrics, ?TIMEOUT).

wait_response(N, AckCode, DestinationNodeMetrics, Timeout)  ->
    wait_response(N, [], AckCode, DestinationNodeMetrics, Timeout).

wait_response(N, RespList, _, _, _) when N == 0 ->
    RespList;

wait_response(N, RespList, AckCode, DestinationNodeMetrics, Timeout) ->
    receive
        {AckCode, Response} -> 
            wait_response(N-1, RespList ++ [Response], AckCode, DestinationNodeMetrics, Timeout);
        {node_metrics, Metrics} -> 
            DestinationNodeMetrics ! {node_metrics, Metrics},
            wait_response(N, RespList, AckCode, DestinationNodeMetrics, Timeout)
    after Timeout ->
        io:format("--- ERROR: timeout occurs ---~n"),
        RespList
    end.

notify_ui(UiPid, Message) ->
    UiPid ! Message.

