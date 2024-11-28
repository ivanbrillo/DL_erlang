-module(message_primitives).
-export([synch_message/4, synch_message/5, notify_ui/2, wait_response/2, wait_response/3]).
-define(TIMEOUT, 60000).   % default time-out


synch_message(PidList, Code, Message, AckCode, Timeout) when is_list(PidList) ->
    lists:foreach(fun(Pid) ->
        Pid ! {Code, Message}
    end, PidList),

    wait_response(length(PidList),  [], AckCode, Timeout);

synch_message(Pid, Code, Message, AckCode, Timeout) ->
    List = synch_message([Pid], Code, Message, AckCode, Timeout),
    case List of
        [] -> no_response;
        _ -> hd(List)
    end.

synch_message(Pid, Code, Message, AckCode) -> 
    synch_message(Pid, Code, Message, AckCode, ?TIMEOUT).


wait_response(N, AckCode) ->
    wait_response(N, [], AckCode, ?TIMEOUT).

wait_response(N, AckCode, Timeout)  ->
    wait_response(N, [], AckCode, Timeout).

wait_response(N, RespList, _, _) when N == 0 ->
    RespList;

wait_response(N, RespList, AckCode, Timeout) ->
    receive
        {AckCode, Response} -> wait_response(N-1, RespList ++ [Response], AckCode, Timeout)
    after Timeout ->
        io:format("--- ERROR: timeout occurs ---~n"),
        RespList
    end.


notify_ui(UiPid, Message) ->
    UiPid ! Message.

