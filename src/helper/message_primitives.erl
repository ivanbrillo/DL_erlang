-module(message_primitives).
-export([synch_message/4, notify_ui/2]).


% form {Code, Message} wait for AckCode
synch_message(PidList, Code, Message, AckCode) when is_list(PidList) ->
    lists:foreach(fun(Pid) ->
        Pid ! {Code, Message}
    end, PidList),

    wait_response(length(PidList),  [], AckCode);
    % TODO: handle the lack of response

synch_message(Pid, Code, Message, AckCode) ->
    hd(synch_message([Pid], Code, Message, AckCode)).

wait_response(N, RespList, _) when N == 0 ->
    RespList;

wait_response(N, RespList, AckCode) ->
    receive
        {AckCode, Response} -> wait_response(N-1, RespList ++ [Response], AckCode)
    end.

notify_ui(UiPid, Message) ->
    UiPid ! Message.

