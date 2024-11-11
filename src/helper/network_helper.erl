-module(network_helper).
-export([get_cluster_nodes/0, initialize_nodes/0]).

get_cluster_nodes() ->
    Nodes = net_adm:world(),
    MasterNode = node(),
    lists:filter(fun(N) -> N =/= MasterNode end, Nodes).

initialize_nodes() ->
    Active_nodes = get_cluster_nodes(),
    initialize_nodes(Active_nodes, []).

initialize_nodes([], Acc) ->
    lists:reverse(Acc);

initialize_nodes([Node|Rest], Acc) ->
    case rpc:call(Node, node, start_link, [self()]) of
        {ok, Pid} ->
            io:format("Started node on ~p~n", [Node]),
            initialize_nodes(Rest, [Pid|Acc]);
        {error, Reason} ->
            io:format("Failed to start node server on ~p: ~p~n", [Node, Reason]),
            initialize_nodes(Rest, Acc);
        {badrpc, Reason} ->
            io:format("RPC failed for node ~p: ~p~n", [Node, Reason]),
            initialize_nodes(Rest, Acc)
    end.