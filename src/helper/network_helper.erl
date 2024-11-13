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
    lists:reverse(Acc);  % Return the accumulated list of tuples

initialize_nodes([Node | Rest], Acc) ->
    case rpc:call(Node, node, start_link, [self()]) of
        {ok, Pid} ->
            io:format("Started node on ~p~n", [Node]),
            initialize_nodes(Rest, [{Pid, Node} | Acc]);  % Store the tuple {Pid, Node} in Acc
        {error, Reason} ->
            io:format("Failed to start node server on ~p: ~p~n", [Node, Reason]),
            initialize_nodes(Rest, Acc);  % Skip the failed node
        {badrpc, Reason} ->
            io:format("RPC failed for node ~p: ~p~n", [Node, Reason]),
            initialize_nodes(Rest, Acc)  % Skip the failed node
    end.
