-module(network_helper).
-export([get_cluster_nodes/0, initialize_nodes/0]).

get_cluster_nodes() ->
    Nodes = net_adm:world(),
    MasterNode = node(),
    lists:filter(fun(N) -> N =/= MasterNode end, Nodes).

initialize_nodes() ->
    ActiveNodes = get_cluster_nodes(),
    lists:filter(fun(N) -> rpc:cast(N, node, start_link, [self()]) end, ActiveNodes),
    PidsOk = message_primitives:wait_response(length(ActiveNodes), ok, 20000),
    PidsOk.
