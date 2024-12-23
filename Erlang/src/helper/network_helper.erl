-module(network_helper).
-export([get_cluster_nodes/0, initialize_nodes/0,  initialize_nodes/1, get_ip_node/0]).

get_cluster_nodes() ->
    Nodes = net_adm:world(),
    MasterNode = node(),
    lists:filter(fun(N) -> N =/= MasterNode end, Nodes).

initialize_nodes() ->
    ActiveNodes = get_cluster_nodes(),
    initialize_nodes(ActiveNodes).

initialize_nodes(Nodes) ->
    lists:filter(fun(N) -> rpc:cast(N, node_api, start_link, [self(), node()]) end, Nodes),
    PidsOk = message_primitives:wait_response(length(Nodes), ok, 20000),
    lists:foreach(fun({P, _N}) -> erlang:monitor(process, P) end, PidsOk),
    PidsOk.

get_ip_node() ->
    {ok, Address} = inet:getaddr(localhost, inet),   % TODO change to master host!!
    Address.