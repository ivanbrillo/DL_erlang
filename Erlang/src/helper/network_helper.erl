-module(network_helper).
-export([get_cluster_nodes/0, initialize_nodes/1,  initialize_nodes/2, get_ip_host/1]).

get_cluster_nodes() ->
    Nodes = net_adm:world(),
    MasterNode = node(),
    lists:filter(fun(N) -> N =/= MasterNode end, Nodes).

initialize_nodes(JavaUiPid) ->
    ActiveNodes = get_cluster_nodes(),
    initialize_nodes(ActiveNodes, JavaUiPid).

initialize_nodes(Nodes, JavaUiPid) ->
    lists:filter(fun(N) -> rpc:cast(N, node_api, start_link, [self(), node()]) end, Nodes),
    PidsOk = message_primitives:wait_response(length(Nodes), ok, JavaUiPid),
    lists:foreach(fun({P, _N}) -> erlang:monitor(process, P) end, PidsOk),
    PidsOk.

get_ip_host(Host) ->
    {ok, Address} = inet:getaddr(Host, inet),
    Address.