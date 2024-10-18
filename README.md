# ds_proj

start the master with:      
rebar3 shell --sname master@localhost

start some slaves with:
erl -sname slave1@localhost

From the master you can see all the nodes in localhost that respond positively with the ping:
master:get_cluster_nodes().
