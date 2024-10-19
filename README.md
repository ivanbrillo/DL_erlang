# ds_proj

HIDE WARNINGS (Linux):
export TF_CPP_MIN_LOG_LEVEL=3

START THE MASTER WITH:    
rebar3 shell --sname master@localhost

START SOME SLAVES WITH:
rebar3 shell --sname slave1@localhost

SPAWN MASTER:
{MasterPid, Master} = master:start_master().

FROM THE MASTER YOU CAN SEE ALL THE NODES IN LOCALHOST THAT RESPOND POSITIVELY WITH THE PING:
master:get_cluster_nodes().

SPAWN NODES:
SlavePid = master:start_slave(slave1@localhost, MasterPid).

CALL OPERATION:
master:initialize_model(Master, SlavePid).
...