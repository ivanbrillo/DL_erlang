# ds_proj

HIDE WARNINGS (Linux):
export TF_CPP_MIN_LOG_LEVEL=3

START THE MASTER WITH:    
rebar3 shell --sname master@localhost

start some slaves with:
rebar3 shell --sname slave1@localhost
rebar3 shell --sname slave2@localhost

P = master:start_master(), P ! initialize_nodes.

wait some seconds ... and then you can distribute the model with:

P ! distribute_model. 
