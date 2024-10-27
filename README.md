# ds_proj

### DOC LINKS

- https://ivanbrillo.github.io/DL_erlang/

The code documentation link dones't work, the html is in the ```docs/_build/html/index.html```

### HIDE WARNINGS (Linux):
```export TF_CPP_MIN_LOG_LEVEL=3```

### Start Erlang shells

START THE MASTER WITH:    
```rebar3 shell --sname master@localhost```

start some slaves with:
```rebar3 shell --sname slave1@localhost```
```rebar3 shell --sname slave2@localhost```

### Model Commands

Initialize master process and start erlang nodes processes

```P = master:start_master(), P ! initialize_nodes.```

wait some seconds in order to wait the inizialization of all the nodes, then you can procede as follow:

* ```P ! load_db.```

* ```P ! distribute_model. ```

* ```P ! distribute_weights.```

* ```P ! train.```  To train for one epochs or ```P ! {train, NEpochs}```  for training with NEpochs the network
