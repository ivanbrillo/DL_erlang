# ds_proj

### Documentation Links
- https://ivanbrillo.github.io/DL_erlang/
Note: The code documentation link in the page above doesn't work. The HTML files are located in `docs/_build/html/index.html`

### Hide Warnings (Linux)
```export TF_CPP_MIN_LOG_LEVEL=3```

### Starting Erlang Shells
To start the master node:
```rebar3 shell --sname master@localhost```

To start slave nodes:
```rebar3 shell --sname slave1@localhost```
```rebar3 shell --sname slave2@localhost```

### Model Commands
To initialize the master process and start Erlang node processes:
```P = master:start_master(), P ! initialize_nodes.```

Wait a few seconds for all nodes to initialize. Then proceed with the following commands:

* Load the database:
  ```P ! load_db.```

* Distribute the model:
  ```P ! distribute_model.```

* Distribute the weights:
  ```P ! distribute_weights.```

* Train the network:
  - For one epoch:
    ```P ! train.```
  - For multiple epochs:
    ```P ! {train, NEpochs}.```
    where `NEpochs` is the number of epochs to train


master_supervisor:start_link_shell().