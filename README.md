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

### Basic Commands
To start the environment, passing models and weights and loading the db in each node the command is:
```master_supervisor:start_link_shell(self()).``` 

To train the model for 1 epoch, the command is:
```master_api:train().```

While, if you want to train the model for `N` epoch, the command is:
```master_api:train(N).```

If you want to train the model for at most `N` epoch, until a `T` accuracy threshold is reached the command is:
```master_api:train(N, T).```


### Emulating disconnection
To emulate a node disconnection, from the master terminal you can execute:

```erlang:disconnect_node('slave1@localhost').```

This will disconnect the slave1@localhost node and the connection lost handling will be executed from the node and the master.