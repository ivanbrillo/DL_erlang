**Start the Erlang Shell for Master and Slaves**

* Run `rebar3 shell --sname master@localhost` to start the master.
* Run `rebar3 shell --sname slave1@localhost` and `rebar3 shell --sname slave2@localhost` to start two slaves.

**Initialization of the Master Node**

* Create two Python processes: one for the model and the other for the UI.
* Initialize the Python communication handler for messaging with the Erlang master.
* Create an Erlang process to perform a looping operation and handle all incoming messages.

**Initialization of Slave Nodes**

* Spawn a process on each node found on the specified network.
* Each node will spawn a Python process and register its message handler.
* The node enters a loop for message handling.

**Loading the Dataset**

* The master sends a message to all the Erlang nodes, requesting them to load the dataset.
* Once the Python process loads the dataset, the node will send an acknowledgment (ack).
* When all acks are collected, the Erlang master will continue its loop.

**Distributing the Model**

* The master asks the Python model to provide the model.
* The master sends a message to all the Erlang nodes, requesting them to load the provided TensorFlow model.
* Once the Python process loads the model, the node will send an ack.
* When all acks are collected, the Erlang master will continue its loop.

**Distributing Weights**

* The master asks the Python model to provide the weights of the network.
* The master sends a message to all the Erlang nodes, requesting them to load the provided weights.
* Once the Python process loads the weights, the node will send an ack.
* When all acks are collected, the Erlang master will continue its loop.

**Training**

* The master sends a message to all the Erlang nodes, requesting them to train their local models for a specified number of epochs.
* Once the Python process finishes training, the node will send an ack.
* When all acks are collected, the Erlang master will send the collected weights to the Python model, which will perform a weighted average and update its weights.
* I ask the python model to provide the weight of the new network and distribute them across different nodes. 
* When the nodes sends back an ack and there are still some epochs to be trained, the Erlang master will restart this sequence of steps.
