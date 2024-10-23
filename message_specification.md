**Received by the Erlang Master**

From Python UI:
* get_nodes
* load_db
* initialize_nodes
* distribute_model
* distribute_weights
* train

From erlang Node:
* {weights_updated, {self(), Weights}}
* {train_ack, {self(), Accuracy}}
* {weights_ack, self()}
* {distribution_ack, self()}
* {db_ack, {self(), Infos}} 

From python Model:
* {update_weights_ack, ""}
* [model_weights, Weights]
* [model_definition, Model]


**Received by Python Master**

From Erlang master: 
* {get_model, ""}
* {get_weights, ""}
* {update_weights, Weights}

**Received by Erlang node**
* {load_db, ""} - [db_ack, Infos]
* {initialize, Model} - initialize_ack
* {update_weights, Weights} - weights_ack
* {train, ""} - [train_ack, Result]
* {get_weights, ""} - [node_weights, Weights]

**Received by Python node**
* {initialize, Model}
* {load_db, ""}
* {update, Weights}
* {train, ""}
* {get_weights, ""}
