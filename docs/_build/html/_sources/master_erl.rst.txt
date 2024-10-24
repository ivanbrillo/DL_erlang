Master Erlang Module
====================

.. module:: master
   :platform: Erlang
   :synopsis: Module for managing the master node in a distributed learning system.

Functions
---------

.. function:: start_master()

    Starts the master node and initializes the Python processes for the model and the UI. 
    This function spawns the master process, registers handlers for the Python processes, 
    and returns the process identifier (PID) of the master.

   :return: The process identifier (PID) of the master.
   :rtype: pid

.. function:: notify_ui(State, Message)

    Sends a message to the Python UI process associated with the master. 
    This function is used to notify the UI about the current state or other events happening in the master node.

   :param State: The state record of the master process, containing the PID of the Python UI process.
   :type State: record(state)
   :param Message: The message to send to the UI.
   :type Message: term
   :return: None.

.. function:: loop_master(State)

    Main loop function of the master node. Handles incoming messages and takes actions based on the message type.
    The following messages can be processed by the master node:
        * `get_nodes`: Retrieves the current nodes in the cluster and notifies the UI.
        * `load_db`: Distributes the command to load the database across initialized nodes.
        * `initialize_nodes`: Initializes the nodes in the system and updates the state.
        * `distribute_model`: Distributes the model across the initialized nodes.
        * `distribute_weights`: Distributes the model weights across the distributed nodes.
        * `train`: Initiates the training process on the distributed nodes and updates the weights.
        * `[python_unhandled, Cause]`: Handles unhandled messages from the Python process.
        * Other messages: Discards unrecognized messages and continues the loop.

   :param State: The state record of the master process, containing the PIDs of the Python processes and lists of nodes.
   :type State: record(state)
   :return: None.


State Record
------------

The `state` record is used to keep track of the current state of the master process.
It includes the PIDs of the Python model and UI processes, as well as lists of nodes involved in the distributed learning process.

Fields:
    * **pythonModelPID**: PID of the Python process managing the model.
    * **pythonUiPID**: PID of the Python process managing the UI.
    * **initializedNodes**: List of PIDs of initialized nodes.
    * **dbLoadedNodes**: List of PIDs of nodes that have loaded the database.
    * **distributedNodes**: List of PIDs of nodes that have received the model.
    * **weightsNodes**: List of PIDs of nodes that have updated weights.
    * **trainNodes**: List of PIDs of nodes that have completed training.