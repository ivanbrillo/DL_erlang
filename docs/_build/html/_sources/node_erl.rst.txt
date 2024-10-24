Node Erlang Module
====================

.. module:: node
   :platform: Erlang
   :synopsis: Node Module in Erlang handles communication with a Python process and acts as a worker node.

Functions
---------

.. function:: start_node(MasterPid)

   Starts the node process, initializes the Python process for handling node operations, and enters the loop to receive and process messages from the master node.

   :param MasterPid: The identifier of the master process.
   :type MasterPid: pid
   :return: none
   :rtype: none


.. function:: loop_node(MasterPid, PythonPid)

   Main loop function of the node. Handles incoming messages and interacts with the Python process based on the message type. The node can receive the following messages:
   
   - **load_db**: Triggers the Python process to load the database and sends acknowledgment to the master.
   - **initialize**: Initializes the node with a received model and sends acknowledgment back to the master process.
   - **update_weights**: Updates the node's weights with the received values and sends acknowledgment to the master process.
   - **train**: Starts training with the assigned dataset and sends the training result to the master process.
   - **get_weights**: Requests the current weights from the node and returns them to the master process.
   - **Other invalid messages**: Discards and logs an invalid message.

   :param MasterPid: The identifier of the master process.
   :type MasterPid: pid
   :param PythonPid: The identifier of the Python process.
   :type PythonPid: pid
   :return: none
   :rtype: none

