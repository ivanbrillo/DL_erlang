Helper Erlang Module
====================

.. module:: helper
   :platform: Erlang
   :synopsis: Helper functions for the distributed system operations, including node initialization, object distribution, and Python process management.

Functions
---------

.. function:: get_cluster_nodes()

    Retrieves the list of cluster nodes, excluding the current master node.

   :return: A list of cluster nodes (excluding the master).
   :rtype: list

.. function:: initialize_nodes()

    Initializes the active nodes in the cluster by spawning processes on the nodes and starting the `node` module.

   :return: A list of process identifiers (PIDs) of the spawned nodes.
   :rtype: list

.. function:: distribute_object(PidList, Code, AckCode, Object)

    Sends a message with the given code and object to each PID in the list. 
    It waits for the acknowledgment (AckCode) from all nodes and collects the responses.

   :param PidList: List of process identifiers (PIDs) to send the object to.
   :type PidList: list
   :param Code: The code to send with the object.
   :type Code: term
   :param AckCode: The acknowledgment code expected from the nodes.
   :type AckCode: term
   :param Object: The object to distribute to the nodes.
   :type Object: term
   :return: List of responses from the nodes.
   :rtype: list

.. function:: wait_response(N, RespList, AckCode)

    Waits for responses from the nodes. It expects `N` responses with the given acknowledgment code.

   :param N: Number of responses to wait for.
   :type N: integer
   :param RespList: The list of responses collected so far.
   :type RespList: list
   :param AckCode: The acknowledgment code expected in the responses.
   :type AckCode: term
   :return: The complete list of responses.
   :rtype: list

.. function:: init_python_process()

    Initializes a Python process by starting it and setting the Python environment path.
    Returns the process identifier (PID) of the Python process.

   :return: The process identifier (PID) of the Python process.
   :rtype: pid

.. function:: python_register_handler(PythonPid, Module, MasterPid)

    Registers the Erlang master process as a handler for the Python process.

   :param PythonPid: The process identifier (PID) of the Python process.
   :type PythonPid: pid
   :param Module: The module in Python where the handler function is located.
   :type Module: atom
   :param MasterPid: The process identifier (PID) of the master Erlang process.
   :type MasterPid: pid
   :return: None

.. function:: model_get_and_distribute(PidReq, CodeReq, MsgReq, CodeResp, SendCode, Ack, ListPid)

    Sends a request to the Python process to retrieve a model or weights, then distributes the received data to a list of nodes.

   :param PidReq: The process identifier (PID) of the Python process making the request.
   :type PidReq: pid
   :param CodeReq: The code to request the model or weights.
   :type CodeReq: term
   :param MsgReq: The message accompanying the request.
   :type MsgReq: term
   :param CodeResp: The code to expect in the response.
   :type CodeResp: term
   :param SendCode: The code to send when distributing the received data.
   :type SendCode: term
   :param Ack: The acknowledgment code expected from the nodes.
   :type Ack: term
   :param ListPid: List of process identifiers (PIDs) of the nodes to distribute the data to.
   :type ListPid: list
   :return: A list of responses from the nodes.
   :rtype: list

.. function:: distribute_command(PidNodes, Request, ResponseCode, Payload)

    Distributes a command to a list of nodes and collects the responses. The payload is sent with the request.

   :param PidNodes: List of process identifiers (PIDs) of the nodes to send the request to.
   :type PidNodes: list
   :param Request: The request command to send.
   :type Request: term
   :param ResponseCode: The response code expected in the acknowledgment.
   :type ResponseCode: term
   :param Payload: The payload to send with the request.
   :type Payload: term
   :return: A tuple containing a list of PIDs and the corresponding responses.
   :rtype: tuple

.. function:: get_nodes_send_model(PidNodes, ModelPid, Request, ResponseCode, Payload, ModelCode, ModelAck)

    Retrieves model weights from the nodes, distributes the updated model, and sends it back to the Python process.

   :param PidNodes: List of process identifiers (PIDs) of the nodes to request the model from.
   :type PidNodes: list
   :param ModelPid: The process identifier (PID) of the Python process holding the model.
   :type ModelPid: pid
   :param Request: The request command to retrieve the model.
   :type Request: term
   :param ResponseCode: The response code expected from the nodes.
   :type ResponseCode: term
   :param Payload: The payload to send with the request.
   :type Payload: term
   :param ModelCode: The code to send when distributing the updated model.
   :type ModelCode: term
   :param ModelAck: The acknowledgment code expected from the nodes for the updated model.
   :type ModelAck: term
   :return: A list of PIDs of the nodes that successfully received the updated model.
   :rtype: list