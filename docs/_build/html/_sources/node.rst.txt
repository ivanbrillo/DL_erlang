Node Module
=================

This module serves as a node controller within the federated learning framework, handling communication
between the Erlang master process and the local Python node.

Functions
---------

.. function:: register_handler(master_pid, node_id)
    :noindex:

    Registers a handler for Erlang messages directed to this Python process. The handler processes incoming
    messages and executes corresponding actions based on their code. Responses are sent back to the master process
    as acknowledgments or data.

    :param master_pid: The Erlang process ID of the master process.
    :param node_id: The ID of this node as a string.
    :returns: A string indicating whether the handler was registered correctly.

.. function:: encode_status_code(code: str) -> Atom
    :noindex:

    Encodes a status code string into an Erlang Atom for communication with the Erlang system.

    :param code: The status code string.
    :returns: An Erlang Atom representing the status code.