Master module
=============

This module serves as the master controller for the federated learning framework, facilitating communication
between the Python-based federated learning model and the Erlang master process.

Functions
---------

.. function:: register_handler(master_pid)

    Registers a message handler for communication with the Erlang master process. The handler processes incoming
    messages based on their code and executes corresponding actions. If an unrecognized message code is received,
    an unhandled message response is sent back.

    :param master_pid: The process identifier of the Erlang master process.
    :returns: A confirmation string indicating successful registration of the handler.

.. function:: encode_status_code(code: str) -> Atom

    Encodes a status code string into an Erlang Atom for communication with the Erlang system.

    :param code: The status code string.
    :returns: An Erlang Atom representing the status code.
