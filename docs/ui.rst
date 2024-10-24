UI Module
=================

.. module:: ui
   :platform: Python


This module manages the user interface (UI) interactions within the federated learning framework, specifically handling
communication between the Erlang master process and the Python UI component.

Functions
---------

.. function:: register_handler(master_pid)

    Registers a handler for Erlang messages directed to this Python UI process. The handler processes incoming
    messages and casts the payload back to the master process.

    :param master_pid: The Erlang process ID of the master process.
    :returns: A string indicating whether the handler was registered correctly.

.. function:: encode_status_code(code: str) -> Atom

    Encodes a status code string into an Erlang Atom for communication with the Erlang system.

    :param code: The status code string.
    :returns: An Erlang Atom representing the status code.