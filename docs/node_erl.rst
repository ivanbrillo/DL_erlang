Node Erlang Module
====================

.. module:: node
   :platform: Erlang
   :synopsis: Description of Node Module in Erlang.

.. function:: loop_node(MasterPid)
    
    Main loop function of the node. Handles incoming messages and takes actions based on the message type.
    The node can receive the following messages:
        * 'initialize': Initializes the node with the received model.
        * 'update': Updates the node weights with the received ones.
        * 'train': Starts the node training with the received dataset.
        * Other messages: Ignores the message and continues the loop.

   :param MasterPid: The identifier of the master process.
   :type MasterPid: pid
   :return: none

.. uml::

   @startuml
   class ClassA {
       +Attribute1
       +Method1()
   }
   ClassB --> ClassA
   @enduml
