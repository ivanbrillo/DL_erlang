from nodeController import NodeController
from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast


nodeController: NodeController = NodeController()


def register_handler(master_pid, node_id):
    """
    Register a handler for Erlang messages to this Python process.

    :param master_pid: The Erlang process ID of the master process
    :param node_id: The ID of this node as a string
    :return: A string indicating whether the handler was registered correctly
    """
    
    nodeController.node_id = node_id.decode('utf-8')
    nodeController.master_pid = master_pid

    def handler(message):        
        code, payload = message
        code = code.decode('utf-8')

        if code == "initialize":
            nodeController.initialize_model(payload)
            cast(master_pid, (encode_status_code("initialize_ack"), None))
        elif code == "load_db":
            response = nodeController.load_db()
            cast(master_pid, (encode_status_code("db_ack"), response))
        elif code == "update":
            nodeController.update_model(payload)
            cast(master_pid, (encode_status_code("weights_ack"), None))
        elif code == "train":
            response = nodeController.train_local()
            cast(master_pid, (encode_status_code("train_ack"), response))
        elif code == "train_pipeline":
            nodeController.update_model(payload)
            accuracy = nodeController.train_local()
            response = nodeController.get_weights([nodeController.dataset_size])
            cast(master_pid, (encode_status_code("train_pipeline_ack"), (response, accuracy)))
        elif code == "get_weights":
            response = nodeController.get_weights([nodeController.dataset_size])
            cast(master_pid, (encode_status_code("node_weights"), response))
        else:
            cast(master_pid, (encode_status_code("python_unhandled"), f"NODE {nodeController.node_id}, invalid message code {code}"))
        

    set_message_handler(handler)
    return (encode_status_code("ok"), "NODE")



def encode_status_code(code: str) -> Atom:
    """
    Encode a status code string into an Erlang Atom.

    :param code: The status code string
    :return: An Erlang Atom representing the status code
    """
    return Atom(code.encode('utf-8'))