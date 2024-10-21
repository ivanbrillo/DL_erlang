from nodeController import NodeController
from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast


nodeController: NodeController = NodeController()


def register_handler(master_pid, node_id):
    nodeController.node_id = node_id.decode('utf-8')
    nodeController.master_pid = master_pid

    def handler(message):
        code, json_payload = message
        code = code.decode('utf-8')

        if code == "initialize":
            nodeController.initialize_model(json_payload)
            cast(master_pid, encode_status_code("initialize_ack"))
        elif code == "update":
            nodeController.update_model(json_payload)
            cast(master_pid, encode_status_code("weights_ack"))
        elif code == "train":
            nodeController.train_local()
            cast(master_pid, encode_status_code("train_ack"))
        elif code == "get_weights":
            nodeController.get_weights()
            cast(master_pid, [encode_status_code("node_weights"), nodeController.get_weights().encode('utf-8')])
        else:
            cast(master_pid, [encode_status_code("python_unhandled"), f"NODE {nodeController.node_id}, invalid message code {code}"])
        

    set_message_handler(handler)
    return f"NODE {nodeController.node_id}, handler registered Correctly"


def encode_status_code(code: str) -> Atom:
    return Atom(code.encode('utf-8'))