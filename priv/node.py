from nodeController import NodeController
from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast


nodeController: NodeController = NodeController()

def register_handler(master_pid, node_id):
    nodeController.node_id = node_id
    nodeController.master_pid = master_pid

    def handler(message):
        code, json_payload = message
        code = code.decode('utf-8')

        if code == "initialize":
            nodeController.initialize_model(json_payload)
            cast(master_pid, f"NODE {node_id}, correctly parsed")
        elif code == "update":
            cast(master_pid, f"NODE {node_id}, correctly updated")
            nodeController.update_model(json_payload)
        else:
            cast(master_pid, f"NODE {node_id}, invalid message code")
            raise Exception("Message code not handled")

    set_message_handler(handler)
    message = f"NODE {node_id}, handler registered Correctly"
    return Atom(message.encode('utf-8'))