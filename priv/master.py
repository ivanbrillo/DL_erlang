from networkModel import NetworkModel
from modelDefinition import network_definition
from federatedController import FederatedController

from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast

# todo: class to handle synchronization between nodes 
# todo: naming conventions
# todo: documentation for each method


federatedModel: NetworkModel = NetworkModel(network_definition)
federatedController: FederatedController = FederatedController(federatedModel)


def update_model_weights(node_outputs: list[list, int]) -> str:
    federatedController.update_weights(node_outputs)
    return "Master model updated correctly"


def register_handler(master_pid):
    federatedController.master_pid = master_pid

    def handler(message):
        code, payload = message
        code = code.decode('utf-8')

        if code == "get_model":
            cast(master_pid, [encode_status_code("model_definition"), federatedController.get_definition().encode('utf-8')])
        elif code == "get_weights":
            cast(master_pid, [encode_status_code("model_weights"), federatedController.get_weights().encode('utf-8')])
        elif code == "update_weights":
            federatedController.update_weights(payload)
            cast(master_pid, (encode_status_code("update_weights_ack"), "ok"))
        else:
            cast(master_pid, [encode_status_code("python_unhandled"), "NODE master, invalid message code " + code])
        

    set_message_handler(handler)
    return "NODE Master, handler registered Correctly"



def encode_status_code(code: str) -> Atom:
    return Atom(code.encode('utf-8'))