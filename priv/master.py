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

def get_model_definition() -> str:
    return federatedController.get_definition()

def get_model_weights() -> str:
    return federatedController.get_weights()

def update_model_weights(node_outputs: list[list, int]) -> str:
    federatedController.update_weights(node_outputs)
    return "Master model updated correctly"


# def register_handler(master_pid):
#     federatedController.master_pid = master_pid

#     def handler(message):
#         code, json_payload = message
#         code = code.decode('utf-8')

#         cast(master_pid, "received something in master python" + code)

#         if code == "get_model":
#             cast(master_pid, {encode_status_code("model_definition"), get_model_definition()})
#         else:
#             # cast(master_pid, f"NODE master, invalid message code")
#             raise Exception("Message code not handled")

#     set_message_handler(handler)
#     message = f"NODE master, handler registered Correctly"
#     return Atom(message.encode('utf-8'))


# def encode_status_code(code: str) -> Atom:
#     return Atom(code.encode('utf-8'))

def register_handler(master_pid):

    def handler(message):
        code, json_payload = message
        code = code.decode('utf-8')

        if code == "get_model":
            cast(master_pid, [encode_status_code("model_definition"), get_model_definition().encode('utf-8')])

    set_message_handler(handler)
    return "NODE Master, handler registered Correctly"



def encode_status_code(code: str) -> Atom:
    return Atom(code.encode('utf-8'))