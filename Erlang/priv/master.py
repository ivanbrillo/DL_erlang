from networkModel import NetworkModel
from modelDefinition import network_definition
from federatedController import FederatedController

from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast
from networkHelper import *

federatedModel: NetworkModel = NetworkModel(network_definition)
federatedController: FederatedController = FederatedController(federatedModel)



def register_handler(master_pid):
    """
    Registers a message handler for handling communication with the Erlang master process.
    
    The handler processes incoming messages based on their code and performs actions.
    If an unrecognized message code is received, an unhandled message response is sent back.

    Args:
        master_pid: The process identifier of the Erlang master process.
    
    Returns:
        A confirmation string indicating that the handler was registered correctly.
    """
    
    federatedController.master_pid = master_pid

    def handler(message):
        code, pid, payload = message
        code = code.decode('utf-8')

        match code:
            case "get_model":
                response = federatedController.get_definition()
                send_message(master_pid, "model_definition", response)
            case "get_weights":
                response = federatedController.get_weights()
                send_message(master_pid, "model_weights", response)
            case "update_weights":
                federatedController.update_weights(payload)
                send_message(master_pid, "update_weights_ack", None)
            case "save_model":
                result = federatedController.save_model(payload.decode('utf-8'))
                send_message(master_pid, "model_saved", encode_status_code(result))
            case "load_model":
                result = federatedController.load_model(payload.decode('utf-8'))
                send_message(master_pid, "model_loaded", encode_status_code(result))
            case _:
                send_message(master_pid, "python_unhandled", "NODE master, invalid message code " + code)

    set_message_handler(handler)
    return (encode_status_code("ok"), "MODEL")



