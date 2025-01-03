from networkModel import NetworkModel
from modelDefinition import network_definition
from federatedController import FederatedController

from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast

# todo: naming conventions
# todo: documentation for each method


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
        code, payload = message
        code = code.decode('utf-8')

        if code == "get_model":
            response = federatedController.get_definition()
            cast(master_pid, (encode_status_code("model_definition"), None, response))
        elif code == "get_weights":
            response = federatedController.get_weights()
            cast(master_pid, (encode_status_code("model_weights"), None, response))
        elif code == "update_weights":
            federatedController.update_weights(payload)
            cast(master_pid, (encode_status_code("update_weights_ack"), None, None))
        elif code == "save_model":
            result = federatedController.save_model(payload.decode('utf-8'))
            cast(master_pid, (encode_status_code("model_saved"), None, encode_status_code(result)))
        elif code == "load_model":
            result = federatedController.load_model(payload.decode('utf-8'))
            cast(master_pid, (encode_status_code("model_loaded"), None, encode_status_code(result)))
        else:
            cast(master_pid, (encode_status_code("python_unhandled"), "NODE master, invalid message code " + code))
        

    set_message_handler(handler)
    return (encode_status_code("ok"), "MODEL")



def encode_status_code(code: str) -> Atom:
    """
    Encode a status code string into an Erlang Atom.

    :param code: The status code string
    :return: An Erlang Atom representing the status code
    """

    return Atom(code.encode('utf-8'))