from networkModel import NetworkModel
from modelDefinition import network_definition
from federatedController import FederatedController

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
