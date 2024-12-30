import pickle
import numpy as np
import json
from networkModel import NetworkModel


class FederatedController():
    def __init__(self, network_model: NetworkModel):
        super().__init__()
        self.model: NetworkModel = network_model 
        self.master_pid: int
    
    def get_definition(self) -> str:
        """
        Retrieve the network configuration as its bytes representation.

        Returns:
            A bytes object containing the network configuration.
        """
        return pickle.dumps(self.model.network.get_config())
    
    def get_weights(self) -> str:
        """
        Retrieve the model weights as its bytes representation.

        Returns:
            A bytes object containing the model weights.
        """
        return pickle.dumps(self.model.get_weights())

    def update_weights(self, node_outputs: list):
        """
        Update the model weights with the given node outputs.

        The node outputs are given as a list of bytes, representing the following tuple serialization:

        0. weights: A list of Numpy arrays representing the Node weights.
        1. size: The total size of the dataset for the specific Node weights.

        The function will average the weights of all nodes based on their dataset
        size and set the averaged weights as the new model weights.

        Args:
            node_outputs: A list of bytes object, where each element contains the node weights and dataset size.

        Returns:
            None
        """
        node_outputs = [pickle.loads(output) for output in node_outputs]
        new_weights = FederatedController.federated_weight_average(node_outputs)
        self.model.set_weights(new_weights)

    @staticmethod
    def federated_weight_average(node_outputs) -> list:
        # Number of nodes
        """
        Averages the weights from a list of node weights.

        Args:
            parsed_outputs: A list of dictionaries, where each dictionary contains the node weights and dataset size.

        Returns:
            A list of Numpy arrays, representing the averaged model weights.
        """
        
        n_nodes = len(node_outputs)
        if n_nodes == 0:
            raise ValueError("No outputs to average")
            
        # Calculate total dataset size
        total_size = sum(output[1] for output in node_outputs)
        
        # Get the structure of weights from first node
        first_weights = node_outputs[0][0]
        averaged_weights = [np.zeros_like(np.array(w)) for w in first_weights]
        
        # Average weights across nodes
        for node_output in node_outputs:
            weight = node_output[1] / total_size
            node_weights = node_output[0]
            
            # Add weighted contribution from this node
            for layer_idx, layer_weights in enumerate(node_weights):
                averaged_weights[layer_idx] += np.array(layer_weights) * weight
        
        return averaged_weights
    
    def save_model(self, path = "model") -> None:
        self.model.save(path + ".keras")

    def load_model(self, path = "model.keras") -> str:
        try:
            self.model.load_weights(path)
            return "true"
        except Exception as e:
            print(f"Error loading model weights: {str(e)}")
        return "false"

