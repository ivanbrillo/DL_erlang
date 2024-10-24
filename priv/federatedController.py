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
        Retrieve the network configuration as a JSON string.

        Returns:
            A JSON string containing the network configuration.
        """

        config = self.model.network.get_config()
        return json.dumps({"config": config})
    
    def get_weights(self) -> str:
        """
        Retrieve the model weights as a JSON string.

        Returns:
            A JSON string containing the model weights.
        """

        weights = [w.tolist() for w in self.model.get_weights()]
        return json.dumps({"weights": weights})

    def update_weights(self, node_outputs: list[list]):
        """
        Update the model weights with the given node outputs.

        The node outputs are given as a list of bytes, which are expected to be
        JSON strings containing dictionaries with the following keys:

        - "weights": A list of Numpy arrays representing the Node weights.
        - "size": The total size of the dataset for the specific Node weights.

        The function will average the weights of all nodes based on their dataset
        size and set the averaged weights as the new model weights.

        Args:
            node_outputs: A list of JSONString, where each element contains the node weights and dataset size.

        Returns:
            None
        """

        data = [json.loads(output.decode('utf-8')) for output in node_outputs]

        new_weights = FederatedController.federated_weight_average(data)
        self.model.set_weights(new_weights)

    @staticmethod
    def federated_weight_average(parsed_outputs: list[dict]) -> list:
        # Number of nodes
        """
        Averages the weights from a list of node weights.

        Args:
            parsed_outputs: A list of dictionaries, where each dictionary contains the node weights and dataset size.

        Returns:
            A list of Numpy arrays, representing the averaged model weights.
        """
        
        n_nodes = len(parsed_outputs)
        if n_nodes == 0:
            raise ValueError("No outputs to average")
            
        # Calculate total dataset size
        total_size = sum(output["size"] for output in parsed_outputs)
        
        # Get the structure of weights from first node
        first_weights = parsed_outputs[0]["weights"]
        averaged_weights = [np.zeros_like(np.array(w)) for w in first_weights]
        
        # Average weights across nodes
        for node_output in parsed_outputs:
            weight = node_output["size"] / total_size
            node_weights = node_output["weights"]
            
            # Add weighted contribution from this node
            for layer_idx, layer_weights in enumerate(node_weights):
                averaged_weights[layer_idx] += np.array(layer_weights) * weight
        
        return averaged_weights
