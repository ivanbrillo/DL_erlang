import tensorflow as tf
import json
from networkModel import NetworkModel
from dataParser import *


class NodeController:

    def __init__(self):
        self.master_pid: int
        self.node_id: int
        self.model: NetworkModel
        self.dataset_size: int = 0 # todo 
        self.x_train = None
        self.y_train = None
        self.x_test = None
        self.y_test = None

    def load_db(self) -> str:
        """
        Load the dataset and preprocess it.

        Returns a JSON object containing the shapes of the training and test sets.
        """
        self.x_train, self.y_train, self.x_test, self.y_test = preprocess_image()
        self.dataset_size = self.x_train.shape[0]
        return json.dumps({"train_size": self.x_train.shape, "test_size": self.x_test.shape})

    def initialize_model(self, json_string: str) -> None:
        """
        Initialize the local model with the provided configuration.
        The model is compiled with the Adam optimizer and categorical cross-entropy loss.

        Args:
            json_string: A JSON string containing the model configuration.
        """
        data = json.loads(json_string)
        sequential = tf.keras.Sequential.from_config(data['config'])
        self.model = NetworkModel(sequential)
        self.model.compile(optimizer="adam", loss="categorical_crossentropy", metrics=["accuracy"])
    
    def update_model(self, json_string: str) -> None:
        """
        Update the local model with the provided weights.

        Args:
            json_string: A JSON string containing the weights of the model.
        """
        data = json.loads(json_string)
        self.model.set_weights([tf.convert_to_tensor(w) for w in data['weights']])

    def train_local(self) -> None:

        """
        Train the local model on the node's dataset for one epoch.

        Returns:
            A JSON object containing the training accuracy.
        """
        train_result = self.model.fit(
            self.x_train, self.y_train,
            epochs=1,
            batch_size=32,
            verbose=0
        )

        return json.dumps({"accuracy": train_result.history['accuracy']})


    def get_weights(self, add_cardinality: bool = False) -> str:
        """
        Returns the weights of the local model as a JSON string.

        Args:
            add_cardinality: If true, the dataset size of the node will be added to the JSON string.

        Returns:
            A JSON string containing the weights of the model and the optional dataset size.
        """
        weights = [w.tolist() for w in self.model.get_weights()]
        if add_cardinality:
            return json.dumps({"weights": weights, "size": self.dataset_size})
        else: 
            return json.dumps({"weights": weights})


