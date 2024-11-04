import pickle
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

    def initialize_model(self, bytes_model: str) -> None:
        """
        Initialize the local model with the provided configuration.
        The model is compiled with the Adam optimizer and categorical cross-entropy loss.

        Args:
            bytes_model: A bytes object containing the model configuration.
        """
        data = pickle.loads(bytes_model)
        sequential = tf.keras.Sequential.from_config(data)
        self.model = NetworkModel(sequential)
        self.model.compile(optimizer="adam", loss="categorical_crossentropy", metrics=["accuracy"])
    
    def update_model(self, bytes_weights: str) -> None:
        """
        Update the local model with the provided weights.

        Args:
            bytes_weights: A bytes object containing the weights of the model.
        """
        data = pickle.loads(bytes_weights)
        self.model.set_weights(data)

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
        Returns the weights of the local model as a bytes object.

        Args:
            add_cardinality: If true, the dataset size of the node will be added to the serialized object.

        Returns:
            A bytes object containing the weights of the model and the optional dataset size.
        """
        weights = [w.tolist() for w in self.model.get_weights()]
        return  pickle.dumps((weights, self.dataset_size)) if add_cardinality else pickle.dumps(weights)
