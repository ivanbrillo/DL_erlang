import tensorflow as tf
import json
from networkModel import NetworkModel


class NodeController:

    def __init__(self):
        self.master_pid: int
        self.node_id: int
        self.model: NetworkModel

    def initialize_model(self, json_string: str) -> None:
        data = json.loads(json_string)
        self.model = tf.keras.Model.from_config(data['config'])
    
    def update_model(self, json_string: str) -> None:
        data = json.loads(json_string)
        self.model.set_weights([tf.convert_to_tensor(w) for w in data['weights']])

    

