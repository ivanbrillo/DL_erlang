from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast
import tensorflow as tf
import json
from networkModel import NetworkModel


class NodeController:

    def __init__(self):
        self.master_pid: int
        self.model: NetworkModel

    def register_handler(self, master_pid):
        def handler(message):
            self.model = NodeController.deserialize_model(message)
            cast(master_pid, b"corectly parsed")
        set_message_handler(handler)
        self.master_pid = master_pid
        return Atom(b"ok")
    
    @staticmethod
    def deserialize_model(json_string: str) -> tf.keras.Model:
        data = json.loads(json_string)
        model = tf.keras.Model.from_config(data['config'])
        model.set_weights([tf.convert_to_tensor(w) for w in data['weights']])
        return model