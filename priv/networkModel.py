import tensorflow as tf


class NetworkModel(tf.keras.Model):
    def __init__(self, sequential: tf.keras.Sequential):
        super().__init__()
        self.network: tf.keras.Sequential = sequential

    def call(self, x):
        network_output = self.network_sequential(x)
        # to be add specific behavior
        return network_output
