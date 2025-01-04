import tensorflow as tf


class NetworkModel(tf.keras.Model):
    def __init__(self, sequential: tf.keras.Sequential):
        super().__init__()
        self.network: tf.keras.Sequential = sequential

    def call(self, x):
        network_output = self.network(x)
        # todo: add specific behavior depending on the model (eg. custom penality as in VAE)
        return network_output
