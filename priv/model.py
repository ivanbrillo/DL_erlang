import tensorflow as tf
import json
import numpy as np

def serialize_model(model):
    config = model.get_config()
    # Convert NumPy arrays to lists directly
    weights = [w.tolist() for w in model.get_weights()]
    return json.dumps({"config": config, "weights": weights})

# Create your model
model = tf.keras.Sequential([
    tf.keras.layers.Input(shape=(10,)),  # Use Input layer to specify input shape
    tf.keras.layers.Dense(64, activation='relu'),
    tf.keras.layers.Dense(32, activation='relu'),
    tf.keras.layers.Dense(1, activation='sigmoid')
])

# Serialize the model
serialized_model = serialize_model(model)