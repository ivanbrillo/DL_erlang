import tensorflow as tf

model = tf.keras.Sequential([
    tf.keras.layers.Input(shape=(10,)),  # Use Input layer to specify input shape
    tf.keras.layers.Dense(64, activation='relu'),
    tf.keras.layers.Dense(32, activation='relu'),
    tf.keras.layers.Dense(1, activation='sigmoid')
])