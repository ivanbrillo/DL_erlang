from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast
import time
import threading
import tensorflow as tf
import json



pid = 0
# counter = 0


def register_handler(dest):
    global pid  # Declare pid as global to modify it
    def handler(message):
        reconstructed_model = deserialize_model(message)
        # parsed = f"{message[0][0]} neurons, {message[0][1].to_string()}"
        cast(dest, b"corectly parsed")
    set_message_handler(handler)
    pid = dest
    return Atom(b"ok")


def deserialize_model(json_string):
    data = json.loads(json_string)
    # model = tf.keras.models.model_from_config(data['config'])
    # model = tf.keras.Sequential.from_config(data['config'])
    
    try:
        # Use from_config instead of model_from_config
        model = tf.keras.Sequential.from_config(data['config'])
    except AttributeError:
        # Fallback for older TensorFlow versions
        model = tf.keras.models.Sequential.from_config(data['config'])


    model.set_weights([tf.convert_to_tensor(w) for w in data['weights']])
    return model


# def loop_msg():
#     global counter  # Declare counter as global to modify it
#     while True: 
#         time.sleep(1)
#         counter += 1
#         cast(pid, counter)


# def start_loop():
#     thread = threading.Thread(target=loop_msg)
#     thread.daemon = True  # Allows the thread to exit when the main program exits
#     thread.start()

