from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast
import time
import threading

pid = 0
counter = 0


def register_handler(dest):
    global pid  # Declare pid as global to modify it
    def handler(message):
        cast(dest, message)
    set_message_handler(handler)
    pid = dest
    return Atom(b"ok")


def loop_msg():
    global counter  # Declare counter as global to modify it
    while True: 
        time.sleep(1)
        counter += 1
        cast(pid, counter)


def start_loop():
    thread = threading.Thread(target=loop_msg)
    thread.daemon = True  # Allows the thread to exit when the main program exits
    thread.start()

