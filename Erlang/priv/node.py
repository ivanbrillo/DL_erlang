import threading
from nodeController import NodeController
from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast
import json
import psutil
from ping3 import ping
import time

nodeController: NodeController = NodeController()


def register_handler(erlang_pid, node_id, master_ip):
    """
    Register a handler for Erlang messages to this Python process.

    :param erlang_pid: The Erlang process ID of the master process
    :param node_id: The ID of this node as a string
    :return: A string indicating whether the handler was registered correctly
    """

    nodeController.node_id = node_id.decode('utf-8')
    nodeController.master_ip = ".".join(map(str, master_ip))
    nodeController.erlang_pid = erlang_pid

    start_metrics_thread()

    def handler(message):
        code, payload = message
        code = code.decode('utf-8')

        if code == "initialize":
            nodeController.initialize_model(payload)
            cast(erlang_pid, (encode_status_code("initialize_ack"), None))
        elif code == "load_db":
            response = nodeController.load_db()
            cast(erlang_pid, (encode_status_code("db_ack"), response))
        elif code == "update":
            nodeController.update_model(payload)
            cast(erlang_pid, (encode_status_code("weights_ack"), None))
        elif code == "train":
            response = nodeController.train_local()
            cast(erlang_pid, (encode_status_code("train_ack"), response))
        elif code == "train_pipeline":
            nodeController.update_model(payload)
            accuracy = nodeController.train_local()
            response = nodeController.get_weights([nodeController.dataset_size])
            cast(erlang_pid, (encode_status_code("train_pipeline_ack"), (response, accuracy)))
        elif code == "get_weights":
            response = nodeController.get_weights([nodeController.dataset_size])
            cast(erlang_pid, (encode_status_code("node_weights"), response))
        else:
            cast(erlang_pid, (
            encode_status_code("python_unhandled"), f"NODE {nodeController.node_id}, invalid message code {code}"))

    set_message_handler(handler)
    return (encode_status_code("ok"), "NODE")


def encode_status_code(code: str) -> Atom:
    """
    Encode a status code string into an Erlang Atom.

    :param code: The status code string
    :return: An Erlang Atom representing the status code
    """
    return Atom(code.encode('utf-8'))


def start_metrics_thread():
    metrics_thread = threading.Thread(target=send_system_metrics)
    metrics_thread.daemon = True
    metrics_thread.start()


def send_system_metrics():
    """
    Fetch system metrics and send them to the Erlang node every second.
    """
    while True:
        cpu_usage = psutil.cpu_percent(interval=1)  # CPU usage in percentage
        memory_info = psutil.virtual_memory()
        memory_usage = memory_info.percent  # Memory usage in percentage

        response_time = ping(nodeController.master_ip, timeout=2)
        response_time_ms = response_time * 1000 if response_time is not None else None  # Convert to milliseconds

        node_details = {
            "Node": nodeController.node_id,
            "CPU": f"{cpu_usage}%",
            "Memory": f"{memory_usage}%",
            "Response Time": f"{response_time_ms:.2f} ms" if response_time_ms else "Unreachable",
        }

        cast(nodeController.erlang_pid, (encode_status_code("node_metrics"), json.dumps(node_details)))
        time.sleep(1)
