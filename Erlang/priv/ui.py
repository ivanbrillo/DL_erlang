from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast


def register_handler(master_pid):

    def handler(message):
        code, json_payload = message
        code = code.decode('utf-8')

        cast(master_pid, json_payload)
        # raise Exception(json_payload)

    set_message_handler(handler)
    return (encode_status_code("ok"), "UI")




def encode_status_code(code: str) -> Atom:
    return Atom(code.encode('utf-8'))