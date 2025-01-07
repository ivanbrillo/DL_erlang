from erlport.erlang import cast
from erlport.erlterms import Atom


def encode_status_code(code: str) -> Atom:
    """
    Encode a status code string into an Erlang Atom.

    :param code: The status code string
    :return: An Erlang Atom representing the status code
    """

    return Atom(code.encode('utf-8'))


def send_message(receiver, code: str, payload) -> None:
    cast(receiver, (code, None, payload))
    # add None because the standard msg format of the system id {Code, Pid, Payload}


