from taa.services.products.states import get_all_states


def get_states():
    "Backwards-compatible formatted states for wizard."
    return [
        {"code": state[0], "name": state[1]}
        for state in get_all_states()
    ]