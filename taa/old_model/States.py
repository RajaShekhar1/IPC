from taa.services.products.states import get_all_states


def get_states():
    "Backwards-compatible formatted states for wizard."
    return [
        {"code": state['statecode'], "name": state['name']}
        for state in get_all_states()
    ]