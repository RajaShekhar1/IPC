from __future__ import division

import itertools as it
import operator as op
import random
from collections import OrderedDict

from hamcrest import assert_that, equal_to

from taa.services.products import ProductService, Product
from taa.services.products.rates import Rates, TYPE_COVERAGE, TYPE_PREMIUM
from taa.services.products.payment_modes import MODES_BY_MODE


# Generate dummy data, test rate table, and verification rate table
rates = Rates()
VERIFIER = {}

# Group CI is only product using lookup table anymore.
base_products = [
    Product(
        code='Group CI',
    )
]
products = sorted([product.code for product in base_products])

ages = range(1, 30+1)
premiums = [random.randint(1, 500) for _ in range(4)]
coverages = [random.randint(1000, 100000) for _ in range(4)]
for product_code in products:
    for payment_mode in iter(MODES_BY_MODE.keys()):
        for type_ in (TYPE_COVERAGE, TYPE_PREMIUM):
            header = coverages if type_ == TYPE_COVERAGE else premiums
            lines = [','.join(['age'] + map(str, header))]
            for index, age in enumerate(ages):
                opfunc = op.truediv if type_ == TYPE_COVERAGE else op.mul

                # Verification table
                key = (product_code, payment_mode, type_, age)
                VERIFIER[key] = OrderedDict()
                for col in header:
                    VERIFIER[key][col] = opfunc(col, age * payment_mode)

                # CSV-like line for rate table
                line = map(str, [age] + map(opfunc, header,
                                            it.repeat(age*payment_mode,
                                                      len(header))))
                lines.append(','.join(line))
            rates.from_string('\n'.join(lines),
                              product_code, payment_mode, type_)

def get_rates_for(product_code, payment_mode, age, smoker=None, digits=2):
    # Verification function
    result = {}
    for type_ in (TYPE_COVERAGE, TYPE_PREMIUM):
        result[type_] = []

        for item in iter(VERIFIER.get(
                (product_code, payment_mode, type_, age)).items()):
            result[type_].append(
                {'payment_mode': payment_mode,
                 'coverage': item[0] if type_ == TYPE_COVERAGE else item[1],
                 'premium': round(item[0], digits) if type_ == TYPE_PREMIUM
                 else round(item[1], digits)}
            )

    return {
        'byface': result[TYPE_COVERAGE],
        'bypremium': result[TYPE_PREMIUM]
    }


def test_groupci():
    for payment_mode in iter(MODES_BY_MODE.keys()):
        for age in ages:
            assert_that(rates.get('Group CI', payment_mode, age),
                        equal_to(get_rates_for('Group CI', payment_mode, age))
                        )

