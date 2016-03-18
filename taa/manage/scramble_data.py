import calendar
import datetime
import json
import os
import random
import urllib

import dateutil.parser
from flask_script import Command

from ..models import db
from ..services.cases.models import CaseCensus


__all__ = ['get_names', 'get_first_names', 'get_last_names', 'gen_address',
           'get_street_names', 'get_cities', 'get_states', 'gen_ssn',
           'scramble_phone', 'scramble_date', 'scramble_email',
           'ScrambleDataCommand']


STREET_API_URL = 'https://www.randomlists.com/data/streets.json'
CITY_API_URL = 'https://www.randomlists.com/data/us-cities.json'
STATE_API_URL = 'https://www.randomlists.com/data/states.json'
ZIPCODE_API_URL = 'https://www.randomlists.com/data/zip-codes.json'


def get_names():
    with open(os.path.dirname(os.path.realpath(__file__)) + '/names.txt',
              'r') as f:
        return f.read().splitlines()


def get_first_names(names):
    return [n.split(' ')[0] for n in names]


def get_last_names(names):
    return [n.split(' ')[1] for n in names]


def gen_address(streets):
    return '{} {}'.format(random.randint(1, 9999), random.choice(streets))


def get_randomlist(url, key=None):
    response = json.load(urllib.urlopen(url))
    if key is None:
        return response['data']
    else:
        return [item[key] for item in response['data']]


def get_street_names():
    return get_randomlist(STREET_API_URL)


def get_cities():
    return get_randomlist(CITY_API_URL, key='name')


def get_states():
    return get_randomlist(STATE_API_URL, key='name')


def get_zipcodes():
    return get_randomlist(ZIPCODE_API_URL, key='name')


def gen_ssn():
    return ''.join([random.choice('0123456789') for _ in range(9)])


def scramble_phone():
    return '{}-{}-{}'.format(random.randint(100, 999), random.randint(100, 999),
                             random.randint(1000, 9999))


def scramble_date(date):
    if not date:
        return None

    if not isinstance(date, datetime.date):
        try:
            date = dateutil.parser.parse(date)
        except ValueError:
            return None

    new_month = random.randint(1, 12)
    days_in_month = calendar.monthrange(date.year, new_month)
    new_day = random.randint(1, days_in_month[1])
    return date.replace(day=new_day, month=new_month)


def scramble_email(first, last):
    # Email domains come from http://www.fakemailgenerator.com to allow for
    # checking emails sent
    # Emails are formatted as firstname.lastname@domain.com
    domains = ['cuvox.de', 'jourrapide.com', 'armyspy.com', 'dayrep.com',
               'fleckens.hu', 'gustr.com', 'rhyta.com', 'superrito.com']
    return '{}.{}@{}'.format(first.lower(), last.lower(),
                             random.choice(domains))


class ScrambleDataCommand(Command):
    """Scramble all data in the database to not contain real information"""

    def run(self):
        # Read names.txt file
        names = get_names()

        # Parse names files into first and last names
        firstNames = get_first_names(names)
        lastNames = get_last_names(names)

        # Get a list of 'real-ish' street names
        streets = get_street_names()

        # These API's aren't needed, but might be useful if we need to scramble
        # cities and states
        # cities = get_cities()
        # states = get_states()
        # zipcodes = get_zipcodes()

        # Get census data from database
        census_data = db.session.query(CaseCensus).all()

        used_ssns = set()

        def get_ssn():
            new_ssn = gen_ssn()
            while new_ssn in used_ssns:
                new_ssn = gen_ssn()
            used_ssns.add(new_ssn)
            return new_ssn

        for census in census_data:
            # Get a random last name for our family
            last_name = random.choice(lastNames)

            # Create an address for our family
            address = gen_address(streets)

            attributes = [
                ('employee_first', lambda: random.choice(firstNames)),
                ('employee_last', lambda: last_name),
                ('employee_ssn', lambda: get_ssn()),
                ('employee_email', lambda: scramble_email(census.employee_first,
                                                          census.employee_last)),
                ('employee_street_address', lambda: address),
                ('employee_phone', lambda: scramble_phone()),
                ('employee_birthdate', lambda: scramble_date(
                        census.employee_birthdate)),

                ('spouse_first', lambda: random.choice(firstNames)),
                ('spouse_last', lambda: last_name),
                ('spouse_ssn', lambda: get_ssn()),
                ('spouse_email', lambda: scramble_email(census.spouse_first,
                                                        census.spouse_last)),
                ('spouse_street_address', lambda: address),
                ('spouse_phone', lambda: scramble_phone()),
                ('spouse_birthdate', lambda: scramble_date(
                        census.spouse_birthdate)),
            ]

            for enrollment_application in census.enrollment_applications:
                beneficiary_attributes =[
                    ('employee_beneficiary_name', lambda: u'{} {}'.format(
                            random.choice(firstNames),
                            random.choice(lastNames))),
                    ('spouse_beneficiary_name', lambda: u'{} {}'.format(
                            random.choice(firstNames),
                            random.choice(lastNames))),
                    ('employee_other_owner_name', lambda: u'{} {}'.format(
                            random.choice(firstNames),
                            random.choice(lastNames))),
                    ('employee_beneficiary_ssn', lambda: get_ssn()),
                    ('spouse_beneficiary_ssn', lambda: get_ssn()),
                    ('employee_other_owner_ssn', lambda: get_ssn()),
                    ('employee_beneficiary_birthdate', lambda: scramble_date(
                            enrollment_application.employee_beneficiary_birthdate)),
                    ('spouse_beneficiary_birthdate', lambda: scramble_date(
                            enrollment_application.employee_beneficiary_birthdate)),
                ]
                for attr, new_val in beneficiary_attributes:
                    if getattr(enrollment_application, attr):
                        setattr(enrollment_application, attr, new_val())

            # Set employee and spouse information
            for attr, new_val in attributes:
                if getattr(census, attr):
                    setattr(census, attr, new_val())

            # Six possible children in a record
            for x in range(1, 6+1):
                # Check each child to see if the exists (if there is a
                # first name)
                if getattr(census, 'child{}_first'.format(x)):
                    # Set new first and last name for the child
                    setattr(census, 'child{}_first'.format(x),
                            random.choice(firstNames))
                    setattr(census, 'child{}_last'.format(x), last_name)

                    # Some records may not have birthdates. If not create
                    # random date
                    birthdate = (getattr(census,
                                         'child{}_birthdate'.format(x)) or
                                 datetime.datetime.now().replace(
                                         year=random.randrange(1995, 2001)))
                    setattr(census, 'child{}_birthdate'.format(x),
                            scramble_date(birthdate))

        db.session.commit()
