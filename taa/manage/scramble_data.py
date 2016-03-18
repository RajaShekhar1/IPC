from flask import current_app
from flask_script import Command, prompt, prompt_pass
from werkzeug.datastructures import MultiDict

import os
import random
import urllib, json
import calendar, datetime

from ..services.cases.models import CaseCensus
from ..models import db

class ScrambleDataCommand(Command):
    """Scramble all data in the database to not contain real information"""

    def run(self):

        # Read names.txt file
        nameFile = open(os.path.dirname(os.path.realpath(__file__))+'/names.txt', 'r')
        names = nameFile.read()
        names = names.splitlines()

        # Parse names files into first and last names
        firstNames = [n.split(" ")[0] for n in names]
        lastNames = [n.split(" ")[1] for n in names]

        # Get a list of 'real-ish' street names
        streetsApi = "https://www.randomlists.com/data/streets.json"
        streetsResponse = urllib.urlopen(streetsApi)
        streets = json.load(streetsResponse)["data"]

        # These API's aren't needed, but might be useful if we need to scramble cities and states
        # citiesApi = "https://www.randomlists.com/data/us-cities.json"
        # citiesResponse = urllib.urlopen(citiesApi)
        # cities = json.load(citiesResponse)["data"]

        # statesApi = "https://www.randomlists.com/data/states.json"
        # statesResponse = urllib.urlopen(statesApi)
        # states = json.load(statesResponse)["data"]

        # Get census data from database
        census_data = db.session.query(CaseCensus).all()


        used_ssns = set()

        def gen_ssn():
            return "".join([random.choice("0123456789") for _ in range(9)])

        def get_ssn():
            new_ssn = gen_ssn()
            while new_ssn in used_ssns:
                new_ssn = gen_ssn()
            used_ssns.add(new_ssn)
            return new_ssn

        def scramble_date(date):
            if not date:
                return None

            if not isinstance(date, datetime.date):
                try:
                    date = datetime.datetime.strptime(date, '%m/%d/%Y')
                except ValueError:
                    return None

            new_month = random.randrange(1,13)
            days_in_month = calendar.monthrange(date.year, new_month)
            new_day = random.randrange(1, days_in_month[1]+1)
            return date.replace(day=new_day, month=new_month)

        def scramble_email(first, last):
            # Email domains come from here [http://www.fakemailgenerator.com] to allow for checking emails sent.
            # Emails are formatted as firstname.lastname@domain.com
            email_domains = ["cuvox.de", "jourrapide.com", "armyspy.com", "dayrep.com", "fleckens.hu", "gustr.com", "rhyta.com", "superrito.com"]
            return first.lower()+last.lower()+"@"+random.choice(email_domains)


        def scramble_phone():
            return str(random.randrange(100, 999))+"-"+str(random.randrange(100, 999))+"-"+str(random.randrange(1000, 9999))

        for census in census_data:

            # Get a random last name for our family
            last_name = random.choice(lastNames)

            # Create an address for our family
            address = str(random.randrange(1, 9999))+" "+random.choice(streets)

            attributes = [
                ("employee_first", lambda : random.choice(firstNames)),
                ("employee_last", lambda : last_name),
                ("employee_ssn", lambda : get_ssn()),
                ("employee_email", lambda : scramble_email(census.employee_first, census.employee_last)),
                ("employee_street_address", lambda: address),
                ("employee_phone", lambda: scramble_phone()),
                ("employee_birthdate", lambda: scramble_date(census.employee_birthdate)),

                ("spouse_first", lambda : random.choice(firstNames)),
                ("spouse_last", lambda : last_name),
                ("spouse_ssn", lambda : get_ssn()),
                ("spouse_email", lambda : scramble_email(census.spouse_first, census.spouse_last)),
                ("spouse_street_address", lambda: address),
                ("spouse_phone", lambda: scramble_phone()),
                ("spouse_birthdate", lambda: scramble_date(census.spouse_birthdate)),
            ]

            for enrollment_application in census.enrollment_applications:
                beneficiary_attributes =[
                    ("employee_beneficiary_name", lambda: u"{} {}".format(random.choice(firstNames), random.choice(lastNames))),
                    ("spouse_beneficiary_name", lambda: u"{} {}".format(random.choice(firstNames), random.choice(lastNames))),
                    ("employee_other_owner_name", lambda: u"{} {}".format(random.choice(firstNames), random.choice(lastNames))),
                    ("employee_beneficiary_ssn", lambda: get_ssn()),
                    ("spouse_beneficiary_ssn", lambda: get_ssn()),
                    ("employee_other_owner_ssn", lambda: get_ssn()),
                    ("employee_beneficiary_birthdate", lambda: scramble_date(enrollment_application.employee_beneficiary_birthdate)),
                    ("spouse_beneficiary_birthdate", lambda: scramble_date(enrollment_application.employee_beneficiary_birthdate)),
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
                # Check each child to see if the exists (if there is a first name)
                if getattr(census, "child{}_first".format(x)):
                    # Set new first and last name for the child
                    setattr(census, "child{}_first".format(x), random.choice(firstNames))
                    setattr(census, "child{}_last".format(x), last_name)

                    # Some records may not have birthdates. If not create random date.
                    birthdate = getattr(census, "child{}_birthdate".format(x)) or datetime.datetime.now().replace(year=random.randrange(1995, 2001))
                    setattr(census, "child{}_birthdate".format(x), scramble_date(birthdate))

        db.session.commit()
