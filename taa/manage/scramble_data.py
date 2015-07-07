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
    """Scrable all data in the database to not contain real information"""

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

        # Create a range of "social security numbers" and select the number we need for the census data.
        ssn_range = xrange(100000000, 999999999)
        # Sampling ensures that the data is unique
        ssns = random.sample(ssn_range, len(census_data)*2);

        def scramble_date(date):
            new_month = random.randrange(1,13)
            days_in_month = calendar.monthrange(date.year, new_month)
            new_day = random.randrange(1, days_in_month[1]+1)
            return date.replace(day=new_day, month=new_month)

        def scramble_email(first, last):
            # Email domains come from here [http://www.fakemailgenerator.com] to allow for checking emails sent.
            # Emails are formatted as firstname.lastname@domain.com
            email_domains = ["cuvox.de", "jourrapide.com", "armyspy.com", "dayrep.com", "fleckens.hu", "gustr.com", "rhyta.com", "superrito.com"]
            return first.lower()+"."+last.lower()+"@"+random.choice(email_domains)


        def scramble_phone():
            return str(random.randrange(100, 999))+"-"+str(random.randrange(100, 999))+"-"+str(random.randrange(1000, 9990))

        for census in census_data:

            # Get a random last name for our family
            last_name = random.choice(lastNames)

            # Create an address for our family
            address = str(random.randrange(1, 9999))+" "+random.choice(streets)



            # Set employee information
            census.employee_first = random.choice(firstNames)
            census.employee_last = last_name

            census.employee_ssn = ssns.pop()

            census.employee_email = scramble_email(census.employee_first, census.employee_last)
            census.employee_street_address = address

            census.employee_birthdate = scramble_date(census.employee_birthdate)
            census.employee_phone = scramble_phone()



            # Set spouse information
            census.spouse_first = random.choice(firstNames)
            census.spouse_last = last_name

            census.spouse_ssn = ssns.pop()

            census.spouse_email = scramble_email(census.spouse_first, census.spouse_last)
            census.spouse_street_address = address

            census.spouse_birthdate = scramble_date(census.spouse_birthdate)
            census.spouse_phone = scramble_phone()

            # Six possible children in a record
            for x in range(1, 6+1):
                # Check each child to see if the exists (if there is a first name)
                if getattr(census,"child"+str(x)+"_first"):
                    # Set new first and last name for the child
                    setattr(census, "child"+str(x)+"_first", random.choice(firstNames))
                    setattr(census, "child"+str(x)+"_last", last_name)

                    # Some records may not have birthdates. If not create random date.
                    birthdate = getattr(census, "child"+str(x)+"_birthdate") or datetime.datetime.now().replace(year=random.randrange(1995, 2001))
                    setattr(census, "child_"+str(x)+"_birthdate", scramble_date(birthdate))

        db.session.commit()
