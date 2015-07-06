from flask import current_app
from flask_script import Command, prompt, prompt_pass
from werkzeug.datastructures import MultiDict

import os
import random
import urllib, json
import calendar, datetime

from ..services.cases.models import CaseCensus
from ..models import db

nameFile = open(os.path.dirname(os.path.realpath(__file__))+'/names.txt', 'r')
names = nameFile.read()
names = names.splitlines()
random.shuffle(names)
firstNames = [n.split(" ")[0] for n in names]
lastNames = [n.split(" ")[1] for n in names]

streetsApi = "https://www.randomlists.com/data/streets.json"
streetsResponse = urllib.urlopen(streetsApi)

streets = json.load(streetsResponse)["data"]

# citiesApi = "https://www.randomlists.com/data/us-cities.json"
# citiesResponse = urllib.urlopen(citiesApi)

# cities = json.load(citiesResponse)["data"]

# statesApi = "https://www.randomlists.com/data/states.json"
# statesResponse = urllib.urlopen(statesApi)

# states = json.load(statesResponse)["data"]

# Email domains come from here [http://www.fakemailgenerator.com] to allow for checking emails sent.
# Emails are formatted as firstname.lastname@domain.com
email_domains = ["cuvox.de", "jourrapide.com", "armyspy.com", "dayrep.com", "fleckens.hu", "gustr.com", "rhyta.com", "superrito.com"]

class ScrambleDataCommand(Command):
    """Scrable all data in the database to not contain real information"""

    def run(self):
        census_data_query = db.session.query(CaseCensus)
        record_count = census_data_query.count()
        census_data = census_data_query.all()
        ssn_range = xrange(100000000, 999999999)
        ssns = random.sample(ssn_range, record_count*3);
        print streets
        for census in census_data:
            emp_first_name = random.choice(firstNames)
            sp_first_name = random.choice(firstNames)
            last_name = random.choice(lastNames)

            address = str(random.randrange(1, 9999))+" "+random.choice(streets)

            census.employee_first=emp_first_name
            census.employee_last=last_name
            census.employee_ssn=ssns.pop()
            census.employee_email=emp_first_name.lower()+"."+last_name.lower()+"@"+random.choice(email_domains)
            census.employee_street_address = address
            new_month = random.randrange(1,13)
            days_in_month = calendar.monthrange(census.employee_birthdate.year, new_month)
            new_day = random.randrange(1, days_in_month[1]+1)
            census.employee_birthdate = census.employee_birthdate.replace(day=new_day, month=new_month)
            census.employee_phone = str(random.randrange(100, 999))+"-"+str(random.randrange(100, 999))+"-"+str(random.randrange(1000, 9990))

            census.spouse_first = sp_first_name
            census.spouse_last= last_name
            census.spouse_ssn=ssns.pop()
            census.spouse_email=sp_first_name.lower()+"."+last_name.lower()+"@"+random.choice(email_domains)
            census.spouse_street_address = address
            new_month = random.randrange(1,13)
            days_in_month = calendar.monthrange(census.spouse_birthdate.year, new_month)
            new_day = random.randrange(1, days_in_month[1]+1)
            census.spouse_birthdate = census.spouse_birthdate.replace(day=new_day, month=new_month)
            census.spouse_phone = str(random.randrange(100, 999))+"-"+str(random.randrange(100, 999))+"-"+str(random.randrange(1000, 9990))

            for x in range(1, 6+1):
                if getattr(census,"child"+str(x)+"_first"):
                    ch_first_name = random.choice(firstNames)
                    setattr(census, "child"+str(x)+"_first", ch_first_name)
                    setattr(census, "child"+str(x)+"_last", last_name)
                    birthdate = getattr(census, "child"+str(x)+"_birthdate")
                    if birthdate:
                        new_month = random.randrange(1,13)
                        days_in_month = calendar.monthrange(birthdate.year, new_month)
                        new_day = day = random.randrange(1, days_in_month[1]+1)
                        setattr(census, "child_"+str(x)+"_birthdate", birthdate.replace(day=new_day, month=new_month))


        db.session.commit()
