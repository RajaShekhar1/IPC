
all_states = map(lambda x: dict(statecode=x[0], name=x[1]), [
    ('AL', 'Alabama'),
    ('AK', 'Alaska'),
    ('AZ', 'Arizona'),
    ('AR', 'Arkansas'),
    ('CA', 'California'),
    ('CO', 'Colorado'),
    ('CT', 'Connecticut'),
    ('DE', 'Delaware'),
    ('DC', 'District of Columbia'),
    ('FL', 'Florida'),
    ('GA', 'Georgia'),
    ('HI', 'Hawaii'),
    ('ID', 'Idaho'),
    ('IL', 'Illinois'),
    ('IN', 'Indiana'),
    ('IA', 'Iowa'),
    ('KS', 'Kansas'),
    ('KY', 'Kentucky'),
    ('LA', 'Louisiana'),
    ('ME', 'Maine'),
    ('MD', 'Maryland'),
    ('MA', 'Massachusetts'),
    ('MI', 'Michigan'),
    ('MN', 'Minnesota'),
    ('MS', 'Mississippi'),
    ('MO', 'Missouri'),
    ('MT', 'Montana'),
    ('NE', 'Nebraska'),
    ('NV', 'Nevada'),
    ('NH', 'New Hampshire'),
    ('NJ', 'New Jersey'),
    ('NM', 'New Mexico'),
    ('NY', 'New York'),
    ('NC', 'North Carolina'),
    ('ND', 'North Dakota'),
    ('OH', 'Ohio'),
    ('OK', 'Oklahoma'),
    ('OR', 'Oregon'),
    ('PA', 'Pennsylvania'),
    ('PR', 'Puerto Rico'),
    ('RI', 'Rhode Island'),
    ('SC', 'South Carolina'),
    ('SD', 'South Dakota'),
    ('TN', 'Tennessee'),
    ('TX', 'Texas'),
    ('UT', 'Utah'),
    ('VT', 'Vermont'),
    ('VA', 'Virginia'),
    ('VI', 'Virgin Islands'),
    ('WA', 'Washington'),
    ('WV', 'West Virginia'),
    ('WI', 'Wisconsin'),
    ('WY', 'Wyoming'),
])

all_statecodes = [s['statecode'] for s in all_states]

states_by_statecode = {s['statecode']: s for s in all_states}

#FPPTI_disabled_statecodes = ['CT', 'DC', 'IN', 'ME', 'MD', 'MA', 'MN', 'NH', 'NJ', 'NY',
#                             'NC', 'ND', 'PR', 'VT', 'WA']
#FPPTI_states = [s for s in all_states if s['statecode'] not in FPPTI_disabled_statecodes]

#FPPCI_disabled_statecodes = ['CT', 'DC', 'ME', 'MD', 'MA', 'MN', 'NH', 'NJ', 'NY', 'NC', 
#                             'ND', 'PA', 'PR', 'VT']
#FPPCI_states = [s for s in all_states if s['statecode'] not in FPPCI_disabled_statecodes]

#FPPTI_generic_states = ["AL", "AK", "AZ", "AR", "CA", "DE", "GA", "HI", "ID", "IA", "KS", "KY", "LA", "MI", "MS", "MT", "NE", "NV", "NM", "OK", "OR", "RI", "SC", "SD", "TN", "TX", "UT", "VI", "WV", "WY"]
#FPPCI_generic_states = ["AL", "AK", "AZ", "AR", "CA", "DE", "GA", "HI", "ID", "IN", "IA", "KS", "KY", "LA", "MI", "MS", "MT", "NE", "NV", "NM", "OK", "OR", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VI", "WV", "WY"]

#GROUPCI_STATES = [
#    s for s in all_states if s['statecode'] in ['IN']
#]

#FPPGOV_STATES = [
#    s for s in all_states if s['statecode'] in ['IN', 'MN']
#]



def get_all_states():
    return all_states