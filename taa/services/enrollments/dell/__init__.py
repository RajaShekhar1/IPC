# -*- coding: utf-8 -*-

ENDPOINT = {
    'prod': 'https://extranetapps.tagtpa.com/TxLifeImport/TxLife.asmx',
    'dev': 'https://extranetapps-mo.tagtpa.com/TxLifeImport/TxLife.asmx',
}

# TxLife.TxLifeResponse.TransResult.ResultCode
RESULT_CODES = {
    '1': 'Success',
    '90001': 'TxLife.OLifEExtension.CompanyNumber is missing',
    '90031': 'TxLife.TXLifeRequest.TransRefGUID is missing',
    '90032': 'TxLife.TXLifeRequest.TransRefGUID is duplicate',
    '99001': 'TxLife.OLifEExtension.CompanyNumber is invalid',
    '99003': 'XML does not pass well-formed check',
    '99099': 'Application form is missing',
    '99101': 'File extension missing for one or more forms',
    '99102': 'One or more forms is not correctly encoded',
    '99103': 'Dell internal network error',
    '': 'Other application error',
}
# In the Response:
# /TxLife/TxLifeResponse/TransRefGUID
# should be equal to
# /TxLife/TXLifeRequest/TransRefGUID
# /TxLife/TxLifeResponse/TransResult/ResultCode[@tc = 1]
# Signifies the item was successfully processed.  So the TransRefGUID 	used in this transmission should not be used again.
# Any @tc value should be regarded as an error.  The transmission may be 	sent again at a later time.

GENDER = {
    'male': '1',
    'female': '2'
}

YESNO = {
    None: '0',
    'no': '0',
    'yes': '1'
}

BOOLEAN = {
    None: '0',
    False: '0',
    True: '1'
}

PAYMENT_MODE = {
    '1': '1',    # Annual
    '2': '2',    # Semiannual
    '4': '3',    # Quarterly
    '12': '4',   # Monthly
    '24': '5',   # Semimonthly
    '52': '6',   # Weekly
    '26': '7',   # Biweekly
    '13': '12',  # Every 4 weeks (4 weekly)
}

VENDOR_NAME = '5Star'      # TODO: GET FROM DELL
VENDOR_CODE = '9999'       # TODO: GET FROM DELL
AUTH_LOGIN = '5Star'
CRYPT_TYPE = 'NONE'        # TODO: GET FROM DELL
COMPANY_NUMBERS = {
    'production': '67',    # 5Star Production
    'test': '87',          # 5Star Model Office
}
# PRODUCT_CODES = {
#     'Term',
#     'Critical Illness',
# }
PLAN_NAMES = {
    'VOL01',
    'BAS01',
    'VOL02',
    'BAS02',
    'FPPTI',
    'FPPTI/MD',
    'FPPTIA',
    'FPPTI/UT',
    'INDFPP',
    'INDFPP/MD',
    'INDFPP/UT',
    'GRPFRE',
    'CRTIL',
    'CRTILE'
}
POLICY_STATUS = 'Proposed'
POLICY_STATUS_CODE = '12'
CARRIER_CODE = '5STAR'
ENROLLEE_TYPES = {
    'new': 'New Enrollee',
    'late': 'Late Enrollee',
}
PAYMENT_MODES = {
    '1': 'Annual',
    '2': 'Semiannual',
    '3': 'Quarterly',
    '4': 'Monthly',
    '12': 'Every 4 weeks',
    '5': 'Semi-Monthly',
    '7': 'Bi-Weekly',
    '6': 'Weekly',
}
PAYMENT_METHODS = {
    '2': 'Regular Billing',
    '5': 'Group Bill',
    '7': 'Electronic Funds Transfer',
}
PAYMENT_ACCOUNT_TYPES = {
    '1': 'Savings Account',
    '2': 'Checking Account',
}
REPLACEMENT_TYPES = {
    None: {'code': '1', 'name': 'None'},
    'internal': {'code': '2', 'name': 'Internal'},
    'external': {'code': '3', 'name': 'External'},
}
TRANS_TYPE_CODE = '103'
TRANS_TYPE = 'New Business Submission'
RELATIONSHIP_ROLES = {
    'As Approved': '77',
    'Aunt': '100001',
    'Brother': '100002',
    'Business': '156',
    'Child': '2',
    'Civil Union': '100003',
    'Corporation': '100004',
    'Custodian': '57',
    'Domestic Partner': '15',
    'Employee': '6',
    'Employer': '7',
    'Estate': '100005',
    'Ex-spouse': '100006',
    'Father': '100007',
    'Father-in-law': '100008',
    'Fiancé': '111',
    'Grandchild': '93',
    'Grandparent': '92',
    'Guardian': '27',
    'Mother': '100009',
    'Mother-in-law': '100010',
    'Nephew': '100011',
    'Niece': '100012',
    'Other': '2147483647',
    'Parent': '3',
    'Self': '168',
    'Sibling': '4',
    'Sister': '100013',
    'Spouse': '1',
    'Trust': '100014',
    'Uncle': '100015',
}
QUALIFYING_PLAN_TYPES = {
    '1': 'Non-Qualified',
}
COVERAGE_INDICATOR_CODES = {
    '1': 'Base',
}
PARTICIPANT_ROLE_CODES = {
    '1': 'Primary Insured',
    '18': 'Owner',
    '12': 'Payor',
    '15': 'Primary Agent',
    '7': 'Beneficiary - Primary',
    '9': 'Beneficiary - Contingent',
    '5': 'Spouse',
    '4': 'Child',
    '31': 'Third Party Recipient',
}
RIDER_CODES = {
    'employee': 'CRTILE',
    'spouse': 'CRTIL/SP',
    'child': 'CRTIL/CH',
}
GENDER_CODES = {
    'male': {'code': 1, 'name': 'Male'},
    'female': {'code': 2, 'name': 'Female'},
}

# From 'STP Data Map (5 Star 67).xls'
# Sheet 'OLI_LU_STATE LifeSys Mapping'
STATE_CODES = {
    'AL': {'code': 1, 'name': 'Alabama'},
    'AK': {'code': 2, 'name': 'Alaska'},
    'AS': {'code': 3, 'name': 'American Samoa'},
    'AZ': {'code': 4, 'name': 'Arizona'},
    'AR': {'code': 5, 'name': 'Arkansas'},
    'CA': {'code': 6, 'name': 'California'},
    'CO': {'code': 7, 'name': 'Colorado'},
    'CT': {'code': 8, 'name': 'Connecticut'},
    'DE': {'code': 9, 'name': 'Delaware'},
    'DC': {'code': 10, 'name': 'District of Columbia'},
    'FM': {'code': 11, 'name': 'Federated States of Micronesia'},
    'FL': {'code': 12, 'name': 'Florida'},
    'GA': {'code': 13, 'name': 'Georgia'},
    'GU': {'code': 14, 'name': 'Guam'},
    'HI': {'code': 15, 'name': 'Hawaii'},
    'ID': {'code': 16, 'name': 'Idaho'},
    'IL': {'code': 17, 'name': 'Illinois'},
    'IN': {'code': 18, 'name': 'Indiana'},
    'IA': {'code': 19, 'name': 'Iowa'},
    'KS': {'code': 20, 'name': 'Kansas'},
    'KY': {'code': 21, 'name': 'Kentucky'},
    'LA': {'code': 22, 'name': 'Louisiana'},
    'ME': {'code': 23, 'name': 'Maine'},
    'MH': {'code': 24, 'name': 'Marshall Islands'},
    'MD': {'code': 25, 'name': 'Maryland'},
    'MA': {'code': 26, 'name': 'Massachusetts'},
    'MI': {'code': 27, 'name': 'Michigan'},
    'MN': {'code': 28, 'name': 'Minnesota'},
    'MS': {'code': 29, 'name': 'Mississippi'},
    'MO': {'code': 30, 'name': 'Missouri'},
    'MT': {'code': 31, 'name': 'Montana'},
    'NE': {'code': 32, 'name': 'Nebraska'},
    'NV': {'code': 33, 'name': 'Nevada'},
    'NH': {'code': 34, 'name': 'New Hampshire'},
    'NJ': {'code': 35, 'name': 'New Jersey'},
    'NM': {'code': 36, 'name': 'New Mexico'},
    'NY': {'code': 37, 'name': 'New York'},
    'NC': {'code': 38, 'name': 'North Carolina'},
    'ND': {'code': 39, 'name': 'North Dakota'},
    'MP': {'code': 40, 'name': 'Northern Mariana Islands'},
    'OH': {'code': 41, 'name': 'Ohio'},
    'OK': {'code': 42, 'name': 'Oklahoma'},
    'OR': {'code': 43, 'name': 'Oregon'},
    'PW': {'code': 44, 'name': 'Palau Island'},
    'PA': {'code': 45, 'name': 'Pennsylvania'},
    'PR': {'code': 46, 'name': 'Puerto Rico'},
    'RI': {'code': 47, 'name': 'Rhode Island'},
    'SC': {'code': 48, 'name': 'South Carolina'},
    'SD': {'code': 49, 'name': 'South Dakota'},
    'TN': {'code': 50, 'name': 'Tennessee'},
    'TX': {'code': 51, 'name': 'Texas'},
    'UT': {'code': 52, 'name': 'Utah'},
    'VT': {'code': 53, 'name': 'Vermont'},
    'VI': {'code': 54, 'name': 'Virgin Islands'},
    'VA': {'code': 55, 'name': 'Virginia'},
    'WA': {'code': 56, 'name': 'Washington'},
    'WV': {'code': 57, 'name': 'West Virginia'},
    'WI': {'code': 58, 'name': 'Wisconsin'},
    'WY': {'code': 59, 'name': 'Wyoming'},
    'AA': {'code': 60, 'name': 'Armed Forces Americas (except Canada)'},
    'AE': {'code': 61, 'name': 'Armed Forces Canada, Africa, Europe, Middle East'},
    'AP': {'code': 62, 'name': 'US Armed Forces Pacific'},
    'AB': {'code': 101, 'name': 'Alberta'},
    'BC': {'code': 102, 'name': 'British Columbia'},
    'MB': {'code': 103, 'name': 'Manitoba'},
    'NB': {'code': 104, 'name': 'New Brunswick'},
    'NL': {'code': 105, 'name': 'AKA NL Newfoundland and Labrador'},
    'NT': {'code': 106, 'name': 'Northwest Territories'},
    'NS': {'code': 107, 'name': 'Nova Scotia'},
    'ON': {'code': 108, 'name': 'Ontario'},
    'PE': {'code': 109, 'name': 'Prince Edward Island'},
    'QC': {'code': 110, 'name': 'Quebec'},
    'NU': {'code': 113, 'name': 'Nunavut'},
    'SK': {'code': 111, 'name': 'Saskatchewan'},
    'YT': {'code': 112, 'name': 'Yukon Territory'},
    'Other': {'code': 2147483647, 'name': '99'},
    'Unknown': {'code': 0, 'name': '99'},
}

SMOKER_CODES = {
    False: {'code': '1', 'name': 'Never used tobacco in any form.'},
    True: {'code': '3', 'name': 'Smoker'},
}

