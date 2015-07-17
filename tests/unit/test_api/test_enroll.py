
# 1. authenticate
#  - check URL parameters for an API user_token
#    - flag as a 3rd party enrollment import
#  - check for logged-in-agent
#  - check for self-enroll token (move to URL parameter)
# 2. determine which case we are importing to
#  - if authenticated via API token,
#     case_token must be in either the URL params, otherwise require it on every record
#     case_id may not be used
#  - if authenticated via login, case_id must be provided in the URL parameters
#  - if authenticated via self-enroll token, case is retrieved from the self enroll setup
# 3. normalize data format
#  - use content type header probably - text/plain for flat-file, text/csv for csv, or application/json
#  - convert flat-file to CSV
#  - parse CSV to list-of-dicts
#  - convert JSON to CSV list-of-dict format if it needs to be converted from wizard,
#    otherwise leave as list of dicts
#    - how to determine if from wizard?
# 4. validate data
#  - configure the validator with whether or not the case_token is required
#  - if any submitted enrollment data fails validation, we fail the whole thing
#  - MAYBE skip validation if logged in or self enroll, but would prefer not to skip if possible.
#  - TODO: do we accept declines from 3rd party enrollment, if so define did_decline column
# 5. save enrollment data
#  - same as now, except also save raw JSON data
# 6. submit
#  - Batch this part if this is not a wizard enrollment
#  - TODO: need to update the docusign service code to expect the new column names
#  - If this is a 3rd party import, the docusign submission is changed by:
#     - removing the employee and agent as recipients
#     - generate the server templates locally instead of via DocuSign
#  - Otherwise, submit to DocuSign normally (with employee and agent recipients)
#  - Soon, will submit to Dell XML
# 7. return response
#  - If 3rd party import, we return success with # records processed, or errors.
#    - If this came from the dropbox, we need to reply via email
#    - otherwise we return JSON
#  - If wizard, then we also include a redirect URL for DocuSign.

import unittest2

from mock import Mock, sentinel, patch

from taa import app as taa_app
from taa.services.enrollments.enrollment_import_processor import EnrollmentProcessor

class TestEnrollmentProcessor(unittest2.TestCase):

    def setUp(self):
        taa_app.config['TESTING'] = True
        self.app = taa_app.test_client()
        self.enrollment_processor = EnrollmentProcessor()

    def tearDown(self):
        pass

    def test_authenticate_with_token(self):


        self.enrollment_processor.authenticate(token=sentinel.token)

