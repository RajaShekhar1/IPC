Feature: Submit enrollments using the API
  In order to allow integration with 3rd party enrollment processing kiosks
  As a 3rd party user
  I want to submit enrollments using the TAA API
    # 1. authenticate
    #  + check URL parameters for an API user_token
    #    - flag as a 3rd party enrollment import
    #  - check for logged-in API user
    #  - check for self-enroll token (move to URL parameter)
    # 2. determine which case we are importing to
    #  + if authenticated via API token,
    #     case_token must be in either the URL params, otherwise require it on every record
    #     case_id may not be used
    #  - if authenticated via login, case_id must be provided in the URL parameters
    #  - if authenticated via self-enroll token, case is retrieved from the self enroll setup

    # - IF CASE IS NOT ENROLLING, reject with an error
    # 3. normalize data format
    #  - use content type header probably - text/plain for flat-file, text/csv for csv, or application/json
    #  - convert flat-file to CSV
    #  + parse CSV to list-of-dicts
    #  - leave JSON as list-of-dicts
    #
    # 4. validate data
    #  - configure the validator with whether or not the case_token is required
    #  + if any submitted enrollment data fails validation, we fail the whole thing
    #  - MAYBE skip validation if logged in or self enroll, but would prefer not to skip if possible.
    #  - TODO: do we accept declines from 3rd party enrollment, if so define did_decline column
    # 5. standardize data and save enrollment data
    #  - same as now, except also save raw JSON data
    # 6. submit
    #  - Batch this part if this is not a wizard enrollment
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

  Background:
    Given I have a user named BHI with token USER-123 in group api_users
    Given I have a case that is enrolling with an api token 'CASE-123' and self-enroll token 'SE-123'

  Scenario: It should return a success response with a valid CSV enrollment file.
    Given I create a minimally valid CSV file with case_token 'CASE-123'
    When I submit the enrollment data to the API using the auth_token 'USER-123' and case_token ' '
    Then I should see a 200 response

  Scenario: It should return the number of records processed when successful.
    Given I create a minimally valid CSV file with case_token 'CASE-123'
    When I submit the enrollment data to the API using the auth_token 'USER-123' and case_token ' '
    Then I should see a positive number of records processed in the result

  Scenario: It should reject an unauthenticated request with an Unauthorized error.
    Given I create a minimally valid CSV file with case_token 'CASE-123'
    When I submit the enrollment data to the API using the auth_token ' ' and case_token ' '
    Then I should see a 401 response

  Scenario: It should reject an invalid auth token.
    Given I create a minimally valid CSV file with case_token 'CASE-123'
    When I submit the enrollment data to the API using the auth_token 'FAKE' and case_token ' '
    Then I should see a 401 response

  Scenario: It should verify that the case is currently enrolling.
    Given I deactivate the case with token 'CASE-123'
    Given I create a minimally valid CSV file with case_token 'CASE-123'
    When I submit the enrollment data to the API using the auth_token 'USER-123' and case_token 'CASE-123'
    Then I should see a 400 response


  Scenario: It should save the enrollment data to the enrollment records table.
    Given I create a minimally valid CSV file with case_token 'CASE-123'
    When I submit the enrollment data to the API using the auth_token 'USER-123' and case_token 'CASE-123'
    Then I should see a 200 response
    And I should see an enrollment record in the database with the following data
      | signature_time      | payment_mode |
      | 2015-01-01 10:30:00 | 12           |


  Scenario: It should process a flat file format.
    Given I create a minimally valid flat-file with case_token 'CASE-123'
    When I submit the enrollment data to the API using the auth_token 'USER-123' and case_token 'CASE-123'
    Then I should see a 200 response


#  Scenario: It should allow a logged-in api user to submit an enrollment
#    Given I log in as the user 'BHI'
#    Given I create a minimally valid CSV file with case_token 'CASE-123'
#    When I submit the enrollment data to the API using the auth_token ' ' and case_token ' '
#    Then I should see a 200 response

#  Scenario: It should look up the case by token if the case token is passed in the params.
#    Given I create a minimally valid CSV file with case_token 'CASE-123'
#    When I submit the enrollment data to the API using the auth_token 'USER-123' and case_token 'CASE-123'
#    Then It should look up the case with token 'CASE-123' in the database
#
#  Scenario: It should use the case token in the file for each record if no case token is passed in the params.
#    Given I create a minimally valid CSV file with case_token 'CASE-123'
#    When I submit the enrollment data to the API using the auth_token 'USER-123' and case_token ' '
#    Then It should look up the case with token 'CASE-123' in the database

  #  - use data_format parameter  - flat for flat-file, csv for csv, or json
  #  - convert flat-file to CSV
  #  + parse CSV to list-of-dicts
  #  - leave JSON as list-of-dicts

  # save enrollment data
    # + first convert to wizard format
    #  - modify enrollment_service.save_enrollment_data() to save the raw data in a new column



  # if there are errors
  #  - if it's from the dropbox, determine the email and send email errors
  #  - otherwise return json errors
  # if this is a 3rd party import (From the FTP dropbox or the agent upload tab), we create a new celery task that processes the batches
    # - Task will take an enrollment record id
    # -
