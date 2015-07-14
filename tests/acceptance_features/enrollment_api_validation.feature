Feature: Validate an enrollment record submitted via API.
  In order to submit enrollments via the TAA API
  As an API User
  I want to receive feedback if my submission had any errors, and if so, what the errors are.

Background:
  Given I have an API User named BHI with token ABC
  Given I have a Case with the token XYZ
  Given The following are valid product codes
    | product_code|
    | FPPTI       |
    | FPPCI       |

Scenario: Submit an enrollment with the minimal data needed to validate.
  Given I prepare an enrollment file with data
    | user_token | case_token | product_code | payment_mode |
    | ABC        | XYZ        | FPPTI        | weekly       |
  And I add the following enrollment data columns
    | emp_first | emp_last | emp_birthdate | emp_ssn     | emp_coverage | emp_premium |
    | Joe       | Johnson  | 01/01/1990    | 123-12-1234 | 50000        | 10.00       |
  And I add the following enrollment data columns
    | emp_street | emp_street2 | emp_city | emp_state | emp_zipcode | emp_phone |
    | 123 Sesame |             | Lansing  | MI        | 12345       |           |
  And I add the following enrollment data columns
    | emp_pin  | emp_sig_txt  | application_date | time_stamp         | signed_at_city | signed_at_state |
    | 12341234 | esign by Joe | 01/01/2015       | 01/01/2015T10:01:00 | Lansing        | MI              |
  And I add the following enrollment data columns
    | agent_name | agent_code | agent_sig_txt       |
    | Andy Agent | 26ABC      | esign by Andy Agent |

  When I submit the file to the Enrollment API
  Then I should see a success response


Scenario: Submit an enrollment that is missing some basic headers which are always required.
  Given I prepare an enrollment file with data
    | bogus_header    |
    | bogus data      |
  When I submit the file to the Enrollment API
  Then I should see the following errors in the response
    | error_type     | error_field      |
    | missing_header | user_token       |
    | missing_header | case_token       |
    | missing_header | product_code     |
    | missing_header | emp_first        |
    | missing_header | emp_last         |
    | missing_header | emp_gender       |
    | missing_header | emp_birthdate    |
    | missing_header | emp_ssn          |
    | missing_header | emp_coverage     |
    | missing_header | emp_premium      |
    | missing_header | emp_street       |
    | missing_header | emp_street2      |
    | missing_header | emp_city         |
    | missing_header | emp_state        |
    | missing_header | emp_zipcode      |
    | missing_header | agent_name       |
    | missing_header | agent_code       |
    | missing_header | agent_sig_txt    |
    | missing_header | employee_pin     |
    | missing_header | employee_sig_txt |
    | missing_header | application_date |
    | missing_header | time_stamp       |
    | missing_header | signed_at_city   |
    | missing_header | signed_at_state  |


Scenario: A user submits an enrollment with an invalid token
  Given I prepare an enrollment file with basic valid enrollment data
  But I substitute 'BOGUS' for the column 'user_token'
  When I submit the file to the Enrollment API
  Then I should see the following errors in the response
  | error_type    | error_field |
  | invalid_token | user_token  |


Scenario: Users submits an invalid product code
  Given I prepare an enrollment file with basic valid enrollment data
  But I substitute 'MY_BAD_PRODUCT_CODE' for the column 'product_code'
  When I submit the file to the Enrollment API
  Then I should see the following errors in the response
    | error_type      | error_field  |
    | invalid_product | product_code |


Scenario Outline: A user submits a file with invalid data types.
  Given I prepare an enrollment file with basic valid enrollment data
  But I substitute '<bad_value>' for the column '<column_name>'
  When I submit the file to the Enrollment API
  Then I should see the following errors in the response
    | error_type   | error_field   |
    | <error_type> | <error_field> |

Examples:
  | column_name   | bad_value     | error_type       | error_field   |
  | emp_ssn       | ABC123        | invalid_ssn      | emp_ssn       |
  | emp_birthdate | Oct 1st       | invalid_date     | emp_birthdate |
  | emp_coverage  | 33,000        | invalid_coverage | emp_coverage  |
  | emp_premium   | $10.00        | invalid_premium  | emp_premium   |
  | payment_mode  | hourly        | invalid_mode     | payment_mode  |


Scenario Outline: A user submits a file with missing data for required columns.
  Given I prepare an enrollment file with basic valid enrollment data
  But I clear the data on column '<column_name>'
  When I submit the file to the Enrollment API
  Then I should see the following errors in the response
    | error_type    | error_field   |
    | missing_data  | <column_name> |

Examples:
  | column_name   |
  | user_token    |
  | case_token    |
  | product_code  |
  | emp_first     |
  | emp_last      |
  | emp_ssn       |
  | emp_birthdate |
  | payment_mode  |
  | agent_name    |
  | agent_code    |
  | employee_pin  |


Scenario: User submits spouse data with the enrollment.
  Given I prepare an enrollment file with basic valid enrollment data
  And I add the following enrollment data columns
    | sp_first | sp_last | sp_birthdate | sp_ssn      | sp_coverage | sp_premium |
    | Jane     | Johnson | 01/01/1989   | 123-33-4444 | 10000       | 3.00       |
  When I submit the file to the Enrollment API
  Then I should see a success response


Scenario Outline: User submits spouse data with missing data that is only required when some spouse data is present.
  Given I prepare an enrollment file with basic valid enrollment data
  And I add the following enrollment data columns
    | sp_first   | sp_last   | sp_birthdate   | sp_ssn   | sp_coverage | sp_premium |
    | <sp_first> | <sp_last> | <sp_birthdate> | <sp_ssn> | 10000       | 3.00       |
  When I submit the file to the Enrollment API
  Then I should see the following errors in the response
    | error_type   | error_field   |
    | <error_type> | <error_field> |

Examples:
  | sp_first | sp_last | sp_birthdate | sp_ssn | error_type   | error_field |
  | Jane     | Doe     | 1990-01-01   |             | missing_data | sp_ssn       |
  | Jane     | Doe     |              | 123-12-1234 | missing_data | sp_birthdate |
  | Jane     |         | 1990-01-01   | 123-12-1234 | missing_data | sp_last      |
  |          | Doe     | 1990-01-01   | 123-12-1234 | missing_data | sp_first     |


Scenario: User submits child data with the enrollment.
  Given I prepare an enrollment file with basic valid enrollment data
  And I add the following enrollment data columns
    | ch1_first | ch1_last | ch1_birthdate | ch1_ssn | ch1_coverage | ch1_premium |
    | Johnny    | Doe      | 2010-02-02    |         |              |             |
  When I submit the file to the Enrollment API
  Then I should see a success response

Scenario: User submits a second child's data with the enrollment.
  Given I prepare an enrollment file with basic valid enrollment data
  And I add the following enrollment data columns
    | ch1_first | ch1_last | ch1_birthdate | ch1_ssn | ch1_coverage | ch1_premium |
    | Johnny    | Doe      | 2010-02-02    |         |              |             |
  And I add the following enrollment data columns
    | ch2_first | ch1_last | ch1_birthdate | ch1_ssn | ch1_coverage | ch1_premium |
    | Susie      | Doe      | 2011-03-03    |         |              |             |
  When I submit the file to the Enrollment API
  Then I should see a success response

Scenario Outline: User submits coverage without premium and vice versa
  Given I prepare an enrollment file with basic valid enrollment data
  And I add valid spouse enrollment data
  And I add valid child enrollment data
  But I substitute <bad_value> for the column '<column_name>'
  When I submit the file to the Enrollment API
  Then I should see the following errors in the response
    | error_type   | error_field   |
    | <error_type> | <column_name> |

Examples:
  | bad_value | column_name  | error_type   |
  | ' '       | emp_coverage | missing_data |
  | ' '       | emp_premium  | missing_data |
  | ' '       | sp_coverage  | missing_data |
  | ' '       | sp_premium   | missing_data |
  | ' '       | ch_coverage  | missing_data |
  | ' '       | ch_premium   | missing_data |



