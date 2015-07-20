Feature: Validate an enrollment record submitted via API.
  In order to submit enrollments via the TAA API
  As an API User
  I want to receive feedback if my submission had any errors, and if so, what the errors are.

  Background:
    Given I have an API User named BHI with token ABC
    Given I have a Case with the token XYZ
    Given The following are valid product codes
      | product_code |
      | FPPTI        |
      | FPPCI        |

  Scenario: Submit an enrollment with the minimal data needed to validate.
    Given I prepare an enrollment file with data
      | user_token | case_token | product_code | payment_mode |
      | ABC        | XYZ        | FPPTI        | weekly       |
    And I add the following enrollment data columns
      | emp_first | emp_last | emp_birthdate | emp_ssn     | emp_coverage | emp_premium | emp_gender |
      | Joe       | Johnson  | 1990-01-01    | 123-12-1234 | 50000        | 10.00       | m          |
    And I add the following enrollment data columns
      | emp_street | emp_street2 | emp_city | emp_state | emp_zipcode | emp_phone |
      | 123 Sesame |             | Lansing  | MI        | 12345       |           |
    And I add the following enrollment data columns
      | emp_pin  | emp_sig_txt  | application_date | time_stamp          | signed_at_city | signed_at_state |
      | 12341234 | esign by Joe | 2015-01-01       | 2015-01-01 10:01:00 | Lansing        | MI              |
    And I add the following enrollment data columns
      | agent_name | agent_code | agent_sig_txt       |
      | Andy Agent | 26ABC      | esign by Andy Agent |

    When I submit the file to the Enrollment API
    Then I should see a success response

  Scenario: Submit an enrollment that is missing some basic headers which are always required.
    Given I prepare an enrollment file with data
      | bogus_header |
      | bogus data   |
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type     | error_field      |
      | missing_header | user_token       |
      | missing_header | case_token       |
      | missing_header | product_code     |
      | missing_header | payment_mode     |
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
      | missing_header | emp_pin          |
      | missing_header | emp_sig_txt      |
      | missing_header | application_date |
      | missing_header | time_stamp       |
      | missing_header | signed_at_city   |
      | missing_header | signed_at_state  |


  Scenario: A user submits an enrollment with an invalid api token
    Given I prepare an enrollment file with basic valid enrollment data
    But I substitute 'BOGUS' for the column 'user_token'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type    | error_field |
      | invalid_token | user_token  |


  Scenario: A user submits an enrollment with an invalid case token
    Given I prepare an enrollment file with basic valid enrollment data
    But I substitute 'BOGUS' for the column 'case_token'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type    | error_field |
      | invalid_token | case_token  |



  Scenario: User submits an invalid product code
    Given I prepare an enrollment file with basic valid enrollment data
    But I substitute 'MY_BAD_PRODUCT_CODE' for the column 'product_code'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type      | error_field  |
      | invalid_product | product_code |


  Scenario Outline: User submits valid payment modes
    Given I prepare an enrollment file with basic valid enrollment data
    And I substitute '<val>' for the column 'payment_mode'
    When I submit the file to the Enrollment API
    Then I should see a success response
    Examples:
      | val         |
      | weekly      |
      | biweekly    |
      | semimonthly |
      | monthly     |

  Scenario Outline: A user submits a file with invalid data types.
    Given I prepare an enrollment file with basic valid enrollment data
    But I substitute '<bad_value>' for the column '<column_name>'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type   | error_field   |
      | <error_type> | <error_field> |

    Examples:
      | column_name   | bad_value | error_type       | error_field   |
      | emp_ssn       | ABC123    | invalid_ssn      | emp_ssn       |
      # Dates must be ISO-8601-ish. Try pyiso8601 maybe?
      | emp_birthdate | Jan 1st   | invalid_date     | emp_birthdate |
      | emp_coverage  | 33,000k   | invalid_coverage | emp_coverage  |
      # No negative coverage
      | emp_coverage  | -10       | invalid_coverage | emp_coverage  |
      # No dollar sign
      | emp_premium   | $10.00    | invalid_premium  | emp_premium   |
      # No negative premiums
      | emp_coverage  | -10.00    | invalid_coverage | emp_coverage  |
      # Must have exactly 2 decimal places
      | emp_premium   | 10.000    | invalid_premium  | emp_premium   |
      | payment_mode  | hourly    | invalid_mode     | payment_mode  |
      # Must be 'm' or 'f' (case-insensitive)
      | emp_gender    | test      | invalid_gender   | emp_gender    |
      # Must be in our statecode list (case insensitive)
      | emp_state     | ZZ        | invalid_state    | emp_state     |
      # Must be at least 5 digits (may be longer)
      | emp_zipcode   | 1234      | invalid_zip      | emp_zipcode   |

  Scenario Outline: A user submits a file with missing data for required columns.
    Given I prepare an enrollment file with basic valid enrollment data
    But I clear the data on column '<column_name>'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type   | error_field   |
      | missing_data | <column_name> |

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
      | emp_pin       |


  Scenario: User submits spouse data with the enrollment.
    Given I prepare an enrollment file with basic valid enrollment data
    And I add the following enrollment data columns
      | sp_first | sp_last | sp_birthdate | sp_ssn      | sp_coverage | sp_premium |
      | Jane     | Johnson | 1989-01-01   | 123-33-4444 | 10000       | 3.00       |
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
      | sp_first | sp_last | sp_birthdate | sp_ssn      | error_type   | error_field  |
      | Jane     | Doe     | 1990-01-01   |             | missing_data | sp_ssn       |
      | Jane     | Doe     |              | 123-12-1234 | missing_data | sp_birthdate |
      | Jane     |         | 1990-01-01   | 123-12-1234 | missing_data | sp_last      |
      |          | Doe     | 1990-01-01   | 123-12-1234 | missing_data | sp_first     |

  Scenario: User submits child data with the enrollment.
    Given I prepare an enrollment file with basic valid enrollment data
    And I add the following enrollment data columns
      | ch1_first | ch1_last | ch1_birthdate | ch1_ssn | ch1_coverage | ch1_premium |
      | Johnny    | Doe      | 2010-02-02    |         | 10000        | 2.50        |
    When I submit the file to the Enrollment API
    Then I should see a success response

  Scenario: User submits a second child's data with the enrollment.
    Given I prepare an enrollment file with basic valid enrollment data
    And I add the following enrollment data columns
      | ch1_first | ch1_last | ch1_birthdate | ch1_ssn | ch1_coverage | ch1_premium |
      | Johnny    | Doe      | 2010-02-02    |         | 10000        | 2.50        |
    And I add the following enrollment data columns
      | ch2_first | ch2_last | ch2_birthdate | ch2_ssn | ch2_coverage | ch2_premium |
      | Susie     | Doe      | 2011-03-03    |         | 10000        | 2.50        |
    When I submit the file to the Enrollment API
    Then I should see a success response

  Scenario Outline: User submits coverage without premium and vice versa
    Given I prepare an enrollment file with basic valid enrollment data
    And I add valid spouse enrollment data
    And I add valid child enrollment data
    And I add a valid second child enrollment data
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
      | ' '       | ch1_coverage | missing_data |
      | ' '       | ch1_premium  | missing_data |
      | ' '       | ch2_coverage | missing_data |
      | ' '       | ch2_premium  | missing_data |

    @wip
  Scenario: It should detect invalid combinations of products and states
    Given I prepare an enrollment file with basic valid enrollment data
    Given 'IN' is not a valid state for the 'FPPTI' product
    And I substitute 'FPPTI' for the column 'product_code'
    And I substitute 'IN' for the column 'signed_at_state'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type                | error_field     |
      | invalid_state_for_product | signed_at_state |

#  @wip
#  Scenario: The user submits answers to exactly the right number of questions given the product and applicant type.
#    Given The product 'FPPTI' has the following health questions
#      | applicant | question                                          |
#      | employee  | How are you feeling today?                        |
#      | employee  | Have you had a heart attack in the past 6 months? |
#      | spouse    | How are you feeling today?                        |
#      | spouse    | Have you had a heart attack in the past 6 months? |
#      | spouse    | Special spouse-only question                      |
#      | child     | Special child question                            |
#    Given I prepare an enrollment file with basic valid enrollment data
#    And I add valid spouse enrollment data
#    And I add valid child enrollment data
#    And I add a valid second child enrollment data
#    And I add the following enrollment data columns
#      | emp_question_1_answer | emp_question_2_answer |
#      | N                     | N                     |
#    And I add the following enrollment data columns
#      | sp_question_1_answer | sp_question_2_answer | sp_question_3_answer |
#      | Y                    | N                    | N                    |
#    And I add the following enrollment data columns
#      | ch1_question_1_answer |
#      | N                     |
#    And I add the following enrollment data columns
#      | ch2_question_1_answer |
#      | N                     |
#    When I submit the file to the Enrollment API
#    Then I should see a success response
#
#
#  Scenario: The user does not submit answers for the questions required for the product.
#    Given The product 'FPPTI' has the following health questions
#      | applicant | question                   |
#      | employee  | How are you feeling today? |
#    And I prepare an enrollment file with basic valid enrollment data
#    When I submit the file to the Enrollment API
#    Then I should see the following errors in the response
#      | error_type        | error_field   |
#      | invalid_questions | emp_questions |
#
#  Scenario: The user submits the wrong number of employee questions for the product.
#    Given The product 'FPPTI' has the following health questions
#      | applicant | question                   |
#      | employee  | How are you feeling today? |
#    And I prepare an enrollment file with basic valid enrollment data
#    And I add the following enrollment data columns
#      | emp_question_1_answer | emp_question_2_answer |
#      | Y                     | Y                     |
#    When I submit the file to the Enrollment API
#    Then I should see the following errors in the response
#      | error_type        | error_field   |
#      | invalid_questions | emp_questions |
#
#  Scenario: The user submits the wrong number of spouse questions for the product.
#    Given The product 'FPPTI' has the following health questions
#      | applicant | question                   |
#      | spouse    | How are you feeling today? |
#      | spouse    | Another question           |
#    And I prepare an enrollment file with basic valid enrollment data
#    And I add valid spouse enrollment data
#    And I add the following enrollment data columns
#      | sp_question_1_answer |
#      | Y                    |
#    When I submit the file to the Enrollment API
#    Then I should see the following errors in the response
#      | error_type        | error_field   |
#      | invalid_questions | sp_questions  |
#
#  Scenario: The user submits the wrong number of child questions for the product.
#    Given The product 'FPPTI' has the following health questions
#      | applicant | question                   |
#      | child     | How are you feeling today? |
#      | child     | Another question           |
#    And I prepare an enrollment file with basic valid enrollment data
#    And I add valid child enrollment data
#    And I add a valid second child enrollment data
#    And I add the following enrollment data columns
#      # Should have a full set of answers for each child.
#      | ch1_question_1_answer | ch2_question_1_answer |
#      | Y                     | N                     |
#    When I submit the file to the Enrollment API
#    Then I should see the following errors in the response
#      | error_type        | error_field   |
#      | invalid_questions | ch_questions  |
#
## TODO: handle multiple records
## TODO: show the record number that an error refers to
## TODO: well-formed email address
## TODO: question Y / N / GI handling?
## TODO: Other boolean questions (actively at work) required? and Y/N validation

# TODO: check that the case is currently enrolling
# TODO: check that the case is enrolling the submitted product
