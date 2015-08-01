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
      | user_token | case_token | product_code | payment_mode | enrollment_type |
      | ABC        | XYZ        | FPPTI        | 52           | S               |
    And I add the following enrollment data columns
      | emp_first | emp_last | emp_birthdate | emp_ssn     | emp_coverage | emp_premium | emp_gender |
      | Joe       | Johnson  | 1990-01-01    | 123-12-1234 | 50000        | 10.00       | m          |
    And I add the following enrollment data columns
      | emp_street | emp_street2 | emp_city | emp_state | emp_zipcode | emp_phone | emp_date_of_hire |
      | 123 Sesame |             | Lansing  | MI        | 12345       |           | 2012-01-01       |
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

  Scenario: It should allow case-insensitive match on required headers
    Given I prepare an enrollment file with basic valid enrollment data
    And I remove the column 'emp_first'
    And I add the following enrollment data columns
    | EMP_FIRST |
    | Joe       |
    When I submit the file to the Enrollment API
    Then I should see a success response


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
      | 52          |
      | 26          |
      | 24          |
      | 12          |

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
      | column_name      |
      | user_token       |
      | case_token       |
      | product_code     |
      | emp_first        |
      | emp_last         |
      | emp_ssn          |
      | emp_birthdate    |
      | payment_mode     |
      | agent_name       |
      | agent_code       |
      | agent_sig_txt    |
      | emp_sig_txt      |
      | application_date |
      | time_stamp       |


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

  Scenario: It should allow adding just spouse coverage (without employee coverage)
    Given I prepare an enrollment file with basic valid enrollment data
    And I substitute ' ' for the column 'emp_coverage'
    And I substitute ' ' for the column 'emp_premium'
    And I add valid spouse enrollment data
    When I submit the file to the Enrollment API
    Then I should see a success response

  Scenario: It should have an error if no one selects any coverage
    Given I prepare an enrollment file with basic valid enrollment data
    And I substitute ' ' for the column 'emp_coverage'
    And I substitute ' ' for the column 'emp_premium'
    And I add valid spouse enrollment data
    And I substitute ' ' for the column 'sp_coverage'
    And I substitute ' ' for the column 'sp_premium'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type   | error_field  |
      | missing_data | emp_coverage |

  Scenario: It should detect invalid combinations of products and states
    Given I prepare an enrollment file with basic valid enrollment data
    Given 'IN' is not a valid state for the 'FPPTI' product
    And I substitute 'FPPTI' for the column 'product_code'
    And I substitute 'IN' for the column 'signed_at_state'
    When I submit the file to the Enrollment API
    Then I should see the following errors in the response
      | error_type                | error_field     |
      | invalid_state_for_product | signed_at_state |

  @wip
  Scenario: The user submits answers to exactly the right number of questions given the product and applicant type.
    Given The product 'FPPTI' has the following health questions
      | applicant | question                                          |
      | employee  | How are you feeling today?                        |
      | employee  | Have you had a heart attack in the past 6 months? |
      | spouse    | How are you feeling today?                        |
      | spouse    | Have you had a heart attack in the past 6 months? |
      | spouse    | Special spouse-only question                      |
      | child     | Special child question                            |
    Given I prepare an enrollment file with basic valid enrollment data
    And I add valid spouse enrollment data
    And I add valid child enrollment data
    And I add a valid second child enrollment data
    And I add the following enrollment data columns
      | emp_question_1_answer | emp_question_2_answer |
      | N                     | N                     |
    And I add the following enrollment data columns
      | sp_question_1_answer | sp_question_2_answer | sp_question_3_answer |
      | Y                    | N                    | N                    |
    And I add the following enrollment data columns
      | ch1_question_1_answer |
      | N                     |
    And I add the following enrollment data columns
      | ch2_question_1_answer |
      | N                     |
    When I submit the file to the Enrollment API
    Then I should see a success response

 Scenario: It should accept all optional fields in addition to the basic data and not ignore these columns.
   Given I prepare an enrollment file with basic valid enrollment data
   And I add valid spouse enrollment data
   And I add valid child enrollment data
   And I add the following enrollment data columns
     | actively_at_work | emp_email     | emp_height_inches | emp_weight_pounds | emp_smoker |
     | Y                | joe@gmail.com | 70                | 150               | N          |
   And I add the following enrollment data columns
     | sp_height_inches | sp_weight_pounds | sp_smoker |
     | 65               | 130              | N         |
   And I add the following enrollment data columns
     | sp_street | sp_street2 | sp_city | sp_state | sp_zipcode | sp_phone   |
     | Other st  |            | Chicago | IL       | 11444      | 1242223535 |
   And I add the following enrollment data columns
     | existing_insurance | replacing_insurance | sp_treated_6_months | sp_disabled_6_months |
     | N                  | N                   | N                   | N                    |
   And I add the following enrollment data columns
     | replacement_read_aloud | replacement_is_terminating | replacement_using_funds |
     | N                      | N                          | N                       |
   And I add the following enrollment data columns
     | replacement_policy1_name | replacement_policy1_number | replacement_policy1_insured |
     | Prudential               | 111AAA33                   | Joe                         |
   And I add the following enrollment data columns
     | replacement_policy1_replaced_or_financing | replacement_policy1_reason |
     | R                                         | Needed better coverage     |
   And I add the following enrollment data columns
     | emp_bene_name   | emp_bene_birthdate | emp_bene_relationship | emp_bene_ssn |
     | Emp. Prim. Bene | 1990-10-10         | Brother               | 555-55-5555  |
   And I add the following enrollment data columns
     | sp_bene_name | sp_bene_birthdate | sp_bene_relationship | sp_bene_ssn |
     | Sp prim bene | 1980-11-11        | daughter             | 111-11-1112 |
   And I add the following enrollment data columns
     | emp_cont_bene_name | emp_cont_bene_birthdate | emp_cont_bene_relationship | emp_cont_bene_ssn |
     | Emp. Cont. Bene    | 1989-01-10              | Relatative                 | 666-55-5555       |
   And I add the following enrollment data columns
     | sp_cont_bene_name | sp_cont_bene_birthdate | sp_cont_bene_relationship | sp_cont_bene_ssn |
     | Sp cont bene      | 1985-11-12             | friend                    | 121-12-1112      |
   When I submit the file to the Enrollment API
   Then I should see a success response
   And the parsed record should include the following attributes
     | attribute_name                            |
     # Misc form questions
     | actively_at_work                          |
     # Additional employee data
     | emp_email                                 |
     | emp_date_of_hire                          |
     | emp_height_inches                         |
     | emp_weight_pounds                         |
     | emp_smoker                                |
     # Additional spouse data
     | sp_street                                 |
     | sp_street2                                |
     | sp_city                                   |
     | sp_state                                  |
     | sp_zipcode                                |
     | sp_phone                                  |
     # Other questions
     | existing_insurance                        |
     | replacing_insurance                       |
     | sp_treated_6_months                       |
     | sp_disabled_6_months                      |
     # Replacement attributes
     | replacement_read_aloud                    |
     | replacement_is_terminating                |
     | replacement_using_funds                   |
     | replacement_policy1_name                  |
     | replacement_policy1_number                |
     | replacement_policy1_insured               |
     | replacement_policy1_replaced_or_financing |
     | replacement_policy1_reason                |
     # Beneficiary attributes
     | emp_bene_name                             |
     | emp_bene_birthdate                        |
     | emp_bene_relationship                     |
     | emp_bene_ssn                              |
     | sp_bene_name                              |
     | sp_bene_birthdate                         |
     | sp_bene_relationship                      |
     | sp_bene_ssn                               |
     | emp_cont_bene_name                        |
     | emp_cont_bene_birthdate                   |
     | emp_cont_bene_relationship                |
     | emp_cont_bene_ssn                         |
     | sp_cont_bene_name                         |
     | sp_cont_bene_birthdate                    |
     | sp_cont_bene_relationship                 |
     | sp_cont_bene_ssn                          |

 Scenario: It should create valid optional enrollment data
   Given I prepare an enrollment file with basic valid enrollment data
   And I add valid spouse enrollment data
   And I add valid child enrollment data
   And I add valid optional enrollment data
   When I submit the file to the Enrollment API
   Then I should see a success response
   And the parsed record should include the following attributes
     | attribute_name                            |
     # Misc form questions
     | actively_at_work                          |
     # Additional employee data
     | emp_email                                 |
     | emp_height_inches                         |
     | emp_weight_pounds                         |
     | emp_smoker                                |
     # Additional spouse data
     | sp_street                                 |
     | sp_street2                                |
     | sp_city                                   |
     | sp_state                                  |
     | sp_zipcode                                |
     | sp_phone                                  |
     | sp_email                                 |
     | sp_height_inches                         |
     | sp_weight_pounds                         |
     | sp_smoker                                |
     # Other questions
     | existing_insurance                        |
     | replacing_insurance                       |
     | sp_treated_6_months                       |
     | sp_disabled_6_months                      |
     # Replacement attributes
     | replacement_read_aloud                    |
     | replacement_is_terminating                |
     | replacement_using_funds                   |
     | replacement_policy1_name                  |
     | replacement_policy1_number                |
     | replacement_policy1_insured               |
     | replacement_policy1_replaced_or_financing |
     | replacement_policy1_reason                |
     # Beneficiary attributes
     | emp_bene_name                             |
     | emp_bene_birthdate                        |
     | emp_bene_relationship                     |
     | emp_bene_ssn                              |
     | sp_bene_name                              |
     | sp_bene_birthdate                         |
     | sp_bene_relationship                      |
     | sp_bene_ssn                               |
     | emp_cont_bene_name                        |
     | emp_cont_bene_birthdate                   |
     | emp_cont_bene_relationship                |
     | emp_cont_bene_ssn                         |
     | sp_cont_bene_name                         |
     | sp_cont_bene_birthdate                    |
     | sp_cont_bene_relationship                 |
     | sp_cont_bene_ssn                          |



## TODO: question Y / N / GI handling?
## TODO: Other boolean questions (actively at work) required? and Y/N validation

## TODO: handle multiple records
## TODO: show the record number that an error refers to
#Scenario: The user submits multiple enrollment records.
  #  Given I prepare an enrollment file with basic valid enrollment data
  #  And I add an additional valid record
  #  When I submit the file to the Enrollment API
  #  Then I should see a success response

  #Scenario: It should show which record caused the error when submitting

# TODO: check that the case is currently enrolling
# TODO: check that the case is enrolling the submitted product
## TODO: check for did_decline?
  # TODO Do we need employee and spouse other owner?
