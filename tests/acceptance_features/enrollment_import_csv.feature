Feature: Convert uploaded enrollment CSV files to JSON before processing
  In order to provide enrollment data in a variety of common formats,
  As a 3rd party,
  I want to submit my enrollment data in CSV file format.

  Scenario: Submit basic file data with uppercase column names
    Given I have prepared the following csv file
    """
    USER_TOKEN,CASE_TOKEN,PRODUCT_CODE,PAYMENT_MODE,EMP_FIRST,EMP_LAST,EMP_SSN,EMP_BIRTHDATE,EMP_COVERAGE,EMP_PREMIUM,\
    EMP_STREET,EMP_STREET2,EMP_CITY,EMP_STATE,EMP_ZIPCODE,EMP_PHONE,EMP_PIN,EMP_SIG_TXT,EMP_APPLICATION_DATE,\
    TIME_STAMP,SIGNED_AT_CITY,SIGNED_AT_STATE,AGENT_NAME,AGENT_CODE,AGENT_SIG_TXT
    token_user,token_agent,FPPTI,monthly,Joe,Smith,111223333,2015-01-31,50000,33.25,\
    123 Sesame,,Chicago,IL,45555,,11441144,esigned by JOE SMITH,2015-01-01,\
    2015-01-01T10:30:00,Chicago,IL,Test Agent,26TEST,esigned by TEST AGENT
    """
    When I convert the CSV to JSON
    Then I should see the following JSON data
      | user_token | case_token | product_code | payment_mode |
      | token_user | token_case | FPPTI        | monthly      |
    And I should see the following JSON data
      | emp_first | emp_last | emp_birthdate | emp_ssn   | emp_coverage | emp_premium |
      | Joe       | Smith    | 2015-01-31    | 111223333 | 50000        | 33.25       |
    And I should see the following JSON data
      | emp_street | emp_street2 | emp_city | emp_state | emp_zipcode | emp_phone |
      | 123 Sesame |             | Chicago  | IL        | 45555       |           |
    And I should see the following JSON data
      | emp_pin  | emp_sig_txt          | application_date | time_stamp          | signed_at_city | signed_at_state |
      | 11441144 | esigned by JOE SMITH | 2015-01-01       | 2015-01-01T10:30:00 | Lansing        | MI              |
    And I should see the following JSON data
      | agent_name | agent_code | agent_sig_txt         |
      | Test Agent | 26TEST     | esigned by TEST AGENT |


