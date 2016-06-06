Feature: An employee takes an app using the wizard with an Agent assisting on the phone
  In order to make the process of signing up for insurance easier
  As an employee
  I want to complete my application using the Call Center

  Background:
    Given I have logged in as an agent
    Given I have a case that is actively enrolling named 'Test Case' with products and call center
      | product_code |
      | FPPTI        |
    Given I have a case that is actively enrolling named 'Family Multi-Protection Plan Test Case' with products and call center
      | product_code |
      | FPPTI        |
      | FPPCI        |
    Given I have a case that is actively enrolling named 'Multi-Product Test Case' with products and call center
      | product_code |
      | FPPTI        |
      | FPPCI        |
      | Group CI     |

  Scenario: Agent does the enrollment through the phone and declines all products
    Given I begin a new empty enrollment for the case 'Test Case'
    When I enter the following information into the wizard step 1
      | emp_first | emp_last  | emp_birthdate | emp_coverage | actively_at_work |
      | Joe       | Person    | 02/29/1980    | declined     | Y                |
    Then I should be redirected to the Application Declined page
    And  I click on Back to Home
    Then I should be redirected to the Home page
    And  I navigate to the enrollment page for 'Test Case'
    Then I should be redirected to the Enrollment Page
    And  I click on that person's Enrollment
    Then I should see that the person Declined


  Scenario: Agent does the enrollment through the phone and reaches signing ceremony
    Given I begin a new empty enrollment for the case 'Test Case'
    When I enter the following information into the wizard step 1
      | emp_first | emp_last  | emp_birthdate | emp_coverage | actively_at_work |
      | Joe       | Person    | 02/29/1980    | best         | Y                |
    And  I select 'No' for every question on step 2 of the wizard
    And  I enter the following data for step 3 of the wizard
      | emp_gender | emp_street1 | emp_city | emp_state | emp_zip |
      | m          | 123 Sesame  | Lansing  | MI        | 12345   |
    And  I enter nothing for step 4 of the wizard
    And  I enter the following for step 5 of the wizard
      | ee_bene_name | ee_bene_relationship |
      | Ron          | Howard               |
    And  I enter the following for step 6 of the wizard
      | date_of_hire | ack_benefit_disclosure | authorize_payroll_deduction |
      | 01/01/2010   | Y                      | Y                           |
    And  I check all boxes on the Agreement disclaimer
    And  I click sign on the Other Insurance Questions
    Then I should be redirected to the Enrollment Page
    And  I click on that person's Enrollment
    Then I should see that the person is Enrolled


  Scenario: Agent does enrollment on the phone for coverage on spouse and child
    Given I begin a new empty enrollment for the case 'Test Case'
    When I enter the following information into the wizard step 1
      | emp_first | emp_last | emp_birthdate | emp_coverage | actively_at_work | is_married | include_children | sp_first | sp_last | sp_birthdate | ch1_first | ch1_last | ch1_birthdate |
      | Joe       | Tester   | 02/29/1980    | best         | Y                | Y          | Y                | Jane     | Tester  | 03/29/1985   | Johnny    | Tester   | 10/10/2000    |
    And I select 'No' for every question on step 2 of the wizard
    And I enter the following data for step 3 of the wizard
      | emp_gender | emp_street1 | emp_city | emp_state | emp_zip |
      | m          | 123 Sesame  | Lansing  | MI        | 12345   |
    And I enter the following data for step 4 of the wizard
      | sp_gender |
      | f         |
    And I enter the following for step 5 of the wizard
      | ee_bene_name | ee_bene_relationship |
      | Ron          | Howard               |
    And I enter the following for step 6 of the wizard
      | date_of_hire | ack_benefit_disclosure | authorize_payroll_deduction |
      | 01/01/2010   | Y                      | Y                           |
    And  I check all boxes on the Agreement disclaimer
    And  I click sign on the Other Insurance Questions
    Then I should be redirected to the Enrollment Page
    And  I click on that person's Enrollment
    Then I should see that the person is Enrolled