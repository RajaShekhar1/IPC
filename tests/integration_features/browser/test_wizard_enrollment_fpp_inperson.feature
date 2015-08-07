
Feature: An employee takes an app using the wizard with an Agent assisting
  In order to make the process of signing up for insurance easier
  As an employee
  I want to complete my application using the TAA wizard

  Scenario: Employee selects coverage for himself and is not in the census.
    Given I have logged in as an agent
    And I have a case that is actively enrolling named 'Test Case' with products
      | product_code |
      | FPPTI        |
    And I begin a new empty enrollment for the case 'Test Case'
    When I enter the following information into the wizard step 1
      | emp_first | emp_last  | emp_birthdate | emp_coverage |
      | Joe       | Testerson | 02/29/1980    | better        |
    And I select 'No' for every question on step 2 of the wizard
    And I enter the following data for step 3 of the wizard
      | emp_gender | emp_street1 | emp_city | emp_state | emp_zip |
      | m          | 123 Sesame  | Lansing  | MI        | 12345   |
    And I enter nothing for step 4 of the wizard
    And I enter the following for step 5 of the wizard
      | bene_name | bene_relationship |
      | Ron       | Howard            |
    And I enter the following for step 6 of the wizard
      | date_of_hire | ack_benefit_disclosure | authorize_payroll_deduction |
      | 01/01/2010   | Y                      | Y                           |
    Then I should be redirected to the DocuSign website.
