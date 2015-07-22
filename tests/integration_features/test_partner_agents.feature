
Feature: Partner agents should have limited access to enrollment cases.
  In order to enable agents to do their jobs and not mess the case up
  As a partner agent
  I want to have access to enrolling certain cases, but not to manage the case.

  Scenario: A partner agent adds a new enrollment to a case (currently creates a census record).
    Given I have a case that is actively enrolling named 'DelMar Test Case'
    And I have an agent 'Joe Agent'
    And I make 'Joe Agent' a partner agent on 'DelMar Test Case'
    And I log in as 'Joe Agent'
    And I navigate to the enrollment page for 'DelMar Test Case'
    When I add a new enrollment with '123-12-1234' as the SSN
    Then I should be redirected to the enrollment wizard

