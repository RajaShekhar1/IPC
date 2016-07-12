Feature: Allow different types of effective dates to be computed automatically by TAA.


  Scenario Outline: Use static date with an open enrollment period, but not ongoing.
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is not checked
    And the 'open' effective date is set to 'Static date' with parameter '12/15/2016'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
    | enroll_date | effective_date |
    # Before the enrollment period, not allowed
    | 11/30/2016   | N/A            |
    | 12/1/2016    | 12/15/2016     |
    | 12/15/2016   | 12/15/2016     |
    # After the static date, use the application date
    | 12/16/2016   | 12/16/2016     |
    | 12/31/2016   | 12/31/2016     |
    # Without ongoing checked, enrollments after the open enrollment period are not allowed
    | 1/1/2017     | N/A            |


  Scenario Outline: Use static date with both open enrollment period and ongoing enrollments.
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Static date' with parameter '1/1/2017'
    And the 'ongoing' effective date is set to 'Static date' with parameter '3/1/2017'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
    | enroll_date | effective_date |
    # Before the enrollment period, not allowed
    | 11/30/2016   | N/A            |
    | 12/1/2016    | 1/1/2017       |
    | 12/31/2016   | 1/1/2017        |
    # After the open enrollment period, use the ongoing static date
    | 1/1/2017   | 3/1/2017     |
    | 3/1/2017   | 3/1/2017     |
    # Which will default to the application date when we pass the ongoing static date
    | 3/2/2017     | 3/2/2017   |


  Scenario Outline: Use the Static method for open enrollment and Cutoff method for ongoing
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Static date' with parameter '1/1/2017'
    And the 'ongoing' effective date is set to 'Cutoff nth of month' with parameter '15'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      # Normal static during enrollment period
      | 12/1/2016   | 1/1/2017       |
      | 12/15/2016  | 1/1/2017       |
      | 12/31/2016  | 1/1/2017       |
      # Day after enrollment period ends, switch to ongoing cutoff method.
      | 1/1/2017    | 2/1/2017       |
      | 1/2/2017    | 2/1/2017       |
      | 1/14/2017   | 2/1/2017       |
      # Cutoff date bumps to next month on the 15th
      | 1/15/2017   | 3/1/2017       |


Scenario Outline: Use Static for open enrollment and Cutoff for ongoing with a different cutoff date
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Static date' with parameter '1/1/2017'
    And the 'ongoing' effective date is set to 'Cutoff nth of month' with parameter '20'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      # Day after enrollment period ends, switch to ongoing cutoff method, but now the cutoff is the 20th.
      | 1/1/2017    | 2/1/2017       |
      | 1/2/2017    | 2/1/2017       |
      | 1/14/2017   | 2/1/2017       |
      | 1/15/2017   | 2/1/2017       |
      | 1/19/2017   | 2/1/2017       |
      | 1/20/2017   | 3/1/2017       |
      | 2/19/2017   | 3/1/2017       |
      | 2/20/2017   | 4/1/2017       |



Scenario Outline: Use Enroller Picks method.
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Enroller Picks' with parameters '5' and '3'
    When I enroll an applicant on '<enroll_date>' and the enroller picks '<enroller_picks_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | enroller_picks_date | effective_date |
      | 12/1/2016   | 12/6/2016           | 12/6/2016      |
      | 1/1/2017    |                     | 2/1/2017       |
      | 1/14/2017   |                     | 2/1/2017       |
      | 1/15/2017   |                     | 2/1/2017       |
      | 1/19/2017   |                     | 2/1/2017       |
      | 1/20/2017   |                     | 3/1/2017       |
      | 2/19/2017   |                     | 3/1/2017       |
      | 2/20/2017   |                     | 4/1/2017       |





