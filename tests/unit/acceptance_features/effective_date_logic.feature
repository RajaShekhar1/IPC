Feature: Allow different types of effective dates to be computed automatically by TAA.


  Scenario Outline: Use static date with an open enrollment period, but not ongoing.
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is not checked
    And the 'open' effective date is set to 'Static Date' with parameter '12/15/2016'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      # Before the enrollment period, not allowed
      | 11/30/2016  | N/A            |
      | 12/1/2016   | 12/15/2016     |
      | 12/15/2016  | 12/15/2016     |
      # After the static date, use the application date
      | 12/16/2016  | 12/16/2016     |
      | 12/31/2016  | 12/31/2016     |
      # Without ongoing checked, enrollments after the open enrollment period are not allowed
      | 1/1/2017    | N/A            |


  Scenario Outline: Use static date with both open enrollment period and ongoing enrollments.
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Static Date' with parameter '1/1/2017'
    And the 'ongoing' effective date is set to 'Static Date' with parameter '3/1/2017'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      # Before the enrollment period, not allowed
      | 11/30/2016  | N/A            |
      | 12/1/2016   | 1/1/2017       |
      | 12/31/2016  | 1/1/2017       |
      # After the open enrollment period, use the ongoing static date
      | 1/1/2017    | 3/1/2017       |
      | 3/1/2017    | 3/1/2017       |
      # Which will default to the application date when we pass the ongoing static date
      | 3/2/2017    | 3/2/2017       |


  Scenario Outline: Use the Static method for open enrollment and Cutoff method for ongoing
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Static Date' with parameter '1/1/2017'
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
    And the 'open' effective date is set to 'Static Date' with parameter '1/1/2017'
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
    When The enroller picks '<enroller_picks_date>' and I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | enroller_picks_date | effective_date |
      | 12/1/2016   | 12/6/2016           | 12/6/2016      |
      # Must be at least the minimum days away
      | 12/1/2016   | 12/4/2016           | 12/4/2016      |
      # Must not be before  the minuimum
      | 12/1/2016   | 12/3/2016           | N/A            |


  Scenario Outline: Use the "Friday grouping" method
    Given I have a case without an enrollment period but it has ongoing enrollments
    And the 'ongoing' effective date is set to 'Friday grouping' with parameter '3'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      | 12/1/2016   | 12/9/2016      |
      # The 9th is a friday; it is also 3 days after the 6th, so it counts
      | 12/6/2016   | 12/9/2016      |
      # The 7th bumps the effective date to the next Friday
      | 12/7/2016   | 12/16/2016     |
      # The 20th, likewise, is exactly 3 days before the 4th friday of the month, the 23rd.
      | 12/20/2016  | 12/23/2016     |
      # The last Friday, the 30th, is a 5th Friday, so we want to make sure it skips that.
      | 12/21/2016  | 1/6/2017       |
      # This was causing problems with the first implementation, when there is no next friday in the current month.
      | 08/24/2016  | 9/2/2016       |



  Scenario Outline: Use the "Friday grouping" method
    Given I have a case without an enrollment period but it has ongoing enrollments
    And the 'ongoing' effective date is set to 'Friday grouping' with parameter '2'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      | 10/1/2016   | 10/7/2016      |
      | 10/19/2016  | 10/21/2016     |
      | 10/20/2016  | 10/28/2016     |
      | 10/21/2016  | 10/28/2016     |








