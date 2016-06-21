Feature: Allow different types of effective dates to be computed automatically by TAA.

  Scenario Outline: Static for open enrollment and Cutoff for ongoing
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Static date' with '1/1/2017'
    And the 'ongoing' effective date is set to 'Cutoff nth of month' with '15'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      | 12/1/2016   | 1/1/2017       |
      | 1/1/2017    | 1/1/2017       |
      | 1/2/2017    | 2/1/2017       |
      | 1/14/2017   | 2/1/2017       |
      | 1/15/2017   | 3/1/2017       |


Scenario Outline: Static for open enrollment and Cutoff for ongoing with a different cutoff date
    Given I have a case with an enrollment period from '12/1/2016' to '12/31/2016' and ongoing is checked
    And the 'open' effective date is set to 'Static date' with '1/1/2017'
    And the 'ongoing' effective date is set to 'Cutoff nth of month' with '20'
    When I enroll an applicant on '<enroll_date>'
    Then I should see the effective date is '<effective_date>'

    Examples:
      | enroll_date | effective_date |
      | 1/2/2017    | 2/1/2017       |
      | 1/15/2017   | 2/1/2017       |
      | 1/20/2017   | 3/1/2017       |