Feature: Display paylogix draft dates as Nth week, where N is 1-4 (5th fridays roll to 1).

  Scenario Outline: Convert Friday draft days to week numbers
    Given the draft day is '<draft_date>'
    When I convert the date to a draft week
    Then I should see '<week_int>'

    Examples:

      | draft_date | week_int |
      | 2016-09-02 | 1        |
      | 2016-09-09 | 2        |
      | 2016-09-16 | 3        |
      | 2016-09-23 | 4        |
      # Rollover week 5 to 1
      | 2016-09-30 | 1        |
      # Had issues with all these multiples of day '7' with the original design
      | 2016-10-07 | 1        |
      | 2016-10-14 | 2        |
      | 2016-10-21 | 3        |
      | 2016-10-28 | 4        |
      | 2016-11-04 | 1        |