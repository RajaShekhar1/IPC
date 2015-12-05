Feature: Compute rates for products and associated riders.
  5Star/Dell has provided an excel spreadsheet (2015-12-02 folders in Artifacts)
    that specify how the different riders can be combined with various products
    and what rates to use. We need to compute the rates data in the same way the
    spreadsheet does, to the penny. What follows is the acceptance criteria for
    useful combinations of rates and the expected output of the rates calculator
    from the TAA system.

  Background:
    Given I have entered the following plan codes into TAA:
      | Base Product | Applicant Type  | Rider Codes | State   | Plan Code |
      | FPPTI        | Employee,Spouse |             | DEFAULT | FPPTI     |
      | FPPTI        | Employee,Spouse |             | UT      | FPPTI UT  |
      | FPPTI        | Employee,Spouse | AIO         | UT      | FPATI UT  |
      | FPPTI        | Child           |             | DEFAULT | FPPTID    |
      | FPP Gov      | Employee,Spouse |             | DEFAULT | FPPTIG    |

  Scenario Outline: : I want to know the Dell FPP Plan code given a base product, applicant type, set of riders, and state
    Given The applicant type is '<Applicant Type>'
    And the riders selected are '<Riders>'
    And the state is '<State>'
    When I look up the plan code for base product '<Base Product Type>'
    Then I should see '<Plan Code>'

    Examples: Employee with different base products
      | Riders | Applicant Type | Base Product Type | State | Plan Code |
      |        | Employee       | FPPTI             | MI    | FPPTI     |
      |        | Employee       | FPPTIG            | MI    | FPPTIG    |
      |        | Employee       | FPPTIW            | MI    | FPPTIW    |
      |        | Employee       | FPPTIB            | MI    | FPPTIB    |
      |        | Employee       | FPPTIY            | MI    | FPPTIY    |

    Examples: Spouse with

      | Y   | N    | N    | N  | Employee       | FPATI          | N/A             | FPATW           | FPATB           | FPATY           |
      | N   | N    | N    | Y  | Employee       | FPPTI          | FPPTIG          | FPPTIW          | FPPTIB          | FPPTIY          |
      | N   | Y    | N    | N  | Employee       | FPQTI3         | FPQTIG/3        | FPQTIW/3        | FPQTIB/3        | FPQTIY/3        |
      | N   | N    | Y    | N  | Employee       | FPQTI4         | FPQTIG/4        | FPQTIW/4        | FPQTIB/4        | FPQTIY/4        |




    # Base Product, Applicant Type, List of Rider Codes = Unique Plan Code if entered, otherwise Disallowed
    Scenario: Lookup without state or riders gets generic code
    When I look up the plan code for product 'FPPTI', applicant type 'Employee', riders ' ', state ' '
    Then I should see 'FPPTI'


  Scenario: Lookup Spouse enrollment plan codes (they should match employee codes above)
    Given The applicant type is 'Spouse'
    Then I should see the following plan codes
    | AIR | QOL3 | QOL4 | WP |  Code for FPPTI | Code for FPPTIG | Code for FPPTIW | Code for FPPTIB | Code for FPPTIY |
    | N   | N    | N    | N  |  FPPTI          | FPPTIG          | FPPTIW          | FPPTIB          | FPPTIY          |

  Scenario: Lookup child (Dependent) enrollment plan codes
    Given The applicant type is 'Employee'
    Then I should see the following plan codes
      | N   | N    | N    | N  | Child          | FPPTID         | FPPTIDG         | FPPTIDW         | FPPTIDB         | FPPTIDY         |

#
#  Scenario: FPPTI Employee / Spouse premiums by converage with no riders.
#    Given I want rates for the following product data:
#      | Product | Mode | WP Rider | QOL Rider | AIR Rider |
#      | FPPTI   | 52   | N        | N         | N         |
#
#    When I lookup premiums by coverage with the above data for the following ages:
#      | Age |
#      | 18  |
#      | 30  |
#      | 60  |
#      | 70  |
#    Then I should see the following premiums:
#      | Age | $10,000 | $20,000 | $25,000 | $30,000 | $40,000 | $50,000 | $60,000 | $70,000 | $75,000 | $80,000 | $90,000 | $100,000 | $110,000 | $125,000 | $130,000 | $140,000 | $150,000 |
#      | 18  | $1.65   | 2.30    | $2.63   | $2.96   | $3.61   | $4.26   | $4.91   | $5.56   | $5.89   | $6.22   | $6.87   | $7.52    | $8.17    | $9.15    | $9.48    | $10.13   | $10.78   |
#      | 30  | $1.75   | $2.50   | $2.88   | $3.26   | $4.01   | $4.76   | $5.51   | $6.26   | $6.64   | $7.02   | $7.77   | $8.52    | $9.27    | $10.40   | $10.78   | $11.53   | $12.28   |
#      | 60  | $6.71   | $12.42  | $15.28  | $18.13  | $23.85  | $29.56  | $35.27  | $40.98  | $43.84  | $46.69  | $52.40  | $58.12   | $63.83   | $72.39   | $75.25   | $80.96   | $86.67   |
#      | 70  | $12.92  | $24.85  | $30.81  | $36.77  | $48.69  | $60.62  | $72.54  | $84.46  | $90.42  | $96.38  | $108.31 | $120.23  | $132.15  | $150.04  | $156.00  | $167.92  | $179.85  |
#
#  Scenario: FPPTI Employee / Spouse coverages by premiums with no riders.
#    Given I want rates for the following product data:
#      | Product | Mode | WP Rider | QOL Rider | AIR Rider |
#      | FPPTI   | 12   | N        | N         | N         |
#
#    When I lookup coverages by premium with the above data for the following ages:
#      | Age |
#      | 18  |
#      | 30  |
#      | 60  |
#      | 70  |
#    Then I should see the following coverages:
#      | Age | $10.00  | $15.00  | $20.00  | $25.00  | $30.00  | $35.00   | $40.00   | $45.00   | $50.00   |
#      | 18  | $20,059 | $37,758 | $55,457 | $73,156 | $90,855 | $108,555 | $126,254 | $143,953 | $161,652 |
#      | 30  | $17,391 | $32,737 | $48,082 | $63,427 | $78,772 | $94,118  | $109,463 | $124,808 | $140,153 |
#      | 60  | $2,290  | $4,310  | $6,330  | $8,350  | $10,370 | $12,391  | $14,411  | $16,431  | $18,451  |
#      | 70  | $1,097  | $2,065  | $3,032  | $4,000  | $4,968  | $5,935   | $6,903   | $7,871   | $8,839   |
#

