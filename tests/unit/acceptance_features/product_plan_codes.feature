# Created by zach at 2/12/16
Feature: Lookup product plan codes for submitting to the Third-Party Administrator, Dell.

  Scenario Outline: : I want to know the Dell FPP Plan code given a base product, applicant type, set of rider codes, and state
    Given The applicant type is '<Applicant Type>'
    And the AIR rider state is '<AIR>'
    And the WP rider state is '<WP>'
    And the QOL3 rider state is '<QOL3>'
    And the QOL4 rider state is '<QOL4>'
    And the state is '<State>'
    When I look up the plan code for base product '<Base Product Code>'
    Then I should see the plan code '<Plan Code>'

    Examples: Employee without riders basic plan codes.
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | N    | N    | Employee       | FPPTI             | MI    | FPPTI     |
      | N   | N  | N    | N    | Employee       | FPPTIG            | MI    | FPPTIG    |
      | N   | N  | N    | N    | Employee       | FPPTIW            | MI    | FPPTIW    |
      | N   | N  | N    | N    | Employee       | FPPTIB            | MI    | FPPTIB    |
      | N   | N  | N    | N    | Employee       | FPPTIY            | MI    | FPPTIY    |

    Examples: Spouse without riders gets the same codes as employee.
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | N    | N    | Spouse         | FPPTI             | MI    | FPPTI     |
      | N   | N  | N    | N    | Spouse         | FPPTIG            | MI    | FPPTIG    |
      | N   | N  | N    | N    | Spouse         | FPPTIW            | MI    | FPPTIW    |
      | N   | N  | N    | N    | Spouse         | FPPTIB            | MI    | FPPTIB    |
      | N   | N  | N    | N    | Spouse         | FPPTIY            | MI    | FPPTIY    |


    Examples: Child (Dependent) basic codes
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | N    | N    | Child          | FPPTI             | MI    | FPPTID    |
      | N   | N  | N    | N    | Child          | FPPTIG            | MI    | FPPTIDG   |
      | N   | N  | N    | N    | Child          | FPPTIW            | MI    | FPPTIDW   |
      | N   | N  | N    | N    | Child          | FPPTIB            | MI    | FPPTIDB   |
      | N   | N  | N    | N    | Child          | FPPTIY            | MI    | FPPTIDY   |


    Examples: Employee or Spouse with AIR rider
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | Y   | N  | N    | N    | Employee       | FPPTI             | MI    | FPATI     |
      | Y   | N  | N    | N    | Employee       | FPPTIG            | MI    | None      |
      | Y   | N  | N    | N    | Employee       | FPPTIW            | MI    | FPATW     |
      | Y   | N  | N    | N    | Employee       | FPPTIB            | MI    | FPATB     |
      | Y   | N  | N    | N    | Employee       | FPPTIY            | MI    | FPATY     |

      | Y   | N  | N    | N    | Spouse         | FPPTI             | MI    | FPATI     |
      | Y   | N  | N    | N    | Spouse         | FPPTIG            | MI    | None      |
      | Y   | N  | N    | N    | Spouse         | FPPTIW            | MI    | FPATW     |
      | Y   | N  | N    | N    | Spouse         | FPPTIB            | MI    | FPATB     |
      | Y   | N  | N    | N    | Spouse         | FPPTIY            | MI    | FPATY     |

    Examples: Employee or Spouse with WP rider
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | Y  | N    | N    | Employee       | FPPTI             | MI    | FPPTI     |
      | N   | Y  | N    | N    | Employee       | FPPTIG            | MI    | FPPTIG    |
      | N   | Y  | N    | N    | Employee       | FPPTIW            | MI    | FPPTIW    |
      | N   | Y  | N    | N    | Employee       | FPPTIB            | MI    | FPPTIB    |
      | N   | Y  | N    | N    | Employee       | FPPTIY            | MI    | FPPTIY    |

      | N   | Y  | N    | N    | Spouse         | FPPTI             | MI    | FPPTI     |
      | N   | Y  | N    | N    | Spouse         | FPPTIG            | MI    | FPPTIG    |
      | N   | Y  | N    | N    | Spouse         | FPPTIW            | MI    | FPPTIW    |
      | N   | Y  | N    | N    | Spouse         | FPPTIB            | MI    | FPPTIB    |
      | N   | Y  | N    | N    | Spouse         | FPPTIY            | MI    | FPPTIY    |

    Examples: Employee or Spouse with QOL3 rider
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | Y    | N    | Employee       | FPPTI             | MI    | FPQTI3    |
      | N   | N  | Y    | N    | Employee       | FPPTIG            | MI    | FPQTIG/3  |
      | N   | N  | Y    | N    | Employee       | FPPTIW            | MI    | FPQTIW/3  |
      | N   | N  | Y    | N    | Employee       | FPPTIB            | MI    | FPQTIB/3  |
      | N   | N  | Y    | N    | Employee       | FPPTIY            | MI    | FPQTIY/3  |

      | N   | N  | Y    | N    | Spouse         | FPPTI             | MI    | FPQTI3   |
      | N   | N  | Y    | N    | Spouse         | FPPTIG            | MI    | FPQTIG/3  |
      | N   | N  | Y    | N    | Spouse         | FPPTIW            | MI    | FPQTIW/3  |
      | N   | N  | Y    | N    | Spouse         | FPPTIB            | MI    | FPQTIB/3  |
      | N   | N  | Y    | N    | Spouse         | FPPTIY            | MI    | FPQTIY/3  |

    Examples: Employee or Spouse with QOL4 rider
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | N    | Y    | Employee       | FPPTI             | MI    | FPQTI4    |
      | N   | N  | N    | Y    | Employee       | FPPTIG            | MI    | FPQTIG/4  |
      | N   | N  | N    | Y    | Employee       | FPPTIW            | MI    | FPQTIW/4  |
      | N   | N  | N    | Y    | Employee       | FPPTIB            | MI    | FPQTIB/4  |
      | N   | N  | N    | Y    | Employee       | FPPTIY            | MI    | FPQTIY/4  |

      | N   | N  | N    | Y    | Spouse         | FPPTI             | MI    | FPQTI4    |
      | N   | N  | N    | Y    | Spouse         | FPPTIG            | MI    | FPQTIG/4  |
      | N   | N  | N    | Y    | Spouse         | FPPTIW            | MI    | FPQTIW/4  |
      | N   | N  | N    | Y    | Spouse         | FPPTIB            | MI    | FPQTIB/4  |
      | N   | N  | N    | Y    | Spouse         | FPPTIY            | MI    | FPQTIY/4  |

    Examples: Employee or Spouse with AIR and WP riders is never allowed.
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | Y   | Y  | N    | N    | Employee       | FPPTI             | MI    | None      |
      | Y   | Y  | N    | N    | Employee       | FPPTIG            | MI    | None      |
      | Y   | Y  | N    | N    | Employee       | FPPTIW            | MI    | None      |
      | Y   | Y  | N    | N    | Employee       | FPPTIB            | MI    | None      |
      | Y   | Y  | N    | N    | Employee       | FPPTIY            | MI    | None      |

      | Y   | Y  | N    | N    | Spouse         | FPPTI             | MI    | None      |
      | Y   | Y  | N    | N    | Spouse         | FPPTIG            | MI    | None      |
      | Y   | Y  | N    | N    | Spouse         | FPPTIW            | MI    | None      |
      | Y   | Y  | N    | N    | Spouse         | FPPTIB            | MI    | None      |
      | Y   | Y  | N    | N    | Spouse         | FPPTIY            | MI    | None      |

    Examples: Employee or Spouse with AIR rider and the QOL3 rider is same as AIR code
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | Y   | N  | Y    | N    | Employee       | FPPTI             | MI    | FPATI     |
      | Y   | N  | Y    | N    | Employee       | FPPTIG            | MI    | None      |
      | Y   | N  | Y    | N    | Employee       | FPPTIW            | MI    | FPATW     |
      | Y   | N  | Y    | N    | Employee       | FPPTIB            | MI    | FPATB     |
      | Y   | N  | Y    | N    | Employee       | FPPTIY            | MI    | FPATY     |

      | Y   | N  | Y    | N    | Spouse         | FPPTI             | MI    | FPATI     |
      | Y   | N  | Y    | N    | Spouse         | FPPTIG            | MI    | None      |
      | Y   | N  | Y    | N    | Spouse         | FPPTIW            | MI    | FPATW     |
      | Y   | N  | Y    | N    | Spouse         | FPPTIB            | MI    | FPATB     |
      | Y   | N  | Y    | N    | Spouse         | FPPTIY            | MI    | FPATY     |

    Examples: Employee or Spouse with AIR rider and the QOL4 rider.
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | Y   | N  | N    | Y    | Employee       | FPPTI             | MI    | FPATI4    |
      | Y   | N  | N    | Y    | Employee       | FPPTIG            | MI    | None      |
      | Y   | N  | N    | Y    | Employee       | FPPTIW            | MI    | FPATW/4   |
      | Y   | N  | N    | Y    | Employee       | FPPTIB            | MI    | FPATB/4   |
      | Y   | N  | N    | Y    | Employee       | FPPTIY            | MI    | FPATY/4   |

      | Y   | N  | N    | Y    | Spouse         | FPPTI             | MI    | FPATI4    |
      | Y   | N  | N    | Y    | Spouse         | FPPTIG            | MI    | None      |
      | Y   | N  | N    | Y    | Spouse         | FPPTIW            | MI    | FPATW/4   |
      | Y   | N  | N    | Y    | Spouse         | FPPTIB            | MI    | FPATB/4   |
      | Y   | N  | N    | Y    | Spouse         | FPPTIY            | MI    | FPATY/4   |

    Examples: Employee or Spouse with QOL3 rider + WP is same as QOL3
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | Y  | Y    | N    | Employee       | FPPTI             | MI    | FPQTI3    |
      | N   | Y  | Y    | N    | Employee       | FPPTIG            | MI    | FPQTIG/3  |
      | N   | Y  | Y    | N    | Employee       | FPPTIW            | MI    | FPQTIW/3  |
      | N   | Y  | Y    | N    | Employee       | FPPTIB            | MI    | FPQTIB/3  |
      | N   | Y  | Y    | N    | Employee       | FPPTIY            | MI    | FPQTIY/3  |

      | N   | Y  | Y    | N    | Spouse         | FPPTI             | MI    | FPQTI3   |
      | N   | Y  | Y    | N    | Spouse         | FPPTIG            | MI    | FPQTIG/3  |
      | N   | Y  | Y    | N    | Spouse         | FPPTIW            | MI    | FPQTIW/3  |
      | N   | Y  | Y    | N    | Spouse         | FPPTIB            | MI    | FPQTIB/3  |
      | N   | Y  | Y    | N    | Spouse         | FPPTIY            | MI    | FPQTIY/3  |

    Examples: Employee or Spouse with WP and QOL4 riders
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | Y  | N    | Y    | Employee       | FPPTI             | MI    | FPQTI4    |
      | N   | Y  | N    | Y    | Employee       | FPPTIG            | MI    | FPQTIG/4  |
      | N   | Y  | N    | Y    | Employee       | FPPTIW            | MI    | FPQTIW/4  |
      | N   | Y  | N    | Y    | Employee       | FPPTIB            | MI    | FPQTIB/4  |
      | N   | Y  | N    | Y    | Employee       | FPPTIY            | MI    | FPQTIY/4  |

      | N   | Y  | N    | Y    | Spouse         | FPPTI             | MI    | FPQTI4   |
      | N   | Y  | N    | Y    | Spouse         | FPPTIG            | MI    | FPQTIG/4  |
      | N   | Y  | N    | Y    | Spouse         | FPPTIW            | MI    | FPQTIW/4  |
      | N   | Y  | N    | Y    | Spouse         | FPPTIB            | MI    | FPQTIB/4  |
      | N   | Y  | N    | Y    | Spouse         | FPPTIY            | MI    | FPQTIY/4  |

    Examples: Employee, Spouse, or Child with single rider in MD
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | N    | N    | Employee       | FPPTI             | MD    | FPPTI MD  |
      | Y   | N  | N    | N    | Employee       | FPPTI             | MD    | FPATI/MD  |
      | N   | Y  | N    | N    | Employee       | FPPTI             | MD    | FPPTI MD  |
      | N   | N  | Y    | N    | Employee       | FPPTI             | MD    | FPQTI3/MD |
      | N   | N  | N    | Y    | Employee       | FPPTI             | MD    | FPQTI4/MD |

      | N   | N  | N    | N    | Spouse         | FPPTI             | MD    | FPPTI MD  |
      | Y   | N  | N    | N    | Spouse         | FPPTI             | MD    | FPATI/MD  |
      | N   | Y  | N    | N    | Spouse         | FPPTI             | MD    | FPPTI MD  |
      | N   | N  | Y    | N    | Spouse         | FPPTI             | MD    | FPQTI3/MD |
      | N   | N  | N    | Y    | Spouse         | FPPTI             | MD    | FPQTI4/MD |

      | N   | N  | N    | N    | Child          | FPPTI             | MD    | FPPTID MD |

    Examples: Employee, Spouse, or Child with single rider in UT
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | N    | N    | Employee       | FPPTI             | UT    | FPPTI UT  |
      | Y   | N  | N    | N    | Employee       | FPPTI             | UT    | FPATI/UT  |
      | N   | Y  | N    | N    | Employee       | FPPTI             | UT    | FPPTI UT  |
      | N   | N  | Y    | N    | Employee       | FPPTI             | UT    | FPQTI3/UT |
      | N   | N  | N    | Y    | Employee       | FPPTI             | UT    | FPQTI4/UT |

      | N   | N  | N    | N    | Spouse         | FPPTI             | UT    | FPPTI UT  |
      | Y   | N  | N    | N    | Spouse         | FPPTI             | UT    | FPATI/UT  |
      | N   | Y  | N    | N    | Spouse         | FPPTI             | UT    | FPPTI UT  |
      | N   | N  | Y    | N    | Spouse         | FPPTI             | UT    | FPQTI3/UT |
      | N   | N  | N    | Y    | Spouse         | FPPTI             | UT    | FPQTI4/UT |

      | N   | N  | N    | N    | Child          | FPPTI             | UT    | FPPTID UT |

    Examples: Employee or Spouse with multiple riders in MD
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code  |
      | Y   | Y  | N    | N    | Employee       | FPPTI             | MD    | None       |
      | Y   | N  | Y    | N    | Employee       | FPPTI             | MD    | FPATI/MD   |
      | Y   | N  | N    | Y    | Employee       | FPPTI             | MD    | FPATI4/MD  |
      | N   | Y  | Y    | N    | Employee       | FPPTI             | MD    | FPQTI3/MD  |
      | N   | Y  | N    | Y    | Employee       | FPPTI             | MD    | FPQTI4/MD  |

      | Y   | Y  | N    | N    | Spouse         | FPPTI             | MD    | None       |
      | Y   | N  | Y    | N    | Spouse         | FPPTI             | MD    | FPATI/MD   |
      | Y   | N  | N    | Y    | Spouse         | FPPTI             | MD    | FPATI4/MD  |
      | N   | Y  | Y    | N    | Spouse         | FPPTI             | MD    | FPQTI3/MD |
      | N   | Y  | N    | Y    | Spouse         | FPPTI             | MD    | FPQTI4/MD |

    Examples: Employee or Spouse with multiple riders in MD
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code  |
      | Y   | Y  | N    | N    | Employee       | FPPTI             | UT    | None       |
      | Y   | N  | Y    | N    | Employee       | FPPTI             | UT    | FPATI/UT   |
      | Y   | N  | N    | Y    | Employee       | FPPTI             | UT    | FPATI4/UT  |
      | N   | Y  | Y    | N    | Employee       | FPPTI             | UT    | FPQTI3/UT  |
      | N   | Y  | N    | Y    | Employee       | FPPTI             | UT    | FPQTI4/UT  |

      | Y   | Y  | N    | N    | Spouse         | FPPTI             | UT    | None       |
      | Y   | N  | Y    | N    | Spouse         | FPPTI             | UT    | FPATI/UT   |
      | Y   | N  | N    | Y    | Spouse         | FPPTI             | UT    | FPATI4/UT  |
      | N   | Y  | Y    | N    | Spouse         | FPPTI             | UT    | FPQTI3/UT |
      | N   | Y  | N    | Y    | Spouse         | FPPTI             | UT    | FPQTI4/UT |


    Examples: FPPCI plan code combinations
      | AIR | WP | QOL3 | QOL4 | Applicant Type | Base Product Code | State | Plan Code |
      | N   | N  | N    | N    | Employee       | FPPCI             | MI    | INDFPP    |
      | N   | N  | N    | N    | Spouse         | FPPCI             | MI    | INDFPP    |
      | N   | N  | N    | N    | Child          | FPPCI             | MI    | INDFPD    |
      | N   | N  | N    | N    | Employee       | FPPCI             | UT    | INDFPP/UT    |
      | N   | N  | N    | N    | Spouse         | FPPCI             | UT    | INDFPP/UT    |
      | N   | N  | N    | N    | Child          | FPPCI             | UT    | INDFPD/UT    |
