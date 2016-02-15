# Created by zach at 12/10/15
Feature: TAA should allow only certain combinations of riders on some base products.


  Scenario: Ensure AIR rider is allowed by itself on FPPTI product.
    Given I am setting up a case enrolling the 'FPPTI' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | Y   | N  | N    | N    |
    Then It should say the riders are allowed.

  Scenario Outline: Ensure AIR is never allowed with WP
    Given I am setting up a case enrolling the '<Product Code>' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | Y   | Y  | N    | N    |
    Then It should show that the riders 'AIR' and 'WP' cannot be combined.

    Examples:
      | Product Code |
      | FPPTI        |
      | FPPTIW       |
      | FPPTIY       |
      | FPPTIB       |

  Scenario: Ensure AIR rider is not allowed on FPP-Gov.
    Given I am setting up a case enrolling the 'FPP-Gov' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | Y   | N  | N    | N    |
    Then It should show that the rider 'AIR' is not compatible with this product.


  Scenario: Ensure QOL3 rider _is_ allowed on FPP-Gov if WP is configured.
    Given I am setting up a case enrolling the 'FPP-Gov' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | N   | Y  | Y    | N    |
    Then It should say the riders are allowed.

  Scenario: Ensure QOL4 rider _is_ allowed on FPP-Gov if WP is configured.
    Given I am setting up a case enrolling the 'FPP-Gov' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | N   | Y  | N    | Y    |
    Then It should say the riders are allowed.

  Scenario: Ensure QOL rider is allowed by itself on FPP-Gov.
    Given I am setting up a case enrolling the 'FPP-Gov' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | N   | N  | Y    | N    |
    Then It should say the riders are allowed.

  Scenario: Ensure WP rider is allowed by itself on FPP-Gov.
    Given I am setting up a case enrolling the 'FPP-Gov' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | N   | Y  | N    | N    |
    Then It should say the riders are allowed.


  Scenario: Ensure WP rider is allowed on FPP-CI
    Given I am setting up a case enrolling the 'FPPCI' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | N   | Y  | N    | N    |
    Then It should say the riders are allowed.

  Scenario: Ensure QOL and AIR riders are not allowed on FPP-CI
    Given I am setting up a case enrolling the 'FPPCI' base product
    When I check the following riders for compatibility
      | AIR | WP | QOL3 | QOL4 |
      | Y   | N  | Y    | N    |
    Then It should show that the rider 'AIR' is not compatible with this product.
    And It should show that the rider 'QOL3' is not compatible with this product.
