Feature: Mass Transfer

  Background:
    Given Alice has a new account
    And Bob has a new account
    And Charlie has a new account

  Scenario Outline: Successful mass-transfer
    Given Alice has 10 lto
    When Alice does a mass-transfer (<version>) of 2 lto to Bob and 1 lto to Charlie
    Then Alice has 5.8 lto
    And Bob has 2 lto
    And Charlie has 1 lto

    Examples:
      | version |
      | v1      |
      | v3      |

  Scenario: Unsuccessful mass-transfer due to insufficient founds
    When Alice tries to do a mass-transfer of 10 lto to Bob and 15 lto to Charlie
    Then the transaction fails

