Feature: Transfer

  Background: Transfer set up
    Given Alice has a new account
    And Bob has a new account

  Scenario Outline: Successful Transfer transaction
    Given Alice has 15 lto
    And Bob has 0 lto
    When Alice transfers (<version>) 5 lto to Bob
    Then Alice has 9 lto
    And Bob has 5 lto

    Examples:
      | version |
      | v2      |
      | v3      |

  Scenario: Unsuccessful transfer transaction
    Given Alice has 0 lto
    When Alice tries to transfer 10 lto to Bob
    Then The transaction fails
