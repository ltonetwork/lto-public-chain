Feature: Sponsored transaction

  Background:
    Given Alice has a new account
    And Bob has a new account
    And Charlie has a new account
    And Dick has a new account

  Scenario Outline: Sponsored transaction
    Given Karen has an <key_type> account with 5 lto
    And Bob has 3 lto
    When Bob anchors "1234" sponsored by Karen
    Then Karen has 4.65 lto
    And Bob has 3 lto

    Examples:
      | key_type  |
      | ed25519   |
      | secp256k1 |
      | secp256r1 |

  Scenario: Sponsored transaction from sponsored account
    Given Charlie is sponsoring Alice
    And Alice has 5 lto
    And Charlie has 5 lto
    And Bob has 5 lto
    When Bob anchors "1234" sponsored by Alice
    Then Bob has 5 lto
    And Alice has 5 lto
    And Charlie has 4.65 lto

  Scenario: Unsuccessful sponsored transaction due to insufficient funds
    Given Alice has 0 lto
    And Bob has 3 lto
    When Bob tries to anchor "1234" sponsored by Alice
    Then the transaction fails

