Feature: Mass Transfer

  Background:
    Given Alice has a new account
    And Bob has a new account
    And Charlie has a new account

  Scenario Outline: Successful mass-transfer
    Given Karen has an <key_type> account with 10 lto
    When Karen does a mass-transfer (<version>) of 2 lto to Bob and 1 lto to Charlie
    Then Karen has 5.8 lto
    And Bob has 2 lto
    And Charlie has 1 lto

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful mass-transfer due to insufficient founds
    Given Alice has 0 lto
    When Alice tries to do a mass-transfer of 10 lto to Bob and 15 lto to Charlie
    Then the transaction fails

