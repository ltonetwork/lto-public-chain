Feature: Transfer

  Background: Transfer set up
    Given Alice has a new account
    And Bob has a new account

  Scenario Outline: Successful transfer
    Given Karen has a new <key_type> account
    Given Karen has 15 lto
    And Bob has 0 lto
    When Karen transfers (<version>) 5 lto to Bob
    Then Karen has 9 lto
    And Bob has 5 lto

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful transfer due to insufficient funds
    Given Alice has 0 lto
    When Alice tries to transfer 10 lto to Bob
    Then the transaction fails
