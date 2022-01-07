Feature: Register

  Background: Register setup
    Given Alice has a new account

  Scenario Outline: Successful register transaction
    Given Karen has a new <key_type> account
    And Karen has 5 lto
    When Karen registers (<version>) an account
    Then Karen has 4.65 lto

    Examples:
      | version | key_type  |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful register transaction because of insufficient funds
    Given Alice has 0 lto
    When Alice tries to register an account
    Then the transaction fails
