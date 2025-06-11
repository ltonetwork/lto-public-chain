Feature: Register

  Background: Register setup
    Given Alice has an account with 5 lto

  Scenario Outline: Successful register transaction
    Given Karen has an account with 5 lto
    When Karen registers (<version>) a <key_type> account
    Then Karen has 4.65 lto

    Examples:
      | version | key_type  |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Register a bls12-381 account
    When Alice registers a bls12-381 account with public key "6aeaDwyiCaA9bmigRWq9cQ1YnBnWi9FDbXb2McLaHtmYbzZLENGjGPgPsj6cBdKZsW"
    Then Alice has 4.65 lto

  Scenario: Unable to register an account with invalid public key
    When Alice tries to register a ed25519 account with public key "C4zLBLkGgUu34tx6S95NWmEpPdW"
    Then the transaction fails

  Scenario: Unsuccessful register transaction because of insufficient funds
    Given Bob has a new account
    And Bob has 0 lto
    When Bob tries to register an account
    Then the transaction fails
