Feature: Anchor

  Scenario Outline: Successful mapped anchor transaction
    Given Karen has an <key_type> account with 5 lto
    When Karen anchors (<version>) key "foo" and value "1234"
    Then Karen has 4.65 lto
    And there is a mapped anchor transaction with key "foo" and value "1234" signed by Karen

    Examples:
      | version | key_type  |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful anchor transaction because of insufficient funds
    Given Alice has a new account
    When Alice tries to anchor key "foo" and value "1234"
    Then the transaction fails
