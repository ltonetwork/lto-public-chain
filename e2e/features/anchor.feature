Feature: Anchor

  Background: Anchor setup
    Given Alice has a new account

  Scenario Outline: Successful anchor transaction
    Given Karen has an <key_type> account with 5 lto
    When Karen anchors (<version>) "1234"
    Then Karen has 4.65 lto
    # Endpoint /address/transactions is broken
    #And there is an anchor transaction with hash "1234" signed by Alice

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful anchor transaction because of insufficient funds
    Given Alice has 0 lto
    When Alice tries to anchor
    Then the transaction fails
