Feature: Anchor

  Background: Anchor setup
    Given Alice has a new account

  Scenario Outline: Successful anchor transaction
    Given Alice has 5 lto
    When Alice anchors (<version>) "1234"
    Then Alice has 4.65 lto
    # Endpoint /address/transactions is broken
    #And there is an anchor transaction with hash "1234" signed by Alice

    Examples:
      | version |
      | v1      |
      | v3      |

  Scenario: Unsuccessful anchor transaction because of insufficient funds
    Given Alice has 0 lto
    When Alice tries to anchor
    Then the transaction fails
