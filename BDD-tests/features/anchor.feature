Feature: Anchor

  Background: Anchor setup
    Given Alice has a new account

  Scenario: Successful anchor transaction
    Given Alice has 5 lto
    When "Alice" anchors "1234"
    Then Alice has 4.65 lto
    # Endpoint /address/transactions is broken
    #And There is an anchor transaction with hash "1234" signed by Alice

  Scenario: Unsuccessful anchor transaction
    Given Alice has 0 lto
    When Alice tries to anchor
    Then The transaction fails


  Scenario: My test
    Given Alice has 3 lto
    And Alice has 2 lto