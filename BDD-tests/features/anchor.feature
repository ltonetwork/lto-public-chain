Feature: Anchor Transaction

  Scenario: Successful anchor transaction
    Given Alice has 100 lto
    When Alice anchors 1234
    Then The transaction is successful
    And Alice has 99.65 lto



  Scenario: Unsuccessful anchor transaction
    Given Alice has 0 lto
    When Alice tries to anchor
    Then The transaction fails

