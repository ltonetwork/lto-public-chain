Feature: Association

  Background: Association setup
    Given Alice has a new account
    And Bob has a new account

  Scenario: Successful association transaction
    Given Alice has 10 lto
    When Alice make an association with Bob
    Then Alice is associated with Bob
    And Alice has 9 lto

  Scenario: Successful revoke association transaction
    Given Alice has 10 lto
    And Alice is associated with Bob
    When Alice revoke the association with Bob
    Then Alice is not associated with Bob
    And Alice has 8 lto

  Scenario: Unsuccessful association transaction due to insufficient balance
    Given Alice has 0 lto
    When Alice tries to make an association with Bob
    Then The transaction fails