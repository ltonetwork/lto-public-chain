Feature: Sponsored transaction

  Background:
    Given Alice has a new account
    And Bob has a new account
    And Charlie has a new account
    And Dick has a new account

  Scenario: Basic sponsored transaction
    Given Alice has 5 lto
    And Bob has 3 lto
    When Bob anchors "1234" sponsored by Alice
    Then Alice has 4.65 lto
    And Bob has 3 lto


  Scenario: Sponsored anchor transaction from sponsored account
    Given Charlie is sponsoring Alice
    And Alice has 5 lto
    And Charlie has 5 lto
    And Bob has 5 lto
    When Bob anchors "1234" sponsored by Alice
    Then Bob has 5 lto
    And Alice has 5 lto
    And Charlie has 4.65 lto



