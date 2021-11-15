Feature: Mass Transfer

  Background:
    Given Alice has a new account
    And Bob has a new account
    And Charlie has a new account

  Scenario: Successful mass-transfer transaction
    Given Alice has 10 lto
    When Alice does a mass-transfer of 2 lto to Bob and 1 lto to Charlie
    Then Alice has 5.8 lto
    And Bob has 2 lto
    And Charlie has 1 lto

  Scenario: Unsuccessful mass-transfer transaction due to insufficient founds
    When Alice tries to do a mass-transfer of 10 lto to Bob and 15 lto to Charlie
    Then The transaction fails

