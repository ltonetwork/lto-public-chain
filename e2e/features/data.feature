Feature: Data

  Background: Register setup
    Given Alice has a new account

  Scenario: Set data for an account
    Given Alice has 5 lto
    When Alice sets data "foo" to "bar"
    Then Alice has 4.65 lto

  Scenario: Unsuccessful data transaction because of insufficient funds
    Given Alice has 0 lto
    When Alice tries to set data "foo" to "bar"
    Then the transaction fails
