Feature: Register

  Background: Register setup
    Given Alice has a new account

  Scenario: Successful register transaction
    Given Alice has 5 lto
    When Alice registers (v3) an account
    Then Alice has 4.65 lto

  Scenario: Unsuccessful register transaction because of insufficient funds
    Given Alice has 0 lto
    When Alice tries to register an account
    Then the transaction fails
