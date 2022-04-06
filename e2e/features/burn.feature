Feature: Burn

  Background: Anchor setup
    Given Alice has an account with 1000 lto

  Scenario: Burn tokens
    When Alice burns 500 lto
    Then Alice has 499 lto