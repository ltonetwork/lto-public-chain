Feature: Sponsored transaction

  Background:
    Given Alice has a new account
    And Bob has a new account
    And Charlie has a new account
    And Dick has a new account

  Scenario: Basic sponsored transaction
    Given Alice has 100 lto
    And Bob has 10 lto
    When Bob anchors "1234" sponsored by Alice
    Then Alice has 99.65 lto
    And Bob has 10 lto

    # give background to all of them

    # also sponsor transaction with sponsored account

    # add lto in the given

