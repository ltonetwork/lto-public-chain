Feature: Mass Transfer transaction

  Scenario: Successful mass-transfer transaction
    Given Alice has 100 lto
    And Bob has 0 lto
    And Charlie has 0 lto
    When Alice does a mass-transfer of 10 lto to Bob and 15 lto to Charlie
    Then Alice has 73.8 lto
    And Bob has 10 lto
    And Charlie has 15 lto

  Scenario: Unsuccessful mass-transfer transaction due to insufficient founds
    Given Alice has 0 lto
    When Alice tries to do a mass-transfer of 10 lto to Bob and 15 lto to Charlie
    Then The transaction fails

