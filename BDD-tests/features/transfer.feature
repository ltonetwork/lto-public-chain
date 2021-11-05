Feature: Transfer Transaction

  Scenario: Successful Transfer transaction
    Given Alice has 100 lto
    And Bob has 0 lto
    When Alice transfers 10 lto to Bob
    Then Alice has 89 lto
    And Bob has 10 lto


  Scenario: Unsuccessful transfer transaction
    Given Alice has 0 lto
    When Alice tries to transfer 10 lto to Bob
    Then The transaction fails
