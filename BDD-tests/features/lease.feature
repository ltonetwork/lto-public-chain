Feature: Lease

  Background:
    Given Alice has a new account
    And Bob has a new account

  Scenario: Successful lease transaction
    Given Alice has 10 lto
    When Alice leases 5 lto to Bob
    Then Alice has 9 lto
    And Alice is leasing 5 lto to Bob

  Scenario: Unsuccessful lease transaction due to insufficient balance
    When Alice tries to lease 10 lto to Bob
    Then The transaction fails

  Scenario: Successful cancel lease transaction
    Given Alice is leasing 10 lto to Bob
    And Alice has 10 lto
    When Alice cancel the lease to Bob
    Then Alice has 5 lto
    And Alice is not leasing to Bob


  Scenario: Unsuccessful cancel Lease transaction, no lease present
    When Alice tries to cancel the lease to Bob
    Then The transaction fails


  Scenario: Transfer fails because the lto intended to transfer are leased
    Given Alice has 10 lto
    And Alice is leasing 5 lto to Bob
    When Alice tries to transfer 5 lto to Bob
    Then The Transaction fails



