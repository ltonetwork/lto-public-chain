Feature: Lease transaction

  Background:
    Given Alice has a new account with 10 LTO
    And Bob has a new account with 10 LTO

  Scenario: Successful lease transaction
    Given Alice has 100 lto
    When Alice leases 10 lto to Bob
    Then Alice has 99 lto
    And Alice is leasing Bob

  Scenario: Unsuccessful lease transaction due to insufficient balance
    Given Alice has 100 lto
    And Alice is not leasing to Bob
    And Alice has 0 lto
    When Alice tries to lease 10 lto to Bob
    Then The transaction fails

  Scenario: Successful cancel lease transaction
    Given Alice has 100 lto
    And Alice is leasing to Bob
    And Alice has 100 lto
    When Alice cancel the lease to Bob
    Then Alice has 95 lto
    And Alice is not leasing Bob

# it is not possible to not have the balance to cancel the lease

  Scenario: Unsuccessful cancel Lease transaction, no lease present
    Given Alice is not leasing to Bob
    When Alice tries to cancel the lease to Bob
    Then The transaction fails



