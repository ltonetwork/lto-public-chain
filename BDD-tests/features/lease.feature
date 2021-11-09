Feature: Lease

  Background:
    Given Alice has a new account
    And Bob has a new account

  Scenario: Successful lease transaction
    Given Alice has 10 lto
    When Alice leases 5 lto to Bob
    Then Alice has 9 lto
    And Alice is leasing to Bob

  Scenario: Unsuccessful lease transaction due to insufficient balance
    When Alice tries to lease 10 lto to Bob
    Then The transaction fails

  Scenario: Successful cancel lease transaction
    Given Alice is leasing to Bob
    And Alice has 10 lto
    When Alice cancel the lease to Bob
    Then Alice has 5 lto
    And Alice is not leasing to Bob

# it is not possible to not have the balance to cancel the lease

  Scenario: Unsuccessful cancel Lease transaction, no lease present
    When Alice tries to cancel the lease to Bob
    Then The transaction fails



