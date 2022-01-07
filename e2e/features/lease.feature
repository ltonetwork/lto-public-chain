Feature: Lease

  Background:
    Given Alice has a new account
    And Bob has a new account

  Scenario Outline: Successful lease
    Given Karen as a new <key_type> account
    Given Karen has 10 lto
    When Karen leases (<version>) 5 lto to Bob
    Then Karen has 9 lto
    And Karen is leasing 5 lto to Bob

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful lease due to insufficient balance
    When Alice tries to lease 10 lto to Bob
    Then the transaction fails

  Scenario Outline: Successful cancel lease transaction
    Given Karen as a new <key_type> account
    Given Karen is leasing 10 lto to Bob
    And Karen has 10 lto
    When Karen cancel the lease (<version>) to Bob
    Then Karen has 5 lto
    And Karen is not leasing to Bob

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unable to cancel an unexisting lease
    When Alice tries to cancel the lease to Bob
    Then the transaction fails

  Scenario: Transfer fails because LTO intended for transfer is being leased
    Given Alice has 10 lto
    And Alice is leasing 5 lto to Bob
    When Alice tries to transfer 6 lto to Bob
    Then the transaction fails
    And Alice has 10 lto


