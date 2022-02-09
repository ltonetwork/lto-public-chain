Feature: Lease

  Background:
    Given Alice has a new account
    And Bob has a new account

  Scenario Outline: Successful lease
    Given Karen has an <key_type> account with 10 lto
    When Karen leases (<version>) 5 lto to Bob
    Then Karen has 9 lto
    And Karen is leasing 5 lto to Bob

    Examples:
      | version | key_type  |
      | v2      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful lease due to insufficient balance
    When Alice tries to lease 10 lto to Bob
    Then the transaction fails

  Scenario Outline: Successful cancel lease transaction
    Given Karen has an <key_type> account with 10 lto
    And Karen is leasing 10 lto to Bob
    When Karen cancels the lease (<version>) to Bob
    Then Karen has 9 lto
    And Karen is not leasing to Bob

    Examples:
      | version | key_type  |
      | v2      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unable to cancel an nonexistent lease
    When Alice tries to cancels the lease to Bob
    Then the transaction fails

  Scenario: Transfer fails because LTO intended for transfer is being leased
    Given Alice has 10 lto
    And Alice is leasing 5 lto to Bob
    When Alice tries to transfer 6 lto to Bob
    Then the transaction fails
    And Alice has 10 lto
