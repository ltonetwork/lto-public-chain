Feature: Sponsorship

  Background:
    Given Alice has a new account
    And Bob has a new account
    And Charlie has a new account
    And Dick has a new account

  Scenario Outline: Successful sponsorship
    Given Karen has an <key_type> account with 10 lto
    When Karen sponsors (<version>) Bob
    Then Karen has 5 lto
    And Karen is sponsoring Bob

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: The sponsoring account pays for the transaction costs
    Given Alice is sponsoring Bob
    And Bob has 10 lto
    And Charlie has 0 lto
    And Alice has 5 lto
    When Bob transfers 5 lto to Charlie
    Then Alice has 4 lto
    And Bob has 5 lto
    And Charlie has 5 lto

  Scenario: Sponsorship fall through to sender
    Given Alice is sponsoring Bob
    And Bob has 10 lto
    And Charlie has 0 lto
    And Alice has 0 lto
    When Bob transfers 5 lto to Charlie
    Then Alice has 0 lto
    And Bob has 4 lto
    And Charlie has 5 lto

  Scenario: Sponsorship with second sponsor
    Given Alice is sponsoring Bob
    And Dick is sponsoring Bob
    And Alice has 10 LTO
    And Bob has 10 LTO
    And Charlie has 0 LTO
    And Dick has 10 LTO
    When Bob transfers 5 lto to Charlie
    Then Alice has 10 lto
    And Dick has 9 LTO
    And Bob has 5 lto
    And Charlie has 5 lto

  Scenario: Sponsorship fall through to second sponsor
    Given Alice is sponsoring Bob
    And Dick is sponsoring Bob
    And Bob has 10 LTO
    And Charlie has 0 LTO
    And Alice has 10 LTO
    And Dick has 0 LTO
    When Bob transfers 5 lto to Charlie
    Then Alice has 9 lto
    And Dick has 0 lto
    And Bob has 5 lto
    And Charlie has 5 lto

  Scenario: Unsuccessful sponsorship transaction due to insufficient balance
    Given Alice has 0 lto
    When Alice tries to sponsor Dick
    Then the transaction fails

Scenario Outline: Successful cancel sponsorship
    Given Karen has an <key_type> account with 6 lto
    Given Karen is sponsoring Bob
    When Karen cancels the sponsorship (<version>) for Bob
    Then Karen has 5 lto
    And Karen is not sponsoring Bob

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful cancel sponsorship due to insufficient balance
    Given Alice has 0 lto
    When Alice tries to cancel the sponsorship for Bob
    Then the transaction fails


