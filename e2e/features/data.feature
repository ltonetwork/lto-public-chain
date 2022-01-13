Feature: Data

  Scenario Outline: Set data for an account
    Given Karen has an <key_type> account with 5 lto
    When Karen sets data (<version>) "foo" to "bar"
    Then Karen has 3.9 lto
    And Karen has data "foo" with value "bar"

    Examples:
      | version | key_type  |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario Outline: Set data with different data types
    Given Alice has an account with 10 lto
    When Alice sets data "<key>" to <value>
    Then Alice has data "<key>" with value <value>

    Examples:
      | key       | value   |
      | my_string | "hello" |
      | my_int    | 10      |
      | my_bool   | true    |

  Scenario: Unsuccessful data transaction because of insufficient funds
    Given Alice has a new account
    When Alice tries to set data "foo" to "bar"
    Then the transaction fails
