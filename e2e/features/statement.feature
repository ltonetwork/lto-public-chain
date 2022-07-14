Feature: Statement

  Background: Statement setup
    Given Alice has an account with 10 lto

  Scenario Outline: Successful statement transaction
    Given Karen has an <key_type> account with 5 lto
    When Karen makes a statement (<version>) of type 1
    Then Karen has 4.5 lto
    # Endpoint /address/transactions is broken
    #And there is an statement transaction with hash "1234" signed by Alice

    Examples:
      | version | key_type  |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful statement transaction because of insufficient funds
    Given Alice has 0 lto
    When Alice tries to make a statement
    Then the transaction fails

  Scenario Outline: Statement with different data types
    When Alice makes a statement with data "<key>" is <value>
    Then there is an statement transaction with data "<key>" is <value> signed by Alice

    Examples:
      | key       | value   |
      | my_string | "hello" |
      | my_int    | 10      |
      | my_bool   | true    |