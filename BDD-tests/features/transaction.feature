Feature: Transfer Transaction

  Scenario Outline: Successfull Transfer transaction
    Given "<Alice>" has "<100>" lto
    And "<Bob>" has "<0>" lto
    When "<Alice>" transfer "<10>" lto to "<Bob>"
    Then "<Alice>" has "<89>" lto
    And "<Bob>" has "<10>" lto

    Examples:
    | Alice | Bob |  100 |  89  |  10  | 0 |
    | Alice | Bob |  100 |  89  |  10  | 0 |

  Scenario Outline: Unsuccessful transfer transaction
    Given "<Alice>" has "<no>" lto
    Then "<Alice>" "<transaction>" transaction fails

    Examples:
    | Alice  | no | transaction |
    | Alice  | 0  |  transfer   |