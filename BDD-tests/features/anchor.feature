Feature: Anchor Transaction

  Scenario Outline: Successfull anchor transaction
    Given "<Alice>" has "<some>" lto
    When "<Alice>" make an anchor transaction
    Then "<Alice>" has "<less>" lto

    Examples:
    | Alice | some | less  |
    | Alice | 100  | 99.65 |

  Scenario Outline: Unsuccessful anchor transaction
    Given "<Alice>" has "<no>" lto
    Then "<Alice>" "<transaction>" transaction fails

    Examples:
    | Alice  | no | transaction |
    | Alice  | 0  |  anchor   |