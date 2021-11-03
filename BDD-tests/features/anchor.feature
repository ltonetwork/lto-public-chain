Feature: Anchor Transaction

  Scenario Outline: Successfull anchor transaction
    Given "<user>" has 100 lto
    When "<user>" make an anchor transaction
    Then "<user>" has 99.65 lto

    Examples:
    | user  |
    | Alice |

  Scenario Outline: Unsuccessful anchor transaction
    Given "<user>" has 0 lto
    Then "<user>" Anchor transaction fails

    Examples:
    | user  |
    | Alice |