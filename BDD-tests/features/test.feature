Feature: Anchor Transaction

  Scenario Outline: Successfull anchor transaction
    Given <user> has <start> lto
    When <user> make an anchor transaction
    Then <user> has <finish> lto

    Examples:
      | user  | start | finish |
      | Alice | 100   | 99.65  |

