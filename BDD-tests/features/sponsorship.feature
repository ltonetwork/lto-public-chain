Feature: Sponsorship Transaction

  Scenario Outline: Successfull sponsorship transaction
    Given "<user1>" has "<initial amount>" lto
    And "<user1>" "<is not>" sponsoring "<user2>"
    And "<user1>" has "<initial amount>" lto
    When "<user1>" sponsor "<user2>"
    Then "<user1>" has "<final amount>" lto
    And "<user1>" "<is>" sponsoring "<user2>"

    Examples:
    | user1 | initial amount | is not | user2 | is | final amount |
    | Alice |       100      |   not  |  Bob  | is |     95       |

  Scenario Outline: Unsuccessful Sponsorship transaction
    Given "<Alice>" has "<no>" lto
    Then "<Alice>" "<transaction>" transaction fails

    Examples:
    | Alice  | no | transaction |
    | Alice  | 0  | sponsor     |


Scenario Outline: Successfull CancelSponsorship transaction
    Given "<user1>" has "<initial amount>" lto
    And "<user1>" "<initial state>" sponsoring "<user2>"
    And "<user1>" has "<initial amount>" lto
    When "<user1>" cancel the sponsorship for "<user2>"
    Then "<user1>" has "<final amount>" lto
    And "<user1>" "<final state>" sponsoring "<user2>"

    Examples:
    | user1 | initial amount | initial state | user2 | final state | final amount |
    | Alice |       100      |   is          |  Bob  | is not      |     95       |

Scenario Outline: Unsuccessful Cancel Sponsorship transaction
  Given "<Alice>" has "<no>" lto
  Then "<Alice>" "<transaction>" transaction fails

  Examples:
  | Alice  | no |  transaction          |
  | Alice  | 10  |  cancelSponsorship   |


