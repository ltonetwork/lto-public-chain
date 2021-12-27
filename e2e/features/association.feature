Feature: Association

  Background: Association setup
    Given Alice has a new account
    And Bob has a new account

  Scenario Outline: Successful issue association
    Given Alice does not have an association with Bob of type 1
    And Alice has 10 lto
    When Alice issues an association (<version>) with Bob of type 1
    Then Alice is associated with Bob
    And Alice has 9 lto

    Examples:
      | version |
      | v1      |
      | v3      |

  Scenario: Unsuccessful issue association due to insufficient balance
    Given Alice has 0 lto
    When Alice tries to issue an association with Bob of type 1
    Then The transaction fails

  Scenario Outline: Successful revoke association
    Given Alice has an association with Bob of type 1
    And Alice has 10 lto
    When Alice revokes the association (<version>) with Bob of type 1
    Then Alice is not associated with Bob
    And Alice has 9 lto

    Examples:
      | version |
      | v1      |
      | v3      |

  Scenario: Unsuccessful revoke association due to insufficient balance
    Given Alice has an association with Bob of type 1
    And Alice has 0 lto
    When Alice tries to revoke an association with Bob of type 1
    Then The transaction fails

  Scenario: Reissue a revoked association
    Given Alice has an association with Bob of type 5
    And Alice has 10 lto
    When Alice revokes the association with Bob of type 5
    And Alice tries to issue an association with Bob of type 5
    Then Alice has 8 lto
    And Alice is not associated with Bob
    
  Scenario: Revoke association with anchor
    Given Alice has an association with Bob of type 76 and anchor qwerty
    And Alice has 10 lto
    When Alice revokes the association with Bob of type 76 and anchor qwerty
    Then Alice has 9 lto
    And Alice is not associated with Bob
