Feature: Association

  Background: Association setup
    Given Alice has an account with 10 lto
    And Bob has a new account

  Scenario Outline: Successful issue association
    Given Karen has an <key_type> account with 10 lto
    And Karen does not have an association with Bob of type 1
    When Karen issues an association (<version>) with Bob of type 1
    Then Karen is associated with Bob
    And Karen has 9 lto

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful issue association due to insufficient balance
    Given Alice has 0 lto
    When Alice tries to issue an association with Bob of type 1
    Then the transaction fails

  Scenario Outline: Successful revoke association
    Given Karen has an <key_type> account with 10 lto
    And Karen has an association with Bob of type 1
    When Karen revokes the association (<version>) with Bob of type 1
    Then Karen is not associated with Bob
    And Karen has 9 lto

    Examples:
      | version | key_type  |
      | v1      | ed25519   |
      | v3      | ed25519   |
      | v3      | secp256k1 |
      | v3      | secp256r1 |

  Scenario: Unsuccessful revoke association due to insufficient balance
    Given Alice has an association with Bob of type 1
    And Alice has 0 lto
    When Alice tries to revoke an association with Bob of type 1
    Then the transaction fails

  # FIXME
  @skip
  Scenario: Reissue a revoked association
    Given Alice has an association with Bob of type 5
    When Alice revokes the association with Bob of type 5
    Then Alice is not associated with Bob
    When Alice issues an association with Bob of type 5
    Then Alice is associated with Bob
    
  Scenario: Revoke association with anchor
    Given Alice has an association with Bob of type 76 and anchor qwerty
    When Alice revokes the association with Bob of type 76 and anchor qwerty
    Then Alice is not associated with Bob

  Scenario: Issue and revoke association to own account
    When Alice issues an association with Alice of type 9
    Then Alice is associated with Alice
    When Alice revokes the association with Alice of type 9
    Then Alice is not associated with Alice

