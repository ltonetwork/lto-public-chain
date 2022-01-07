Feature: Association

  Background: Association setup
    Given Alice has a new account
    And Bob has a new account

  Scenario Outline: Successful issue association
    Given Karen has a new <key_type> account
    Given Karen does not have an association with Bob of type 1
    And Karen has 10 lto
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
    Then The transaction fails

  # Wait after association because of https://github.com/ltonetwork/lto-public-chain/issues/106
  Scenario Outline: Successful revoke association
    Given Karen has a new <key_type> account
    Given Karen has an association with Bob of type 1
    And Karen has 10 lto
    And wait
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
    Then The transaction fails

  @skip
  # https://github.com/ltonetwork/lto-public-chain/issues/103
  Scenario: Reissue a revoked association
    Given Alice has an association with Bob of type 5
    And Alice has 10 lto
    When Alice revokes the association with Bob of type 5
    And Alice tries to issues an association with Bob of type 5
    Then Alice has 8 lto
    And Alice is not associated with Bob
    
  # Wait after association because of https://github.com/ltonetwork/lto-public-chain/issues/106
  Scenario: Revoke association with anchor
    Given Alice has an association with Bob of type 76 and anchor qwerty
    And Alice has 10 lto
    And wait
    When Alice revokes the association with Bob of type 76 and anchor qwerty
    Then Alice has 9 lto
    And Alice is not associated with Bob

