Feature: Certificate

  Background:
    Given Alice has a new secp256r1 account with private key "AJQn2L4EhJhQh2NX5NvyDDB5BUPuiZBiNRmqRcSmj3g7"
    And Alice has 10 lto
    Given Bob has an account with 10 lto

  Scenario: Submit a valid certificate
    When Alice submits certificate "valid-cert.pem"
    Then the certificate for Alice matches
      | field      | value                         |
      | status     | untrusted                     |
      | subject    | CN="Alice,O=Example Ltd,C=NL" |
      | issuer     | CN="Alice,O=Example Ltd,C=NL" |
    And Alice has 5 lto

  Scenario: Submit an expired certificate
    When Alice submits certificate "expired-cert.pem"
    Then the certificate for Alice matches
      | field      | value                         |
      | status     | expired                       |
      | subject    | CN="Alice,O=Example Ltd,C=NL" |

  Scenario: Submit an empty certificate
    Given Alice has a certificate
    When Alice submits an empty certificate
    Then there is no certificate for Alice

   Scenario: Submit a certificate issued to another account
    When Bob tries to submit certificate "valid-cert.pem"
    Then the transaction fails
