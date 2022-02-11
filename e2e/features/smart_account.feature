Feature: Smart account

  Background: Anchor setup
    Given Alice has an account with 100 lto
    Given Bob has a new account
    Given Charlie has a new account

  Scenario: Create smart account
    When Alice creates a smart account with script
      """
        sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
      """
    Then Alice has a smart account
    And Alice has 95 lto


  Scenario: Clear smart account
    Given Alice has a smart account with script
      """
        sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
      """
    When Alice removes the account script
    Then Alice doesn't have a smart account
    And Alice has 95 lto

  Scenario: Modify smart account
    Given Alice has a smart account with script
      """
        sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
      """
    When Alice creates a smart account with script
      """
        true
      """
    Then Alice has a smart account
    And Alice has 95 lto

  Scenario: Transactions with smart account
    Given Alice has a smart account with script
      """
        sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
      """
    When Alice transfers 10 LTO to Bob
    And Alice does a mass-transfer of 2 lto to Bob and 1 lto to Charlie
    And Alice issues an association with Bob of type 1
    And Alice revokes the association with Bob of type 1
    And Alice sets data "foo" to "bar"
    And Alice leases 10 lto to Bob
    And Alice cancels the lease to Bob
    And Alice sponsors Bob
    And Alice cancels the sponsorship for Bob
    And Alice registers an account

  Scenario: Restricted account
    Given Alice has a smart account with script
      """
        match tx {
          case t:  TransferTransaction => false
          case mt: MassTransferTransaction => false
          case ss: SetScriptTransaction => false
          case _ => sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
        }
      """
    When Alice tries to transfer 10 LTO to Bob
    Then the transaction fails
    When Alice tries to remove the account script
    Then the transaction fails
    When Alice tries to anchor
    Then the transaction is successful
