package com.ltonetwork.wallet

import com.ltonetwork.settings.WalletSettings
import com.ltonetwork.state.ByteStr
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.Files

class WalletSpecification extends AnyFunSuite with Matchers {

  private val walletSize = 10
  def newWallet1() = Wallet(WalletSettings(None, "cookies", ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption, None, None))
  def newWallet2() = Wallet(WalletSettings(None, "cookies", None, None, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))

  test("wallet from accountSeed") {
    val w2       = newWallet2()
    val accounts = w2.privateKeyAccounts
    accounts.size shouldBe 1
  }

  test("wallet - accs creation") {
    val w = newWallet1()
    w.generateNewAccounts(walletSize)

    w.privateKeyAccounts.size shouldBe walletSize + 1
    w.privateKeyAccounts.map(_.address) shouldBe Seq(
      "3MsY36nMfpp3PgH7q5J7upa2WbNkjz9eYnN",
      "3N14vexKx7EpVy4iSBCyG3esoEggqSvZvn5",
      "3N2qbDFCL1R568R59X56SduiRGzj4C2ACmE",
      "3MuCs3n1eKQyzJuyBhP5gAr3AJKmNttB3DA",
      "3MvcGCU86aUuo5h1BvNPcvWMfy2UMuYQ6ew",
      "3MpxGeBAXyz34PZyT5TAswqGieds45oJKeu",
      "3N5qLUu7Tjopj8KLeUT2RyRCEFc12TZb6Ui",
      "3MryuhLhpfSFkGLFswTxyPd3ruxDCZk6qZ9",
      "3MvaWJxhPYXwskNqnQZqvW5eWNhVqr8Lrqi",
      "3MvSswYmUL83nrAZDgqC3yXaoTD4TxJycxz",
      "3N7vtzezG94RF59qgEPuu7GYS92x6rVdu3h"
    )
  }
  test("wallet - one acc creation") {
    val w = newWallet1()
    val r = w.generateNewAccount()
    r.right.get.nonEmpty shouldBe true
  }
  test("wallet - acc deletion") {
    val w = newWallet1()
    w.generateNewAccount()
    w.generateNewAccount()
    w.privateKeyAccounts.size shouldBe 3

    w.deleteAccount(w.privateKeyAccounts.head)
    w.privateKeyAccounts.size shouldBe 2

    w.privateKeyAccounts.foreach(w.deleteAccount)
    assert(w.privateKeyAccounts.isEmpty)
  }

  test("reopening") {
    val walletFile = Some(createTestTemporaryFile("wallet", ".dat"))

    val w1 = Wallet(WalletSettings(walletFile, "cookies", ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption, None, None))
    w1.generateNewAccounts(10)
    val w1privateKeyAccounts = w1.privateKeyAccounts
    w1.privateKeyAccounts.nonEmpty shouldBe true
    val w1nonce = w1.nonce

    val w2 = Wallet(WalletSettings(walletFile, "cookies", None, None, None))
    w2.privateKeyAccounts.nonEmpty shouldBe true
    w2.privateKeyAccounts shouldEqual w1privateKeyAccounts
    w2.nonce shouldBe w1nonce
  }

  test("reopening with accountSeed") {
    val walletFile = Some(createTestTemporaryFile("wallet", ".dat"))

    val w1                   = Wallet(WalletSettings(walletFile, "cookies", None, None, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))
    val w1privateKeyAccounts = w1.privateKeyAccounts
    w1.privateKeyAccounts.nonEmpty shouldBe true
    w1.accountSeed.nonEmpty shouldBe true
    w1.seed shouldBe 'left
    w1.save()

    val w2 = Wallet(WalletSettings(walletFile, "cookies", None, None, None))
    ByteStr(w2.accountSeed.get) shouldEqual ByteStr(w1.accountSeed.get)
    w2.seed shouldBe 'left
    w2.privateKeyAccounts.nonEmpty shouldBe true
    w2.privateKeyAccounts shouldEqual w1privateKeyAccounts
  }

  test("reopening with seed phrase") {
    val walletFile = Some(createTestTemporaryFile("wallet", ".dat"))

    val w1                   = Wallet(WalletSettings(walletFile, "cookies", None, Some("crypto is here to stay"), None))
    val w1privateKeyAccounts = w1.privateKeyAccounts
    w1privateKeyAccounts.nonEmpty shouldBe true
    w1.accountSeed shouldBe empty
    w1.seed shouldBe 'right
    w1.save()

    val w2 = Wallet(WalletSettings(walletFile, "cookies", None, None, None))
    w2.accountSeed shouldBe None
    w2.privateKeyAccounts.nonEmpty shouldBe true
    w2.privateKeyAccounts shouldEqual w1privateKeyAccounts
    ByteStr(w2.seed.right.get) shouldEqual ByteStr(w1.seed.right.get)
  }

  test("reopen with incorrect password") {
    val file = Some(createTestTemporaryFile("wallet", ".dat"))
    val w1   = Wallet(WalletSettings(file, "password", ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption, None, None))
    w1.generateNewAccounts(3)

    assertThrows[IllegalStateException] {
      Wallet(WalletSettings(file, "incorrect password", None, None, None))
    }
  }

  def createTestTemporaryFile(name: String, ext: String): File = {
    val file = Files.createTempFile(name, ext).toFile
    file.deleteOnExit()

    file
  }
}
