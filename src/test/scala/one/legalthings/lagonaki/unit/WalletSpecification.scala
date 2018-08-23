package one.legalthings.lagonaki.unit

import java.io.File
import java.nio.file.Files

import one.legalthings.settings.WalletSettings
import one.legalthings.state.ByteStr
import one.legalthings.wallet.Wallet
import org.scalatest.{FunSuite, Matchers}

class WalletSpecification extends FunSuite with Matchers {

  private val walletSize = 10
  val w                  = Wallet(WalletSettings(None, "cookies", ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))

  test("wallet - acc creation") {
    w.generateNewAccounts(walletSize)

    w.privateKeyAccounts.size shouldBe walletSize
    w.privateKeyAccounts.map(_.address) shouldBe Seq(
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

  test("wallet - acc deletion") {

    val head = w.privateKeyAccounts.head
    w.deleteAccount(head)
    assert(w.privateKeyAccounts.lengthCompare(walletSize - 1) == 0)

    w.deleteAccount(w.privateKeyAccounts.head)
    assert(w.privateKeyAccounts.lengthCompare(walletSize - 2) == 0)

    w.privateKeyAccounts.foreach(w.deleteAccount)

    assert(w.privateKeyAccounts.isEmpty)
  }

  test("reopening") {
    val walletFile = Some(createTestTemporaryFile("wallet", ".dat"))

    val w1 = Wallet(WalletSettings(walletFile, "cookies", ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))
    w1.generateNewAccounts(10)
    val w1privateKeyAccounts = w1.privateKeyAccounts
    val w1nonce              = w1.nonce

    val w2 = Wallet(WalletSettings(walletFile, "cookies", None))
    w2.privateKeyAccounts.nonEmpty shouldBe true
    w2.privateKeyAccounts shouldEqual w1privateKeyAccounts
    w2.nonce shouldBe w1nonce
  }

  test("reopen with incorrect password") {
    val file = Some(createTestTemporaryFile("wallet", ".dat"))
    val w1   = Wallet(WalletSettings(file, "password", ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))
    w1.generateNewAccounts(3)

    assertThrows[IllegalStateException] {
      Wallet(WalletSettings(file, "incorrect password", None))
    }
  }

  def createTestTemporaryFile(name: String, ext: String): File = {
    val file = Files.createTempFile(name, ext).toFile
    file.deleteOnExit()

    file
  }
}
