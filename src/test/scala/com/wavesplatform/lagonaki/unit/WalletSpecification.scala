package com.wavesplatform.lagonaki.unit

import java.io.File
import java.nio.file.Files

import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.state.ByteStr
import com.wavesplatform.wallet.Wallet
import org.scalatest.{FunSuite, Matchers}

class WalletSpecification extends FunSuite with Matchers {

  private val walletSize = 10
  val w                  = Wallet(WalletSettings(None, "cookies", ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))

  test("wallet - acc creation") {
    w.generateNewAccounts(walletSize)

    w.privateKeyAccounts.size shouldBe walletSize
    w.privateKeyAccounts.map(_.address) shouldBe Seq(
      "3N8ZfrtnSjrTNTc3P3uRu4A9NB1wBMgxnTJ",
      "3N22PiounrgKHTAGWSXHLUc6EWTWiaY7nX9",
      "3MwHWrpSL8qaJrXquMa6UZQAVHFms8rMnYL",
      "3MvVU2dcNxkgXXcATnGu5M7NX1i1kcDxkXb",
      "3MvjaBpcAuwRBpn2tYivuTXY3aHn7SjnEyh",
      "3MxrPVU6kbCVp87gf4YkACNnhx18Xeu5awK",
      "3Myx7npu7q3CUw5YKo3kmwshc16G7pCiWhS",
      "3N9En8QSFtTiKna8V7LD9nH4grb3viyU1RE",
      "3MsGpxLgjELXcmjpm73vUs1uuRoAw3HzAUr",
      "3N8KGxGwwhbyCGUjUbXsz8uA4RtM48doT6y"
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
