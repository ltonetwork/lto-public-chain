package com.ltonetwork.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.settings.WalletSettings
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError
import com.ltonetwork.transaction.ValidationError.{AccountBasedWallet, MissingSenderPrivateKey}
import com.ltonetwork.utils.{JsonFileStorage, randomBytes, _}
import play.api.libs.json._

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

trait Wallet {

  def seed: Either[ValidationError, Array[Byte]]

  def accountSeed: Option[Array[Byte]]

  def nonce: Either[ValidationError, Int]

  def privateKeyAccounts: List[PrivateKeyAccount]

  def generateNewAccounts(howMany: Int): Either[ValidationError, Seq[PrivateKeyAccount]]

  def generateNewAccount(): Either[ValidationError, Option[PrivateKeyAccount]]

  def deleteAccount(account: PrivateKeyAccount): Either[ValidationError, Boolean]

  def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount]

  def save(): Unit

}

object Wallet extends ScorexLogging {

  implicit class WalletExtension(w: Wallet) {

    def accountBased = w.accountSeed.nonEmpty

    def findPrivateKey(addressString: String): Either[ValidationError, PrivateKeyAccount] =
      for {
        acc        <- Address.fromString(addressString)
        privKeyAcc <- w.privateKeyAccount(acc)
      } yield privKeyAcc

    def exportAccountSeed(account: Address): Either[ValidationError, Array[Byte]] = w.privateKeyAccount(account).map(_.seed)

  }

  private case class WalletData(seed: ByteStr, accountSeeds: Set[ByteStr], nonce: Int, accountSeed: Option[ByteStr] = None) {
    def getAccountSeeds() =
      accountSeeds ++ accountSeed.toSet
    def copyWithAccountSeeds(ass: Set[ByteStr]) = {
      accountSeed match {
        case Some(as) => throw new IllegalStateException("this is account-seed-based wallet, can't do")
        case None     => this.copy(accountSeeds = ass)
      }
    }
    def accountBased = accountSeed.nonEmpty
  }

  private implicit val walletFormat: Format[WalletData] = Json.format

  def generateNewAccount(seed: Array[Byte], nonce: Int): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    PrivateKeyAccount(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    crypto.secureHash(Bytes.concat(Ints.toByteArray(nonce), seed))

  def apply(settings: WalletSettings): Wallet = new WalletImpl(settings.file, settings.password, settings.miningWalletSeed, settings.accountSeed)

  private class WalletImpl(maybeFile: Option[File],
                           password: String,
                           maybeSeedFromConfig: Option[ByteStr],
                           maybeAccountSeedFromConfig: Option[ByteStr])
      extends ScorexLogging
      with Wallet {

    private val key = JsonFileStorage.prepareKey(password)

    private def loadOrImport(f: File): Option[WalletData] =
      try {
        Some(JsonFileStorage.load[WalletData](f.getCanonicalPath, Some(key)))
      } catch {
        case NonFatal(_) => None
      }

    private lazy val actualSeed = maybeSeedFromConfig
      .getOrElse {
        val randomSeed = ByteStr(randomBytes(64))
        log.info(s"Your randomly generated seed is ${randomSeed.base58}")
        randomSeed
      }

    private lazy val actualAccountSeed = maybeAccountSeedFromConfig

    def loadWalletData(): WalletData = {
      if (maybeAccountSeedFromConfig.nonEmpty) WalletData(ByteStr.empty, maybeAccountSeedFromConfig.toSet, 0, maybeAccountSeedFromConfig)
      else {
        lazy val first = ByteStr(generateAccountSeed(actualSeed.arr, 0))
        if (maybeFile.isEmpty)
          WalletData(actualSeed, Set(first), 0, actualAccountSeed)
        else {
          val file = maybeFile.get
          if (file.exists() && file.length() > 0) {
            val wd = loadOrImport(maybeFile.get)
            if (wd.isDefined) wd.get
            else {
              throw new IllegalStateException(s"Failed to open existing wallet file '${maybeFile.get}' maybe provided password is incorrect")
            }
          } else WalletData(actualSeed, Set(first), 0, actualAccountSeed)
        }
      }
    }
    private var walletData: WalletData = loadWalletData()

    private val l = new Object

    private def lock[T](f: => T): T = l.synchronized(f)

    private def readAccountsFromWalletData: TrieMap[String, PrivateKeyAccount] = {
      val accounts = walletData.getAccountSeeds().map(seed => PrivateKeyAccount(seed.arr))
      TrieMap(accounts.map(acc => acc.address -> acc).toSeq: _*)
    }

    override def save(): Unit = maybeFile.foreach(f => JsonFileStorage.save(walletData, f.getCanonicalPath, Some(key)))

    private def generateAndSave(): Option[PrivateKeyAccount] = lock {
      val nonce = incrementAndGetNonce()
      import com.ltonetwork.state._
      val account = Wallet.generateNewAccount(seed.explicitGet(), nonce) // called from guarded public methods only

      val address = account.address
      if (!readAccountsFromWalletData.contains(address)) {
        walletData = walletData.copyWithAccountSeeds(walletData.getAccountSeeds() + ByteStr(account.seed))
        log.info("Added account #" + privateKeyAccounts.size)
        save()
        Some(account)
      } else None
    }

    override def seed = Either.cond(!walletData.accountBased, walletData.seed.arr, AccountBasedWallet)

    override def accountSeed: Option[Array[Byte]] = walletData.accountSeed.map(_.arr)

    override def privateKeyAccounts: List[PrivateKeyAccount] = readAccountsFromWalletData.values.toList

    override def generateNewAccounts(howMany: Int) =
      if (walletData.accountBased) Left(AccountBasedWallet)
      else
        Right {
          (1 to howMany).flatMap(_ => generateAndSave())
        }

    override def generateNewAccount() = lock {
      if (walletData.accountBased) Left(AccountBasedWallet)
      else
        Right {
          generateAndSave()
        }
    }

    override def deleteAccount(account: PrivateKeyAccount) = lock {
      if (walletData.accountBased) Left(AccountBasedWallet)
      else
        Right {
          val before = walletData.getAccountSeeds().size
          walletData = walletData.copyWithAccountSeeds(walletData.getAccountSeeds() - ByteStr(account.seed))
          save()
          before > walletData.getAccountSeeds().size
        }
    }

    override def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] =
      readAccountsFromWalletData.get(account.address).toRight[ValidationError](MissingSenderPrivateKey)

    override def nonce = Either.cond(!walletData.accountBased, walletData.nonce, AccountBasedWallet)

    private def incrementAndGetNonce(): Int = lock {
      val r = walletData.nonce + 1
      walletData = walletData.copy(nonce = r)
      r
    }
  }

}
