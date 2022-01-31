package com.ltonetwork

import com.ltonetwork.account.{Address, AddressScheme, PrivateKeyAccount}
import com.ltonetwork.block.Block
import com.ltonetwork.consensus.PoSCalculator.{generatorSignature, hit}
import com.ltonetwork.consensus.nxt.NxtLikeConsensusBlockData
import com.ltonetwork.consensus.{FairPoSCalculator, PoSCalculator}
import com.ltonetwork.crypto.signatureLength
import com.ltonetwork.features.{BlockchainFeature, BlockchainFeatures}
import com.ltonetwork.settings.{FunctionalitySettings, GenesisSettings, GenesisTransactionSettings}
import com.ltonetwork.state._
import com.ltonetwork.transaction.Proofs
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.wallet.Wallet
import com.ltonetwork.utils._
import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import java.io.{File, FileNotFoundException}
import java.nio.file.Files
import java.time.Instant
import scala.annotation.tailrec
import scala.concurrent.duration._

object GenesisBlockGenerator extends App {

  private type SeedText = String
  private type Share    = Long

  case class DistributionItem(seedText: String, nonce: Int = 0, amount: Share, miner: Boolean = true)

  case class Settings(
      networkType: String,
      baseTarget: Option[Long],
      averageBlockDelay: FiniteDuration,
      timestamp: Option[Long],
      distributions: List[DistributionItem],
      preActivatedFeatures: List[Int] = BlockchainFeatures.implemented.map(_.toInt).toList,
  ) {

    val initialBalance: Share = distributions.map(_.amount).sum

    val chainId: Char = networkType.head

    val features: Map[Short, Int] =
      preActivatedFeatures.map(_.toShort -> 0).toMap

    val functionalitySettings: FunctionalitySettings = FunctionalitySettings(
      featureCheckBlocksPeriod = Int.MaxValue,
      blocksForFeatureActivation = Int.MaxValue,
      preActivatedFeatures = features,
      doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
      feeVoteBlocksPeriod = Int.MaxValue,
      blocksForFeeChange = Int.MaxValue
    )

    def preActivated(feature: BlockchainFeature): Boolean = features.contains(feature.id)
  }

  case class FullAddressInfo(
      seedText: SeedText,
      seed: ByteStr,
      accountSeed: ByteStr,
      accountPrivateKey: ByteStr,
      accountPublicKey: ByteStr,
      accountAddress: Address,
      account: PrivateKeyAccount,
      miner: Boolean
  )

  private def toFullAddressInfo(item: DistributionItem): FullAddressInfo = {
    val seedHash = item.seedText.utf8Bytes
    val acc      = Wallet.generateNewAccount(seedHash, item.nonce)

    FullAddressInfo(
      seedText = item.seedText,
      seed = ByteStr(seedHash),
      accountSeed = ByteStr(acc.seed),
      accountPrivateKey = ByteStr(acc.privateKey),
      accountPublicKey = ByteStr(acc.publicKey),
      accountAddress = acc.toAddress,
      acc,
      item.miner
    )
  }

  val inputConfFile = new File(args.headOption.getOrElse(throw new IllegalArgumentException("Specify a path to genesis.conf")))
  if (!inputConfFile.exists()) throw new FileNotFoundException(inputConfFile.getCanonicalPath)

  val outputConfFile = args
    .drop(1)
    .headOption
    .map(new File(_).getAbsoluteFile.ensuring(f => !f.isDirectory && f.getParentFile.isDirectory || f.getParentFile.mkdirs()))

  val settings: Settings = {
    import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
    ConfigFactory.parseFile(inputConfFile).as[Settings]("genesis-generator")
  }

  com.ltonetwork.account.AddressScheme.current = new AddressScheme {
    override val chainId: Byte = settings.chainId.toByte
  }

  val shares: Seq[(FullAddressInfo, Share)] = settings.distributions
    .map(x => (toFullAddressInfo(x), x.amount))
    .sortBy(_._2)

  val timestamp = settings.timestamp.getOrElse(System.currentTimeMillis())

  val genesisTxs: Seq[GenesisTransaction] = shares.map {
    case (addrInfo, part) =>
      GenesisTransaction(1, settings.chainId.toByte, timestamp, addrInfo.accountAddress, part, Proofs.empty)
  }

  report(
    addrInfos = shares.map(_._1),
    settings = genesisSettings(settings.baseTarget),
    settings.chainId,
    settings.features
  )

  private def report(addrInfos: Iterable[FullAddressInfo], settings: GenesisSettings, chainId: Char, preActivatedFeatures: Map[Short, Int]): Unit = {
    val output = new StringBuilder(8192)
    output.append("Addresses:\n")
    addrInfos.foreach { acc =>
      output.append(s"""
           | Seed text:           ${acc.seedText}
           | Seed:                ${acc.seed}
           | Account seed:        ${acc.accountSeed}
           | Private account key: ${acc.accountPrivateKey}
           | Public account key:  ${acc.accountPublicKey}
           | Account address:     ${acc.accountAddress}
           | ===
           |""".stripMargin)
    }

    val walletSettings = addrInfos
      .collect {
        case fai if fai.miner =>
          s"""  wallet {
             |    seed = ${fai.seed}
             |    # password =
             |  }""".stripMargin
      }
      .mkString("\n", "\n", "")

    val confBody =
      s"""lto {
         |  blockchain {
         |    type = CUSTOM
         |    custom {
         |      address-scheme-character = $chainId
         |      functionality {
         |        pre-activated-features = null # undefines all previously defined pre-activated features
         |        pre-activated-features = ${preActivatedFeatures.toSeq.sorted.map { case (f, h) => s"$f = $h" }.mkString("{", ", ", "}")}
         |        feature-check-blocks-period = 1000
         |        blocks-for-feature-activation = 100
         |        double-features-periods-after-height = 100000000
         |        fee-vote-blocks-period = 10
         |        blocks-for-fee-change = 6
         |      }
         |      genesis {
         |        average-block-delay = ${settings.averageBlockDelay.toSeconds}s
         |        initial-base-target = ${settings.initialBaseTarget}
         |        timestamp = ${settings.timestamp} # ${Instant.ofEpochMilli(settings.timestamp)}
         |        block-timestamp = ${settings.blockTimestamp} # ${Instant.ofEpochMilli(settings.blockTimestamp)}
         |        signature = "${settings.signature.get}"
         |        initial-balance = ${settings.initialBalance}
         |        transactions = [
         |          ${settings.transactions.map(x => s"""{recipient = "${x.recipient}", amount = ${x.amount}}""").mkString(",\n    ")}
         |        ]
         |      }
         |    }
         |  }
         |  miner {
         |    micro-block-interval = ${Math.min((settings.averageBlockDelay.toMillis / 10).floor.toInt, 5000)}ms
         |    min-micro-block-age = 0s
         |  }$walletSettings
         |}
         |""".stripMargin

    output.append("Settings:\n")
    output.append(confBody)
    System.out.print(output.result())
    outputConfFile.foreach(ocf => Files.write(ocf.toPath, confBody.utf8Bytes))
  }

  def genesisSettings(predefined: Option[Long]): GenesisSettings =
    predefined
      .map(baseTarget => mkGenesisSettings(baseTarget))
      .getOrElse(mkGenesisSettings(calcInitialBaseTarget()))

  def mkGenesisSettings(baseTarget: Long): GenesisSettings = {
    val reference     = ByteStr(Array.fill(signatureLength)(-1: Byte))
    val genesisSigner = PrivateKeyAccount(ByteStr.empty)

    val genesis = Block
      .buildAndSign(
        version = 1,
        timestamp = timestamp,
        reference = reference,
        consensusData = NxtLikeConsensusBlockData(baseTarget, ByteStr(Array.fill(crypto.digestLength)(0: Byte))),
        transactionData = genesisTxs,
        signer = genesisSigner,
        featureVotes = Set.empty,
        feeVote = 0
      )
      .explicitGet()

    GenesisSettings(
      genesis.timestamp,
      timestamp,
      settings.initialBalance,
      Some(genesis.signature),
      genesisTxs.map { tx =>
        GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount)
      },
      genesis.consensusData.baseTarget,
      settings.averageBlockDelay
    )
  }

  def calcInitialBaseTarget(): Long = {
    val posCalculator: PoSCalculator = FairPoSCalculator

    val hitSource = ByteStr(new Array[Byte](crypto.digestLength))

    def getHit(account: PrivateKeyAccount): BigInt = {
      val gs = generatorSignature(hitSource, account.publicKey)
      hit(gs)
    }

    shares.collect {
      case (accountInfo, amount) if accountInfo.miner =>
        val hit = getHit(accountInfo.account)

        @tailrec def calculateBaseTarget(keyPair: PrivateKeyAccount, minBT: Long, maxBT: Long, balance: Long): Long =
          if (maxBT - minBT <= 1) maxBT
          else {
            val newBT = (maxBT + minBT) / 2
            val delay = posCalculator.calculateDelay(hit, newBT, balance)
            if (math.abs(delay - settings.averageBlockDelay.toMillis) < 100) newBT
            else {
              val (min, max) = if (delay > settings.averageBlockDelay.toMillis) (newBT, maxBT) else (minBT, newBT)
              calculateBaseTarget(keyPair, min, max, balance)
            }
          }

        calculateBaseTarget(accountInfo.account, PoSCalculator.MinBaseTarget, 1000000, amount)
    }.max
  }
}
