package com.ltonetwork

import cats.syntax.semigroup._
import com.ltonetwork.account.PublicKeyAccount._
import com.ltonetwork.account._
import com.ltonetwork.lang.Global
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.compiler.Terms._
import com.ltonetwork.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.ltonetwork.lang.v1.testing.ScriptGen
import com.ltonetwork.state._
import com.ltonetwork.state.diffs.ENOUGH_AMT
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease._
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.TimeImpl
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.util.Random

trait TransactionGen extends BeforeAndAfterAll with TransactionGenBase with ScriptGen {
  _: Suite =>
  override protected def afterAll(): Unit = {
    super.close()
  }
}

trait TransactionGenBase extends ScriptGen {

  protected def lto(n: Float): Long = (n * 100000000L).toLong

  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])

  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val bytes64gen: Gen[Array[Byte]] = byteArrayGen(64)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    for {
      length <- Gen.chooseNum(minSize, maxSize)
      bytes  <- byteArrayGen(length)
    } yield bytes

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz =>
      Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray)
    }
  }

  private val time = new TimeImpl

  def close(): Unit = {
    time.close()
  }

  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(time.correctedTime() - _)

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val addressGen: Gen[Address] = accountGen.map(PublicKeyAccount.toAddress(_))
  def otherAccountGen(candidate: PrivateKeyAccount): Gen[PrivateKeyAccount] = accountGen.flatMap(Gen.oneOf(candidate, _))

  val positiveLongGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L / 100)
  val positiveIntGen: Gen[Int]   = Gen.choose(1, Int.MaxValue / 100)
  val smallFeeGen: Gen[Long]     = Gen.choose(100000000, 1000000000)
  val enoughFeeGen: Gen[Long]    = Gen.choose(100000000, Int.MaxValue / 10)

  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - 100)

  val ltoAssetGen: Gen[Option[ByteStr]] = Gen.const(None)

  val proofsGen: Gen[Proofs] = for {
    proofsAmount <- Gen.choose(1, 8)
    proofs       <- Gen.listOfN(proofsAmount, genBoundedBytes(0, 50))
  } yield Proofs.create(proofs.map(ByteStr(_))).explicitGet()

  val scriptGen: Gen[Script] = BOOLgen(100).map {
    case (expr, _) =>
      val typed =
        CompilerV1(PureContext.compilerContext |+| CryptoContext.compilerContext(Global), expr).explicitGet()
      ScriptV1(typed._1).explicitGet()
  }

  val setScriptTransactionGen: Gen[SetScriptTransaction] = for {
    version                   <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    sender: PrivateKeyAccount <- accountGen
    fee                       <- smallFeeGen
    timestamp                 <- timestampGen
    proofs                    <- proofsGen
    script                    <- Gen.option(scriptGen)
  } yield SetScriptTransaction.create(version, None, timestamp, sender, fee, script, None, proofs).explicitGet()

  def selfSignedSetScriptTransactionGenP(sender: PrivateKeyAccount, script: Script, timestamp: Long): Gen[SetScriptTransaction] =
    for {
      version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
      fee     <- smallFeeGen
    } yield SetScriptTransaction.selfSigned(version, timestamp, sender, fee, Some(script)).explicitGet()

  private val leaseParamGen = for {
    sender    <- accountGen
    amount    <- positiveLongGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
    recipient <- accountGen
  } yield (sender, amount, fee, timestamp, recipient)

  def createLease(sender: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: Address): Gen[LeaseTransaction] = for {
    version <- Gen.oneOf(LeaseTransaction.supportedVersions.toSeq)
  } yield LeaseTransaction.selfSigned(version, timestamp, sender, fee, recipient, amount).explicitGet()

  def createLeaseCancel(sender: PrivateKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long): Gen[CancelLeaseTransaction] = for {
    version <- Gen.oneOf(LeaseTransaction.supportedVersions.toSeq)
  } yield CancelLeaseTransaction.selfSigned(version, timestamp, sender, fee, leaseId).explicitGet()

  val leaseAndCancelGen: Gen[(LeaseTransaction, CancelLeaseTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    lease                                       <- createLease(sender, amount, fee, timestamp, recipient)
    cancelFee                                   <- smallFeeGen
    leaseCancel                                 <- createLeaseCancel(sender, lease.id(), cancelFee, timestamp + 1)
  } yield (lease, leaseCancel)

  def leaseAndCancelGeneratorP(leaseSender: PrivateKeyAccount,
                               recipient: Address,
                               unleaseSender: PrivateKeyAccount,
                               timestamp: Long): Gen[(LeaseTransaction, CancelLeaseTransaction)] =
    for {
      (_, amount, fee, _, _) <- leaseParamGen
      lease                  <- createLease(leaseSender, amount, fee, timestamp, recipient)
      fee2                   <- smallFeeGen
      unlease                <- createLeaseCancel(unleaseSender, lease.id(), fee2, timestamp + 1)
    } yield (lease, unlease)

  val twoLeasesGen: Gen[(LeaseTransaction, LeaseTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    amount2                                     <- positiveLongGen
    recipient2: PrivateKeyAccount               <- accountGen
    fee2                                        <- smallFeeGen
    lease1                                      <- createLease(sender, amount, fee, timestamp, recipient)
    lease2                                      <- createLease(sender, amount2, fee2, timestamp + 1, recipient2)
  } yield (lease1, lease2)

  val leaseAndCancelWithOtherSenderGen: Gen[(LeaseTransaction, CancelLeaseTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    otherSender: PrivateKeyAccount              <- accountGen
    lease                                       <- createLease(sender, amount, fee, timestamp, recipient)
    fee2                                        <- smallFeeGen
    timestamp2                                  <- positiveLongGen
    leaseCancel                                 <- createLeaseCancel(otherSender, lease.id(), fee2, timestamp2)
  } yield (lease, leaseCancel)

  val leaseGen: Gen[LeaseTransaction]             = leaseAndCancelGen.map(_._1)
  val leaseCancelGen: Gen[CancelLeaseTransaction] = leaseAndCancelGen.map(_._2)

  val transferParamGen: Gen[(PrivateKeyAccount, Address, Long, Long, Long, Array[Byte])] = for {
    amount    <- positiveLongGen
    fee <- smallFeeGen
    timestamp  <- timestampGen
    sender     <- accountGen
    attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient  <- addressGen
  } yield (sender, recipient, amount, timestamp, fee, attachment)

  def transferGeneratorP(sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    for {
      (_, _, amount, timestamp, fee, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(1, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def versionedTransferGeneratorP(sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    for {
      (_, _, amount, timestamp, fee, attachment) <- transferParamGen
    } yield
      TransferTransaction
        .selfSigned(2, timestamp, sender, fee, recipient, amount, attachment)
        .explicitGet()

  def transferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: Address, maxAmount: Long): Gen[TransferTransaction] =
    for {
      amount                              <- Gen.choose(1, maxAmount)
      (_, _, _, _, fee, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(1, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def transferGeneratorPV2(timestamp: Long, sender: PrivateKeyAccount, recipient: Address, maxAmount: Long): Gen[TransferTransaction] =
    for {
      amount                              <- Gen.choose(1, maxAmount)
      (_, _, _, _, fee, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(2, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def transferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    for {
      (_, _, amount, _, fee, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(1, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def ltoTransferGeneratorP(sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    transferGeneratorP(sender, recipient)

  def ltoTransferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    transferGeneratorP(timestamp, sender, recipient)

  def massTransferGeneratorP(sender: PrivateKeyAccount, transfers: List[ParsedTransfer]): Gen[MassTransferTransaction] =
    for {
      version                             <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
      (_, _, _, timestamp, _, attachment) <- transferParamGen
    } yield MassTransferTransaction.selfSigned(version, timestamp, sender, 100000000 + 10000000 * transfers.size, transfers, attachment).explicitGet()

  def createLtoTransfer(sender: PrivateKeyAccount,
                        recipient: Address,
                        amount: Long,
                        fee: Long,
                        timestamp: Long): Either[ValidationError, TransferTransaction] =
    TransferTransaction.selfSigned(1, timestamp, sender, fee, recipient, amount, Array())

  val transferGen: Gen[TransferTransaction] = (for {
    version                                                 <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
    (sender, recipient, amount, timestamp, fee, attachment) <- transferParamGen
  } yield
    TransferTransaction.selfSigned(version, timestamp, sender, fee, recipient, amount, attachment).explicitGet())
    .label("TransferTransaction")

  val transferV1Gen: Gen[TransferTransaction] = (for {
    (sender, recipient, amount, timestamp, fee, attachment) <- transferParamGen
  } yield TransferTransaction.selfSigned(1, timestamp, sender, fee, recipient, amount, attachment).explicitGet())
    .label("TransferTransactionV1")

  val transferV2Gen: Gen[TransferTransaction] = (for {
    (sender, recipient, amount, timestamp, fee, attachment) <- transferParamGen
    proofs                                                  <- proofsGen
  } yield
    TransferTransaction
      .create(2, None, timestamp, sender, fee, recipient, amount, attachment, None, proofs)
      .explicitGet())
    .label("TransferTransactionV2")

  def versionedTransferGenP(sender: PublicKeyAccount, recipient: Address, proofs: Proofs): Gen[TransferTransaction] =
    (for {
      version   <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
      amount    <- positiveLongGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TransferTransaction.create(version, None, timestamp, sender, fee, recipient, amount, Array.emptyByteArray, None, proofs).explicitGet())
      .label("VersionedTransferTransactionP")

  val selfTransferGen: Gen[TransferTransaction] = for {
    (sender, _, amount, timestamp, fee, attachment) <- transferParamGen
  } yield TransferTransaction.selfSigned(1, timestamp, sender, fee, sender, amount, attachment).explicitGet()

  val transferWithLtoFeeGen: Gen[TransferTransaction] = transferV1Gen
  val selfTransferWithLtoFeeGen: Gen[TransferTransaction] = selfTransferGen

  val massTransferGen: Gen[MassTransferTransaction] = massTransferGen(MaxTransferCount)

  def massTransferGen(maxTransfersCount: Int): Gen[MassTransferTransaction] =
    for {
      version                                    <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
      (sender, _, _, timestamp, fee, attachment) <- transferParamGen
      transferCount                              <- Gen.choose(0, maxTransfersCount)
      transferGen = for {
        recipient <- addressGen
        amount    <- Gen.choose(1L, Long.MaxValue / maxTransfersCount)
      } yield ParsedTransfer(recipient, amount)
      recipients <- Gen.listOfN(transferCount, transferGen)
    } yield MassTransferTransaction.selfSigned(version, timestamp, sender, fee, recipients, attachment).explicitGet()

  val MinIssueFee = 100000000

  def versionGen(builder: TransactionBuilder): Gen[Byte] = {
    Gen.oneOf(builder.supportedVersions.toSeq)
  }
  val randomTransactionGen: Gen[Transaction] = (for {
    tr <- transferV1Gen
  } yield tr).label("random transaction")

  def randomTransactionsGen(count: Int): Gen[Seq[Transaction]] =
    for {
      transactions <- Gen.listOfN(count, randomTransactionGen)
    } yield transactions

  val genesisGen: Gen[GenesisTransaction] = accountGen.flatMap(genesisGeneratorP)

  def genesisGeneratorP(recipient: PrivateKeyAccount): Gen[GenesisTransaction] =
    for {
      amt <- Gen.choose(1, 100000000L * 100000000L)
      ts  <- positiveIntGen
    } yield GenesisTransaction.create(recipient, amt, ts).explicitGet()

  import DataEntry.MaxKeySize

  val dataKeyGen: Gen[String] = for {
    size <- Gen.choose[Byte](1, MaxKeySize)
  } yield Random.nextString(size)

  val dataScriptsKeyGen: Gen[String] = for {
    size <- Gen.choose[Byte](1, 10)
  } yield Random.nextString(size)

  val dataAsciiKeyGen: Gen[String] = for {
    size <- Gen.choose[Byte](1, MaxKeySize)
  } yield Random.alphanumeric.take(size).mkString

  def longEntryGen(keyGen: Gen[String] = dataKeyGen): Gen[IntegerDataEntry] =
    for {
      key   <- keyGen
      value <- Gen.choose[Long](Long.MinValue, Long.MaxValue)
    } yield IntegerDataEntry(key, value)

  def booleanEntryGen(keyGen: Gen[String] = dataKeyGen): Gen[BooleanDataEntry] =
    for {
      key   <- keyGen
      value <- Gen.oneOf(true, false)
    } yield BooleanDataEntry(key, value)

  def binaryEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[BinaryDataEntry] =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- byteArrayGen(size)
    } yield BinaryDataEntry(key, ByteStr(value))

  def stringEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[StringDataEntry] =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- Gen.listOfN(size, Gen.alphaNumChar)
    } yield StringDataEntry(key, value.mkString)

  def dataEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen) =
    Gen.oneOf(longEntryGen(keyGen), booleanEntryGen(keyGen), binaryEntryGen(maxSize, keyGen), stringEntryGen(maxSize, keyGen))

  val dataTransactionGen: Gen[DataTransaction] = dataTransactionGen(DataTransaction.MaxEntryCount)

  def dataTransactionGen(maxEntryCount: Int, useForScript: Boolean = false): Gen[DataTransaction] =
    (for {
      sender    <- accountGen
      timestamp <- timestampGen
      size      <- Gen.choose(0, maxEntryCount)
      fee = 15000000
      maxEntrySize = if (useForScript) 200 else (DataTransaction.MaxBytes - 122) / (size max 1) min DataEntry.MaxValueSize
      data <- if (useForScript) Gen.listOfN(size, dataEntryGen(maxEntrySize, dataScriptsKeyGen)) else Gen.listOfN(size, dataEntryGen(maxEntrySize))
      uniq = data.foldRight(List.empty[DataEntry[_]]) { (e, es) =>
        if (es.exists(_.key == e.key)) es else e :: es
      }
      version <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
    } yield DataTransaction.selfSigned(version, timestamp, sender, fee, uniq).explicitGet())
      .label("DataTransaction")

  def dataTransactionGenP(sender: PrivateKeyAccount, data: List[DataEntry[_]]): Gen[DataTransaction] =
    (for {
      version   <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      timestamp <- timestampGen
      fee = 150000000
    } yield DataTransaction.selfSigned(version, timestamp, sender, fee, data).explicitGet())
      .label("DataTransactionP")

  def preconditionsTransferAndLease(typed: EXPR): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] =
    for {
      master    <- accountGen
      recipient <- accountGen
      timestamp <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, timestamp).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typed).explicitGet(), timestamp + 1)
      transfer  <- transferGeneratorPV2(timestamp, master, recipient.toAddress, ENOUGH_AMT / 2)
      fee       <- smallFeeGen
      lease = LeaseTransaction.selfSigned(2, timestamp, master, fee, recipient, ENOUGH_AMT / 2).explicitGet()
    } yield (genesis, setScript, lease, transfer)

  val anchorTransactionGen: Gen[AnchorTransaction] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    size      <- Gen.choose(0, AnchorTransaction.MaxEntryCount)
    len       <- Gen.oneOf(AnchorTransaction.EntryLength)
    data      <- Gen.listOfN(size, genBoundedBytes(len, len))
    version   <- Gen.oneOf(AnchorTransaction.supportedVersions.toSeq)
    fee = 15000000
    anchors = data.map(ByteStr(_))
  } yield AnchorTransaction.selfSigned(version, timestamp, sender, fee, anchors).explicitGet()

  val assocTransactionGen: Gen[AssociationTransaction] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    version   <- Gen.oneOf(IssueAssociationTransaction.supportedVersions.toSeq)
    recipient <- accountGen
    assocType <- Gen.choose(Int.MinValue, Int.MaxValue)
    builder   <- Gen.oneOf(IssueAssociationTransaction, RevokeAssociationTransaction)
    fee       <- smallFeeGen
    hashOpt   <- Gen.option(genBoundedBytes(0, IssueAssociationTransaction.MaxHashLength).map(ByteStr(_)))
  } yield {
    builder match {
      case IssueAssociationTransaction =>
        IssueAssociationTransaction.selfSigned(version, timestamp, sender, fee, recipient, assocType, None, hashOpt).explicitGet()
      case RevokeAssociationTransaction =>
        RevokeAssociationTransaction.selfSigned(version, timestamp, sender, fee, recipient, assocType, hashOpt).explicitGet()
    }
  }

  val sponsorshipGen: Gen[SponsorshipTransaction] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    version   <- Gen.oneOf(SponsorshipTransaction.supportedVersions.toSeq)
    recipient <- accountGen
    fee       <- smallFeeGen
  } yield SponsorshipTransaction.selfSigned(version, timestamp, sender, fee, recipient).explicitGet()

  val cancelSponsorshipGen: Gen[CancelSponsorshipTransaction] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    version   <- Gen.oneOf(CancelSponsorshipTransaction.supportedVersions.toSeq)
    recipient <- accountGen
    fee       <- smallFeeGen
  } yield CancelSponsorshipTransaction.selfSigned(version, timestamp, sender, fee, recipient).explicitGet()

}
