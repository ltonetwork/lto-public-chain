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
import com.ltonetwork.transaction.anchor.AnchorTransactionV1
import com.ltonetwork.transaction.association.{AssociationTransaction, AssociationTransactionBase, IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease._
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.sponsorship.{SponsorshipCancelTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.TimeImpl
import org.scalacheck.Gen.{alphaLowerChar, alphaUpperChar, frequency, numChar}
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

  val aliasSymbolChar: Gen[Char] = Gen.oneOf('.', '@', '_', '-')

  val invalidAliasSymbolChar: Gen[Char] = Gen.oneOf('~', '`', '!', '#', '$', '%', '^', '&', '*', '=', '+')

  val aliasAlphabetGen: Gen[Char]        = frequency((1, numChar), (1, aliasSymbolChar), (9, alphaLowerChar))
  val invalidAliasAlphabetGen: Gen[Char] = frequency((1, numChar), (3, invalidAliasSymbolChar), (9, alphaUpperChar))

  val validAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, aliasAlphabetGen)
  } yield aliasChars.mkString

  val accountOrAliasGen: Gen[AddressOrAlias] = accountGen.map(PublicKeyAccount.toAddress(_))

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

  val scriptGen = BOOLgen(100).map {
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
  } yield SetScriptTransaction.create(version, sender, script, fee, timestamp, proofs).explicitGet()

  def selfSignedSetScriptTransactionGenP(sender: PrivateKeyAccount, s: Script, ts: Long): Gen[SetScriptTransaction] =
    for {
      version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
      fee     <- smallFeeGen
    } yield SetScriptTransaction.selfSigned(version, sender, Some(s), fee, ts).explicitGet()

  val paymentGen: Gen[PaymentTransaction] = for {
    sender: PrivateKeyAccount    <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    tx                           <- paymentGeneratorP(sender, recipient)
  } yield tx

  val selfPaymentGen: Gen[PaymentTransaction] = accountGen.flatMap(acc => paymentGeneratorP(acc, acc))

  def paymentGeneratorP(sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] =
    timestampGen.flatMap(ts => paymentGeneratorP(ts, sender, recipient))

  def paymentGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] =
    for {
      amount: Long <- positiveLongGen
      fee: Long    <- smallFeeGen
    } yield PaymentTransaction.create(sender, recipient, amount, fee, timestamp).explicitGet()

  private val leaseParamGen = for {
    sender    <- accountGen
    amount    <- positiveLongGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
    recipient <- accountGen
  } yield (sender, amount, fee, timestamp, recipient)

  def createLease(sender: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias) = {
    val v1 = LeaseTransactionV1.selfSigned(sender, amount, fee, timestamp, recipient).explicitGet()
    val v2 = LeaseTransactionV2.selfSigned(LeaseTransactionV2.supportedVersions.head, sender, amount, fee, timestamp, recipient).explicitGet()
    Gen.oneOf(v1, v2)
  }

  def createLeaseCancel(sender: PrivateKeyAccount, leaseId: ByteStr, cancelFee: Long, timestamp: Long) = {
    val v1 = LeaseCancelTransactionV1.selfSigned(sender, leaseId, cancelFee, timestamp + 1).explicitGet()
    val v2 = LeaseCancelTransactionV2
      .selfSigned(LeaseTransactionV2.supportedVersions.head, AddressScheme.current.chainId, sender, leaseId, cancelFee, timestamp + 1)
      .right
      .get
    Gen.oneOf(v1, v2)
  }
  val leaseAndCancelGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    lease                                       <- createLease(sender, amount, fee, timestamp, recipient)
    cancelFee                                   <- smallFeeGen
    leaseCancel                                 <- createLeaseCancel(sender, lease.id(), cancelFee, timestamp + 1)
  } yield (lease, leaseCancel)

  def leaseAndCancelGeneratorP(leaseSender: PrivateKeyAccount,
                               recipient: AddressOrAlias,
                               unleaseSender: PrivateKeyAccount,
                               timestamp: Long): Gen[(LeaseTransaction, LeaseCancelTransaction)] =
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

  val leaseAndCancelWithOtherSenderGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    otherSender: PrivateKeyAccount              <- accountGen
    lease                                       <- createLease(sender, amount, fee, timestamp, recipient)
    fee2                                        <- smallFeeGen
    timestamp2                                  <- positiveLongGen
    leaseCancel                                 <- createLeaseCancel(otherSender, lease.id(), fee2, timestamp2)
  } yield (lease, leaseCancel)

  val leaseGen: Gen[LeaseTransaction]             = leaseAndCancelGen.map(_._1)
  val leaseCancelGen: Gen[LeaseCancelTransaction] = leaseAndCancelGen.map(_._2)

  val transferParamGen = for {
    amount    <- positiveLongGen
    feeAmount <- smallFeeGen
    assetId    = None
    feeAssetId = None
    timestamp  <- timestampGen
    sender     <- accountGen
    attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient  <- accountOrAliasGen
  } yield (sender, recipient, amount, timestamp, feeAmount, attachment)

  def transferGeneratorP(sender: PrivateKeyAccount,
                         recipient: AddressOrAlias,
                         assetId: Option[AssetId],
                         feeAssetId: Option[AssetId]): Gen[TransferTransaction] =
    for {
      (_, _, amount, timestamp, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(sender, recipient, amount, timestamp, feeAmount, attachment).explicitGet()

  def versionedTransferGeneratorP(sender: PrivateKeyAccount,
                                  recipient: AddressOrAlias,
                                  assetId: Option[AssetId],
                                  feeAssetId: Option[AssetId]): Gen[TransferTransactionV2] =
    for {
      (_, _, amount, timestamp, feeAmount, attachment) <- transferParamGen
    } yield
      TransferTransactionV2
        .selfSigned(TransferTransactionV2.supportedVersions.head, sender, recipient, amount, timestamp, feeAmount, attachment)
        .explicitGet()

  def transferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: AddressOrAlias, maxAmount: Long): Gen[TransferTransaction] =
    for {
      amount                              <- Gen.choose(1, maxAmount)
      (_, _, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(sender, recipient, amount, timestamp, feeAmount, attachment).explicitGet()

  def transferGeneratorPV2(timestamp: Long, sender: PrivateKeyAccount, recipient: AddressOrAlias, maxAmount: Long): Gen[TransferTransactionV2] =
    for {
      amount                              <- Gen.choose(1, maxAmount)
      (_, _, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransactionV2.selfSigned(2, sender, recipient, amount, timestamp, feeAmount, attachment).explicitGet()

  def transferGeneratorP(timestamp: Long,
                         sender: PrivateKeyAccount,
                         recipient: AddressOrAlias,
                         assetId: Option[AssetId],
                         feeAssetId: Option[AssetId]): Gen[TransferTransaction] =
    for {
      (_, _, amount, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(sender, recipient, amount, timestamp, feeAmount, attachment).explicitGet()

  def ltoTransferGeneratorP(sender: PrivateKeyAccount, recipient: AddressOrAlias): Gen[TransferTransaction] =
    transferGeneratorP(sender, recipient, None, None)

  def ltoTransferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: AddressOrAlias): Gen[TransferTransaction] =
    transferGeneratorP(timestamp, sender, recipient, None, None)

  def massTransferGeneratorP(sender: PrivateKeyAccount, transfers: List[ParsedTransfer], assetId: Option[AssetId]): Gen[MassTransferTransaction] =
    for {
      version                             <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
      (_, _, _, timestamp, _, attachment) <- transferParamGen
    } yield MassTransferTransaction.selfSigned(version, sender, transfers, timestamp, 100000000 + 10000000 * transfers.size, attachment).explicitGet()

  def createLtoTransfer(sender: PrivateKeyAccount,
                          recipient: Address,
                          amount: Long,
                          fee: Long,
                          timestamp: Long): Either[ValidationError, TransferTransaction] =
    TransferTransaction.selfSigned(sender, recipient, amount, timestamp, fee, Array())

  val transferV1Gen = (for {
    (sender, recipient, amount, timestamp, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.selfSigned(sender, recipient, amount, timestamp, feeAmount, attachment).explicitGet())
    .label("transferTransaction")

  val transferV2Gen = (for {
    version                                                       <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
    (sender, recipient, amount, timestamp, feeAmount, attachment) <- transferParamGen
    proofs                                                        <- proofsGen
  } yield
    TransferTransactionV2
      .create(version, sender, recipient, amount, timestamp, feeAmount, attachment, proofs)
      .explicitGet())
    .label("VersionedTransferTransaction")

  def versionedTransferGenP(sender: PublicKeyAccount, recipient: Address, proofs: Proofs) =
    (for {
      version   <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TransferTransactionV2.create(version, sender, recipient, amt, timestamp, fee, Array.emptyByteArray, proofs).explicitGet())
      .label("VersionedTransferTransactionP")

  val transferWithLtoFeeGen = for {
    (sender, recipient, amount, timestamp, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.selfSigned(sender, recipient, amount, timestamp, feeAmount, attachment).explicitGet()

  val selfTransferWithLtoFeeGen: Gen[TransferTransaction] = for {
    (sender, _, amount, timestamp, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.selfSigned(sender, sender, amount, timestamp, feeAmount, attachment).explicitGet()

  val selfTransferGen: Gen[TransferTransaction] = for {
    (sender, _, amount, timestamp, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.selfSigned(sender, sender, amount, timestamp, feeAmount, attachment).explicitGet()

  val massTransferGen: Gen[MassTransferTransaction] = massTransferGen(MaxTransferCount)

  def massTransferGen(maxTransfersCount: Int) =
    for {
      version                                          <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
      (sender, _, _, timestamp, feeAmount, attachment) <- transferParamGen
      transferCount                                    <- Gen.choose(0, maxTransfersCount)
      transferGen = for {
        recipient <- accountOrAliasGen
        amount    <- Gen.choose(1L, Long.MaxValue / maxTransfersCount)
      } yield ParsedTransfer(recipient, amount)
      recipients <- Gen.listOfN(transferCount, transferGen)
    } yield MassTransferTransaction.selfSigned(version, sender, recipients, timestamp, feeAmount, attachment).explicitGet()

  val MinIssueFee = 100000000

  def versionGen(builder: TransactionBuilder): Gen[Byte] = {
    Gen.oneOf(builder.supportedVersions.toSeq)
  }
  val randomTransactionGen: Gen[SignedTransaction] = (for {
    tr <- transferV1Gen
  } yield tr).label("random transaction")

  def randomTransactionsGen(count: Int): Gen[Seq[SignedTransaction]] =
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

  val dataKeyGen = for {
    size <- Gen.choose[Byte](1, MaxKeySize)
  } yield Random.nextString(size)

  val dataScriptsKeyGen = for {
    size <- Gen.choose[Byte](1, 10)
  } yield Random.nextString(size)

  val dataAsciiKeyGen = for {
    size <- Gen.choose[Byte](1, MaxKeySize)
  } yield Random.alphanumeric.take(size).mkString

  def longEntryGen(keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      value <- Gen.choose[Long](Long.MinValue, Long.MaxValue)
    } yield IntegerDataEntry(key, value)

  def booleanEntryGen(keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      value <- Gen.oneOf(true, false)
    } yield BooleanDataEntry(key, value)

  def binaryEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- byteArrayGen(size)
    } yield BinaryDataEntry(key, ByteStr(value))

  def stringEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen) =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- Gen.listOfN(size, aliasAlphabetGen)
    } yield StringDataEntry(key, value.mkString)

  def dataEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen) =
    Gen.oneOf(longEntryGen(keyGen), booleanEntryGen(keyGen), binaryEntryGen(maxSize, keyGen), stringEntryGen(maxSize, keyGen))

  val dataTransactionGen: Gen[DataTransaction] = dataTransactionGen(DataTransaction.MaxEntryCount)

  def dataTransactionGen(maxEntryCount: Int, useForScript: Boolean = false) =
    (for {
      sender    <- accountGen
      timestamp <- timestampGen
      size      <- Gen.choose(0, maxEntryCount)
      maxEntrySize = if (useForScript) 200 else (DataTransaction.MaxBytes - 122) / (size max 1) min DataEntry.MaxValueSize
      data <- if (useForScript) Gen.listOfN(size, dataEntryGen(maxEntrySize, dataScriptsKeyGen)) else Gen.listOfN(size, dataEntryGen(maxEntrySize))
      uniq = data.foldRight(List.empty[DataEntry[_]]) { (e, es) =>
        if (es.exists(_.key == e.key)) es else e :: es
      }
      version <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
    } yield DataTransaction.selfSigned(version, sender, uniq, 15000000, timestamp).explicitGet())
      .label("DataTransaction")

  def dataTransactionGenP(sender: PrivateKeyAccount, data: List[DataEntry[_]]): Gen[DataTransaction] =
    (for {
      version   <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      timestamp <- timestampGen
    } yield DataTransaction.selfSigned(version, sender, data, 150000000, timestamp).explicitGet())
      .label("DataTransactionP")

  def preconditionsTransferAndLease(typed: EXPR): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typed).explicitGet(), ts + 1)
      transfer  <- transferGeneratorPV2(ts, master, recipient.toAddress, ENOUGH_AMT / 2)
      fee       <- smallFeeGen
      lease = LeaseTransactionV2.selfSigned(LeaseTransactionV2.supportedVersions.head, master, ENOUGH_AMT / 2, fee, ts, recipient).explicitGet()
    } yield (genesis, setScript, lease, transfer)

  val anchorTransactionGen: Gen[AnchorTransactionV1] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    size      <- Gen.choose(0, AnchorTransactionV1.MaxEntryCount)
    len       <- Gen.oneOf(AnchorTransactionV1.EntryLength)
    data      <- Gen.listOfN(size, genBoundedBytes(len, len))
    version   <- Gen.oneOf(AnchorTransactionV1.supportedVersions.toSeq)
  } yield {
    val anchors = data.map(ByteStr(_))
    AnchorTransactionV1.selfSigned(version, sender, anchors, 15000000, timestamp).explicitGet()
  }

  val assocTransactionGen: Gen[AssociationTransactionBase] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    version   <- Gen.oneOf(AssociationTransaction.supportedVersions.toSeq)
    party     <- accountGen
    assocType <- Gen.choose(Int.MinValue, Int.MaxValue)
    action    <- Gen.oneOf(AssociationTransaction.ActionType.Issue, AssociationTransaction.ActionType.Revoke)
    fee       <- smallFeeGen
    hashOpt   <- Gen.option(genBoundedBytes(0, AssociationTransaction.MaxHashLength).map(ByteStr(_)))
  } yield {
    action match {
      case AssociationTransaction.ActionType.Issue =>
        IssueAssociationTransaction.selfSigned(version, sender, party, assocType, hashOpt, fee, timestamp).explicitGet()
      case AssociationTransaction.ActionType.Revoke =>
        RevokeAssociationTransaction.selfSigned(version, sender, party, assocType, hashOpt, fee, timestamp).explicitGet()
    }
  }

  val sponsorshipGen: Gen[SponsorshipTransaction] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    version   <- Gen.oneOf(SponsorshipTransaction.supportedVersions.toSeq)
    party     <- accountGen
    fee       <- smallFeeGen
  } yield SponsorshipTransaction.selfSigned(version, sender, party, fee, timestamp).explicitGet()

  val sponsorshipCancelGen: Gen[SponsorshipCancelTransaction] = for {
    sender    <- accountGen
    timestamp <- timestampGen
    version   <- Gen.oneOf(SponsorshipCancelTransaction.supportedVersions.toSeq)
    party     <- accountGen
    fee       <- smallFeeGen
  } yield SponsorshipCancelTransaction.selfSigned(version, sender, party, fee, timestamp).explicitGet()

}
