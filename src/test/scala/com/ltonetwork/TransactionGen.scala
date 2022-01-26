package com.ltonetwork

import cats.syntax.semigroup._
import com.ltonetwork.account.KeyTypes.{ED25519, SECP256K1}
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
import com.ltonetwork.transaction.register.RegisterTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.TimeImpl
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.TableFor1

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

  def accountGen(keyType: KeyType): Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed, keyType))
  def accountGen: Gen[PrivateKeyAccount]                   = accountGen(ED25519)

  def accountGenRandom(): Gen[PrivateKeyAccount] =
    accountGen(Gen.oneOf(KeyTypes.all).pureApply(Gen.Parameters.default, Seed.random(), 100))

  val addressGen: Gen[Address] = accountGen.map(PublicKeyAccount.toAddress(_))

  def otherAccountGen(candidate: PrivateKeyAccount): Gen[PrivateKeyAccount] = accountGen.flatMap(Gen.oneOf(candidate, _))

  def sponsorGen(version: Byte): Gen[Option[PrivateKeyAccount]] = if (version < 3) Gen.const(None) else Gen.option(accountGen)
  def sponsorGen: Gen[Option[PrivateKeyAccount]]                = Gen.option(accountGen)

  val positiveLongGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L / 100)
  val positiveIntGen: Gen[Int]   = Gen.choose(1, Int.MaxValue / 100)
  val smallFeeGen: Gen[Long]     = Gen.choose(100000000, 1000000000)
  val enoughFeeGen: Gen[Long]    = Gen.choose(100000000, Int.MaxValue / 10)

  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - 100)

  def versionGen(builder: TransactionBuilder): Gen[Byte]         = Gen.oneOf(builder.supportedVersions.toSeq)
  def versionTable(builder: TransactionBuilder): TableFor1[Byte] = Table("version", builder.supportedVersions.toList: _*)
  val keyTypeTable: TableFor1[KeyType]                           = Table("keytypes", KeyTypes.all: _*)

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

  def setScriptTransactionGen: Gen[SetScriptTransaction]                = versionGen(SetScriptTransaction).flatMap(setScriptTransactionGen)
  def setScriptTransactionGen(version: Byte): Gen[SetScriptTransaction] = setScriptTransactionGen(version, ED25519)
  def setScriptTransactionGen(version: Byte, keyType: KeyType): Gen[SetScriptTransaction] =
    for {
      sender: PrivateKeyAccount <- accountGen(keyType)
      fee                       <- smallFeeGen
      timestamp                 <- timestampGen
      proofs                    <- proofsGen
      script                    <- Gen.option(scriptGen)
      sponsor                   <- sponsorGen(version)
    } yield SetScriptTransaction.create(version, None, timestamp, sender, fee, script, sponsor, proofs).explicitGet()

  def selfSignedSetScriptTransactionGenP(sender: PrivateKeyAccount, script: Script, timestamp: Long): Gen[SetScriptTransaction] =
    for {
      fee <- smallFeeGen
    } yield SetScriptTransaction.signed(1, timestamp, sender, fee, Some(script)).explicitGet()

  private def leaseParamGen(): Gen[(PrivateKeyAccount, Long, Long, Long, PrivateKeyAccount)] = leaseParamGen(ED25519)
  private def leaseParamGen(keyType: KeyType): Gen[(PrivateKeyAccount, Long, Long, Long, PrivateKeyAccount)] =
    for {
      sender    <- accountGen(keyType)
      amount    <- positiveLongGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
      recipient <- accountGen(keyType)
    } yield (sender, amount, fee, timestamp, recipient)

  def createLease(sender: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: Address): Gen[LeaseTransaction] =
    versionGen(LeaseTransaction).flatMap(createLease(_, sender, amount, fee, timestamp, recipient))
  def createLease(version: Byte, sender: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: Address): Gen[LeaseTransaction] =
    LeaseTransaction.signed(version, timestamp, sender, fee, recipient, amount).explicitGet()

  def createLeaseCancel(sender: PrivateKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long): Gen[CancelLeaseTransaction] =
    versionGen(LeaseTransaction).flatMap(createLeaseCancel(_, sender, leaseId, fee, timestamp))
  def createLeaseCancel(version: Byte, sender: PrivateKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long): Gen[CancelLeaseTransaction] =
    CancelLeaseTransaction.signed(version, timestamp, sender, fee, leaseId).explicitGet()

  def leaseAndCancelGen: Gen[(LeaseTransaction, CancelLeaseTransaction)]                = versionGen(LeaseTransaction).flatMap(leaseAndCancelGen)
  def leaseAndCancelGen(version: Byte): Gen[(LeaseTransaction, CancelLeaseTransaction)] = leaseAndCancelGen(version, ED25519)
  def leaseAndCancelGen(version: Byte, keyType: KeyType): Gen[(LeaseTransaction, CancelLeaseTransaction)] =
    for {
      (sender, amount, fee, timestamp, recipient) <- leaseParamGen(keyType)
      lease                                       <- createLease(version, sender, amount, fee, timestamp, recipient)
      cancelFee                                   <- smallFeeGen
      leaseCancel                                 <- createLeaseCancel(version, sender, lease.id(), cancelFee, timestamp + 1)
    } yield (lease, leaseCancel)

  def leaseAndCancelGeneratorP(leaseSender: PrivateKeyAccount,
                               recipient: Address,
                               unleaseSender: PrivateKeyAccount,
                               timestamp: Long): Gen[(LeaseTransaction, CancelLeaseTransaction)] =
    for {
      (_, amount, fee, _, _) <- leaseParamGen
      lease                  <- createLease(1, leaseSender, amount, fee, timestamp, recipient)
      fee2                   <- smallFeeGen
      unlease                <- createLeaseCancel(1, unleaseSender, lease.id(), fee2, timestamp + 1)
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

  def leaseGen: Gen[LeaseTransaction]                                  = versionGen(LeaseTransaction).flatMap(leaseGen)
  def leaseGen(version: Byte): Gen[LeaseTransaction]                   = leaseGen(version, ED25519)
  def leaseGen(version: Byte, keyType: KeyType): Gen[LeaseTransaction] = leaseAndCancelGen(version, keyType).map(_._1)

  def cancelLeaseGen: Gen[CancelLeaseTransaction]                                  = versionGen(CancelLeaseTransaction).flatMap(cancelLeaseGen)
  def cancelLeaseGen(version: Byte): Gen[CancelLeaseTransaction]                   = cancelLeaseGen(version, ED25519)
  def cancelLeaseGen(version: Byte, keyType: KeyType): Gen[CancelLeaseTransaction] = leaseAndCancelGen(version, keyType).map(_._2)

  def transferParamGen(keyType: KeyType): Gen[(PrivateKeyAccount, Address, Long, Long, Long, Array[Byte])] =
    for {
      amount     <- positiveLongGen
      fee        <- smallFeeGen
      timestamp  <- timestampGen
      sender     <- accountGen(keyType)
      attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
      recipient  <- addressGen
    } yield (sender, recipient, amount, timestamp, fee, attachment)
  def transferParamGen(): Gen[(PrivateKeyAccount, Address, Long, Long, Long, Array[Byte])] = transferParamGen(ED25519)

  def transferGeneratorP(sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    for {
      (_, _, amount, timestamp, fee, attachment) <- transferParamGen
    } yield TransferTransaction.signed(1, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def versionedTransferGeneratorP(sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    for {
      (_, _, amount, timestamp, fee, attachment) <- transferParamGen
    } yield
      TransferTransaction
        .signed(2, timestamp, sender, fee, recipient, amount, attachment)
        .explicitGet()

  def transferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: Address, maxAmount: Long): Gen[TransferTransaction] =
    for {
      amount                        <- Gen.choose(1, maxAmount)
      (_, _, _, _, fee, attachment) <- transferParamGen
    } yield TransferTransaction.signed(1, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def transferGeneratorPV2(timestamp: Long, sender: PrivateKeyAccount, recipient: Address, maxAmount: Long): Gen[TransferTransaction] =
    for {
      amount                        <- Gen.choose(1, maxAmount)
      (_, _, _, _, fee, attachment) <- transferParamGen
    } yield TransferTransaction.signed(2, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def transferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    for {
      (_, _, amount, _, fee, attachment) <- transferParamGen
    } yield TransferTransaction.signed(1, timestamp, sender, fee, recipient, amount, attachment).explicitGet()

  def ltoTransferGeneratorP(sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    transferGeneratorP(sender, recipient)

  def ltoTransferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: Address): Gen[TransferTransaction] =
    transferGeneratorP(timestamp, sender, recipient)

  def massTransferGeneratorP(sender: PrivateKeyAccount, transfers: List[ParsedTransfer]): Gen[MassTransferTransaction] =
    for {
      version                             <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
      (_, _, _, timestamp, _, attachment) <- transferParamGen
    } yield MassTransferTransaction.signed(version, timestamp, sender, 100000000 + 10000000 * transfers.size, transfers, attachment).explicitGet()

  def createLtoTransfer(sender: PrivateKeyAccount,
                        recipient: Address,
                        amount: Long,
                        fee: Long,
                        timestamp: Long): Either[ValidationError, TransferTransaction] =
    TransferTransaction.signed(1, timestamp, sender, fee, recipient, amount, Array())

  def transferGen: Gen[TransferTransaction]                = versionGen(TransferTransaction).flatMap(transferGen)
  def transferGen(version: Byte): Gen[TransferTransaction] = transferGen(version, ED25519)
  def transferGen(version: Byte, keyType: KeyType): Gen[TransferTransaction] =
    for {
      (sender, recipient, amount, timestamp, fee, attachment) <- transferParamGen(keyType)
      sponsor                                                 <- sponsorGen(version)
    } yield TransferTransaction.signed(version, timestamp, sender, fee, recipient, amount, attachment).sponsorWith(sponsor).explicitGet()

  val transferV1Gen: Gen[TransferTransaction]   = transferGen(1)
  val transferV2Gen: Gen[TransferTransaction]   = transferGen(2)
  val selfTransferGen: Gen[TransferTransaction] = transferV1Gen

  def versionedTransferGenP(sender: PublicKeyAccount, recipient: Address, proofs: Proofs): Gen[TransferTransaction] =
    for {
      version   <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
      amount    <- positiveLongGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield TransferTransaction.create(version, None, timestamp, sender, fee, recipient, amount, Array.emptyByteArray, None, proofs).explicitGet()

  for {
    (sender, _, amount, timestamp, fee, attachment) <- transferParamGen
  } yield TransferTransaction.signed(1, timestamp, sender, fee, sender, amount, attachment).explicitGet()

  def massTransferGen: Gen[MassTransferTransaction] = versionGen(MassTransferTransaction).flatMap(massTransferGen(_, MaxTransferCount))
  def massTransferGen(maxTransfersCount: Int): Gen[MassTransferTransaction] =
    versionGen(MassTransferTransaction).flatMap(massTransferGen(_, maxTransfersCount))
  def massTransferGen(version: Byte, maxTransfersCount: Int): Gen[MassTransferTransaction] = massTransferGen(version, ED25519, maxTransfersCount)
  def massTransferGen(version: Byte, keyType: KeyType, maxTransfersCount: Int): Gen[MassTransferTransaction] =
    for {
      (sender, _, _, timestamp, fee, attachment) <- transferParamGen(keyType)
      transferCount                              <- Gen.choose(0, maxTransfersCount)
      transferGen = for {
        recipient <- addressGen
        amount    <- Gen.choose(1L, Long.MaxValue / maxTransfersCount)
      } yield ParsedTransfer(recipient, amount)
      recipients <- Gen.listOfN(transferCount, transferGen)
    } yield MassTransferTransaction.signed(version, timestamp, sender, fee, recipients, attachment).explicitGet()

  val MinIssueFee = 100000000

  val randomTransactionGen: Gen[Transaction] = Gen.oneOf(
    transferGen,
    anchorTransactionGen,
    assocTransactionGen
//    leaseGen
  )
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

  def dataEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[DataEntry[_]] =
    Gen.oneOf(longEntryGen(keyGen), booleanEntryGen(keyGen), binaryEntryGen(maxSize, keyGen), stringEntryGen(maxSize, keyGen))

  def dataGen(size: Int): Gen[List[DataEntry[_]]] = {
    val maxEntrySize = ((DataTransaction.MaxBytes / (size max 1)) - DataEntry.MaxKeySize) min DataEntry.MaxValueSize
    Gen.listOfN(size, dataEntryGen(maxEntrySize max 64)) suchThat (data => data.flatMap(_.toBytes).toArray.length <= DataTransaction.MaxBytes)
  }

  def dataForScriptGen(size: Int): Gen[List[DataEntry[_]]] =
    Gen.listOfN(size, dataEntryGen(200, dataScriptsKeyGen))

  val dataTransactionGen: Gen[DataTransaction] = dataTransactionGen(DataTransaction.MaxEntryCount)

  def dataTransactionGen(maxEntryCount: Int, useForScript: Boolean = false): Gen[DataTransaction] =
    versionGen(DataTransaction).flatMap(dataTransactionGen(_, maxEntryCount, useForScript))

  def dataTransactionGen(version: Byte, maxEntryCount: Int, useForScript: Boolean): Gen[DataTransaction] =
    for {
      sender    <- accountGen
      timestamp <- timestampGen
      size      <- Gen.choose(0, maxEntryCount)
      fee       = 15000000
      data      <- if (useForScript) dataGen(size) else dataForScriptGen(size)
      uniq = data.foldRight(List.empty[DataEntry[_]]) { (e, es) =>
        if (es.exists(_.key == e.key)) es else e :: es
      }
    } yield DataTransaction.signed(version, timestamp, sender, fee, uniq).explicitGet()

  def dataTransactionGenP(sender: PrivateKeyAccount, data: List[DataEntry[_]]): Gen[DataTransaction] =
    (for {
      version   <- Gen.oneOf(DataTransaction.supportedVersions.toSeq)
      timestamp <- timestampGen
      fee = 150000000
    } yield DataTransaction.signed(version, timestamp, sender, fee, data).explicitGet())

  def preconditionsTransferAndLease(typed: EXPR): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] =
    for {
      master    <- accountGen
      recipient <- accountGen
      timestamp <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, timestamp).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(typed).explicitGet(), timestamp + 1)
      transfer  <- transferGeneratorPV2(timestamp, master, recipient.toAddress, ENOUGH_AMT / 2)
      fee       <- smallFeeGen
      lease = LeaseTransaction.signed(2, timestamp, master, fee, recipient, ENOUGH_AMT / 2).explicitGet()
    } yield (genesis, setScript, lease, transfer)

  def anchorTransactionGen: Gen[AnchorTransaction]                = versionGen(AnchorTransaction).flatMap(anchorTransactionGen)
  def anchorTransactionGen(version: Byte): Gen[AnchorTransaction] = anchorTransactionGen(version, ED25519)
  def anchorTransactionGen(version: Byte, keyType: KeyType): Gen[AnchorTransaction] =
    for {
      sender    <- accountGen(keyType)
      timestamp <- timestampGen
      size      <- Gen.choose(0, AnchorTransaction.MaxEntryCount)
      len       <- Gen.oneOf(AnchorTransaction.EntryLength)
      data      <- Gen.listOfN(size, genBoundedBytes(len, len))
      sponsor   <- sponsorGen(version)
      fee     = 15000000
      anchors = data.map(ByteStr(_))
    } yield AnchorTransaction.signed(version, timestamp, sender, fee, anchors).sponsorWith(sponsor).explicitGet()

  def issueAssocTransactionGen: Gen[IssueAssociationTransaction]                = versionGen(IssueAssociationTransaction).flatMap(issueAssocTransactionGen)
  def issueAssocTransactionGen(version: Byte): Gen[IssueAssociationTransaction] = issueAssocTransactionGen(version, ED25519)
  def issueAssocTransactionGen(version: Byte, keyType: KeyType): Gen[IssueAssociationTransaction] =
    for {
      sender    <- accountGen(keyType)
      timestamp <- timestampGen
      recipient <- accountGen(keyType)
      assocType <- Gen.choose(Int.MinValue, Int.MaxValue)
      expires   <- if (version < 3) Gen.const(None) else Gen.option(timestampGen)
      fee       <- smallFeeGen
      minHashLength = if (version < 3) 0 else 1
      hashOpt <- Gen.option(genBoundedBytes(minHashLength, IssueAssociationTransaction.MaxHashLength).map(ByteStr(_)))
      sponsor <- sponsorGen(version)
    } yield
      IssueAssociationTransaction.signed(version, timestamp, sender, fee, recipient, assocType, expires, hashOpt).sponsorWith(sponsor).explicitGet()

  def revokeAssocTransactionGen: Gen[RevokeAssociationTransaction]                = versionGen(RevokeAssociationTransaction).flatMap(revokeAssocTransactionGen)
  def revokeAssocTransactionGen(version: Byte): Gen[RevokeAssociationTransaction] = revokeAssocTransactionGen(version, ED25519)
  def revokeAssocTransactionGen(version: Byte, keyType: KeyType): Gen[RevokeAssociationTransaction] =
    for {
      sender    <- accountGen(keyType)
      timestamp <- timestampGen
      recipient <- accountGen(keyType)
      assocType <- Gen.choose(Int.MinValue, Int.MaxValue)
      fee       <- smallFeeGen
      minHashLength = if (version < 3) 0 else 1
      hashOpt <- Gen.option(genBoundedBytes(minHashLength, RevokeAssociationTransaction.MaxHashLength).map(ByteStr(_)))
      sponsor <- sponsorGen(version)
    } yield RevokeAssociationTransaction.signed(version, timestamp, sender, fee, recipient, assocType, hashOpt).sponsorWith(sponsor).explicitGet()

  def assocTransactionGen: Gen[AssociationTransaction] = Gen.oneOf(issueAssocTransactionGen, revokeAssocTransactionGen)

  def sponsorshipGen: Gen[SponsorshipTransaction]                = versionGen(SponsorshipTransaction).flatMap(sponsorshipGen)
  def sponsorshipGen(version: Byte): Gen[SponsorshipTransaction] = sponsorshipGen(version, ED25519)
  def sponsorshipGen(version: Byte, keyType: KeyType): Gen[SponsorshipTransaction] =
    for {
      sender    <- accountGen(keyType)
      timestamp <- timestampGen
      recipient <- accountGen(keyType)
      fee       <- smallFeeGen
      sponsor   <- sponsorGen(version)
    } yield SponsorshipTransaction.signed(version, timestamp, sender, fee, recipient).sponsorWith(sponsor).explicitGet()

  def cancelSponsorshipGen: Gen[CancelSponsorshipTransaction]                = versionGen(CancelSponsorshipTransaction).flatMap(cancelSponsorshipGen)
  def cancelSponsorshipGen(version: Byte): Gen[CancelSponsorshipTransaction] = cancelSponsorshipGen(version, ED25519)
  def cancelSponsorshipGen(version: Byte, keyType: KeyType): Gen[CancelSponsorshipTransaction] =
    for {
      sender    <- accountGen(keyType)
      timestamp <- timestampGen
      recipient <- accountGen(keyType)
      fee       <- smallFeeGen
      sponsor   <- sponsorGen(version)
    } yield CancelSponsorshipTransaction.signed(version, timestamp, sender, fee, recipient).sponsorWith(sponsor).explicitGet()

  def registerTransactionGen: Gen[RegisterTransaction]                = versionGen(RegisterTransaction).flatMap(registerTransactionGen)
  def registerTransactionGen(version: Byte): Gen[RegisterTransaction] = registerTransactionGen(version, ED25519)
  def registerTransactionGen(version: Byte, keyType: KeyType): Gen[RegisterTransaction] =
    for {
      sender    <- accountGen(keyType)
      timestamp <- timestampGen
      size      <- Gen.chooseNum(1, RegisterTransaction.MaxEntryCount)
      data      <- Gen.containerOfN[Array, PublicKeyAccount](size, accountGenRandom())
      sponsor   <- sponsorGen(version)
      fee  = 15000000
      accounts = data.toList
    } yield RegisterTransaction.signed(version, timestamp, sender, fee, accounts).sponsorWith(sponsor).explicitGet()
}
