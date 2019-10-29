package com.wavesplatform.transaction

import com.wavesplatform.account._
import com.wavesplatform.api.http.assets._
import com.wavesplatform.api.http.leasing.{LeaseCancelV1Request, LeaseCancelV2Request, LeaseV1Request, LeaseV2Request}
import com.wavesplatform.api.http.{AnchorRequest, BroadcastRequest, DataRequest}
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.ValidationError.Validation
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransactionV1, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{Base58, Time}
import com.wavesplatform.wallet.Wallet

object TransactionFactory extends BroadcastRequest {

  private val EmptySignature = ByteStr(Array.fill(SignatureLength)(0: Byte))

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV1] =
    transferAssetV1(request, wallet, request.sender, time)

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransactionV1] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV1.signed(
        sender,
        recipientAcc,
        request.amount,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def transferAssetV1(request: TransferV1Request, sender: PublicKeyAccount): Either[ValidationError, TransferTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV1.create(
        sender,
        recipientAcc,
        request.amount,
        0,
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        EmptySignature
      )
    } yield tx

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransactionV2] =
    transferAssetV2(request, wallet, request.sender, time)

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransactionV2] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV2.signed(
        request.version,
        sender,
        recipientAcc,
        request.amount,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def transferAssetV2(request: TransferV2Request, sender: PublicKeyAccount): Either[ValidationError, TransferTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransactionV2.create(
        request.version,
        sender,
        recipientAcc,
        request.amount,
        0,
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        Proofs.empty
      )
    } yield tx

  def massTransferAsset(request: MassTransferRequest, wallet: Wallet, time: Time): Either[ValidationError, MassTransferTransaction] =
    massTransferAsset(request, wallet, request.sender, time)

  def massTransferAsset(request: MassTransferRequest,
                        wallet: Wallet,
                        signerAddress: String,
                        time: Time): Either[ValidationError, MassTransferTransaction] =
    for {
      sender    <- wallet.findPrivateKey(request.sender)
      signer    <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.signed(
        request.version,
        sender,
        transfers,
        request.timestamp.getOrElse(time.getTimestamp()),
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def massTransferAsset(request: MassTransferRequest, sender: PublicKeyAccount): Either[ValidationError, MassTransferTransaction] =
    for {
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.create(
        request.version,
        sender,
        transfers,
        0,
        request.fee,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        Proofs.empty
      )
    } yield tx

  def setScript(request: SetScriptRequest, wallet: Wallet, time: Time): Either[ValidationError, SetScriptTransaction] =
    setScript(request, wallet, request.sender, time)

  def setScript(request: SetScriptRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SetScriptTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      script <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.signed(
        request.version,
        sender,
        script,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def setScript(request: SetScriptRequest, sender: PublicKeyAccount): Either[ValidationError, SetScriptTransaction] =
    for {
      script <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.create(
        request.version,
        sender,
        script,
        request.fee,
        0,
        Proofs.empty
      )
    } yield tx

  def leaseV1(request: LeaseV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV1] =
    leaseV1(request, wallet, request.sender, time)

  def leaseV1(request: LeaseV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransactionV1] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV1.signed(
        sender,
        request.amount,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        recipientAcc,
        signer
      )
    } yield tx

  def leaseV1(request: LeaseV1Request, sender: PublicKeyAccount): Either[ValidationError, LeaseTransactionV1] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV1.create(
        sender,
        request.amount,
        request.fee,
        0,
        recipientAcc,
        EmptySignature
      )
    } yield tx

  def leaseV2(request: LeaseV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransactionV2] =
    leaseV2(request, wallet, request.sender, time)

  def leaseV2(request: LeaseV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransactionV2] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV2.signed(
        request.version,
        sender,
        request.amount,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        recipientAcc,
        signer
      )
    } yield tx

  def leaseV2(request: LeaseV2Request, sender: PublicKeyAccount): Either[ValidationError, LeaseTransactionV2] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransactionV2.create(
        request.version,
        sender,
        request.amount,
        request.fee,
        0,
        recipientAcc,
        Proofs.empty
      )
    } yield tx

  def leaseCancelV1(request: LeaseCancelV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    leaseCancelV1(request, wallet, request.sender, time)

  def leaseCancelV1(request: LeaseCancelV1Request,
                    wallet: Wallet,
                    signerAddress: String,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV1] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- LeaseCancelTransactionV1.signed(
        sender,
        ByteStr.decodeBase58(request.txId).get,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def leaseCancelV1(request: LeaseCancelV1Request, sender: PublicKeyAccount): Either[ValidationError, LeaseCancelTransactionV1] =
    LeaseCancelTransactionV1.create(
      sender,
      ByteStr.decodeBase58(request.txId).get,
      request.fee,
      0,
      EmptySignature
    )

  def leaseCancelV2(request: LeaseCancelV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    leaseCancelV2(request, wallet, request.sender, time)

  def leaseCancelV2(request: LeaseCancelV2Request,
                    wallet: Wallet,
                    signerAddress: String,
                    time: Time): Either[ValidationError, LeaseCancelTransactionV2] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- LeaseCancelTransactionV2.signed(
        request.version,
        AddressScheme.current.chainId,
        sender,
        ByteStr.decodeBase58(request.txId).get,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def leaseCancelV2(request: LeaseCancelV2Request, sender: PublicKeyAccount): Either[ValidationError, LeaseCancelTransactionV2] =
    LeaseCancelTransactionV2.create(
      request.version,
      AddressScheme.current.chainId,
      sender,
      ByteStr.decodeBase58(request.txId).get,
      request.fee,
      0,
      Proofs.empty
    )

  def data(request: DataRequest, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    data(request, wallet, request.sender, time)

  def data(request: DataRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, DataTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- DataTransaction.signed(
        request.version,
        sender,
        request.data,
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def data(request: DataRequest, sender: PublicKeyAccount): Either[ValidationError, DataTransaction] =
    DataTransaction.create(
      request.version,
      sender,
      request.data,
      request.fee,
      0,
      Proofs.empty
    )

  def parseAnchors(l: List[String]): Validation[List[ByteStr]] = {
    import cats.implicits._
    l.traverse(s => {
      BroadcastRequest.parseBase58(s,
                                   s"invalid base58 string or anchor too long: max anchor string size = ${Proofs.MaxAnchorStringSize}",
                                   Proofs.MaxAnchorStringSize)
    })
  }
  import com.wavesplatform.api.http._

  def anchor(request: AnchorRequest, wallet: Wallet, time: Time): Either[ValidationError, AnchorTransaction] =
    anchor(request, wallet, request.sender, time)

  def anchor(request: AnchorRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, AnchorTransaction] =
    for {
      sender  <- wallet.findPrivateKey(request.sender)
      signer  <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      anchors <- parseAnchors(request.anchors)
      tx <- AnchorTransaction.signed(
        request.version,
        sender,
        anchors.map(AnchorRequest.prependZeros),
        request.fee,
        request.timestamp.getOrElse(time.getTimestamp()),
        signer
      )
    } yield tx

  def anchor(request: AnchorRequest, sender: PublicKeyAccount): Either[ValidationError, AnchorTransaction] =
    for {
      anchors <- parseAnchors(request.anchors)
      tx <- AnchorTransaction.create(
        request.version,
        sender,
        anchors.map(AnchorRequest.prependZeros),
        request.fee,
        0,
        Proofs.empty
      )
    } yield tx

  def association(request: AssociationRequest, wallet: Wallet, time: Time): Either[ValidationError, AssociationTransaction] =
    association(request, wallet, request.sender, time)

  def association(request: AssociationRequest, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, AssociationTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      party  <- Address.fromString(request.party)
      hash <- if (request.hash == "") Right(None)
      else parseBase58(request.hash, "Incorrect hash", AssociationTransaction.StringHashLength).map(Some(_))
      action <- AssociationTransaction.ActionType.fromString(request.action)
      tx <- AssociationTransaction.signed(
        version = request.version,
        sender = sender,
        party = party,
        assocType = request.associationType,
        hash = hash.map(AnchorRequest.prependZeros),
        action = action,
        feeAmount = request.fee,
        timestamp = request.timestamp.getOrElse(time.getTimestamp()),
        signer = signer
      )
    } yield tx

  def association(request: AssociationRequest, sender: PublicKeyAccount): Either[ValidationError, AssociationTransaction] =
    for {
      party <- Address.fromString(request.party)
      hash <- if (request.hash == "") Right(None)
      else parseBase58(request.hash, "Incorrect hash", AssociationTransaction.StringHashLength).map(Some(_))
      action <- AssociationTransaction.ActionType.fromString(request.action)
      tx <- AssociationTransaction.create(
        version = request.version,
        sender = sender,
        party = party,
        assocType = request.associationType,
        hash = hash.map(AnchorRequest.prependZeros),
        action = action,
        feeAmount = request.fee,
        timestamp = 0,
        proofs = Proofs.empty
      )
    } yield tx
}
