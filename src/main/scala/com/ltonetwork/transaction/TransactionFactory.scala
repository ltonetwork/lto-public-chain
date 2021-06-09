package com.ltonetwork.transaction

import com.ltonetwork.account._
import com.ltonetwork.api.http.requests.BroadcastRequest
import com.ltonetwork.api.http.requests.anchor.AnchorV1Request
import com.ltonetwork.api.http.requests.association.IssueAssociationV1Request
import com.ltonetwork.api.http.requests.data.DataV1Request
import com.ltonetwork.api.http.requests.lease.{CancelLeaseV1Request, CancelLeaseV2Request, LeaseV1Request, LeaseV2Request}
import com.ltonetwork.api.http.requests.smart.SetScriptV1Request
import com.ltonetwork.api.http.requests.sponsorship.SponsorshipV1Request
import com.ltonetwork.api.http.requests.transfer.{MassTransferV1Request, TransferV1Request, TransferV2Request}
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError.Validation
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.association.{IssueAssociationTransaction, RevokeAssociationTransaction}
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.utils.{Base58, Time}
import com.ltonetwork.wallet.Wallet

// TODO Rename all methods to apply and use overloading
// TODO Do we really need a request class for each version?
object TransactionFactory extends BroadcastRequest {
  def transferAssetV1(request: TransferV1Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransaction] =
    transferAssetV1(request, wallet, request.sender, time)

  def transferAssetV1(request: TransferV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransaction] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransaction.signed(
        1,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def transferAssetV1(request: TransferV1Request, sender: PublicKeyAccount): Either[ValidationError, TransferTransaction] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransaction.create(
        1,
        None,
        0,
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        None,
        Proofs.empty
      )
    } yield tx

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, time: Time): Either[ValidationError, TransferTransaction] =
    transferAssetV2(request, wallet, request.sender, time)

  def transferAssetV2(request: TransferV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, TransferTransaction] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def transferAssetV2(request: TransferV2Request, sender: PublicKeyAccount): Either[ValidationError, TransferTransaction] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- TransferTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        None,
        Proofs.empty
      )
    } yield tx

  def massTransferAsset(request: MassTransferV1Request, wallet: Wallet, time: Time): Either[ValidationError, MassTransferTransaction] =
    massTransferAsset(request, wallet, request.sender, time)

  def massTransferAsset(request: MassTransferV1Request,
                        wallet: Wallet,
                        signerAddress: String,
                        time: Time): Either[ValidationError, MassTransferTransaction] =
    for {
      sender    <- wallet.findPrivateKey(request.sender)
      signer    <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        transfers,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        signer
      )
    } yield tx

  def massTransferAsset(request: MassTransferV1Request, sender: PublicKeyAccount): Either[ValidationError, MassTransferTransaction] =
    for {
      transfers <- MassTransferTransaction.parseTransfersList(request.transfers)
      tx <- MassTransferTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        transfers,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        None,
        Proofs.empty
      )
    } yield tx

  def setScript(request: SetScriptV1Request, wallet: Wallet, time: Time): Either[ValidationError, SetScriptTransaction] =
    setScript(request, wallet, request.sender, time)

  def setScript(request: SetScriptV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SetScriptTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      script <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        script,
        signer
      )
    } yield tx

  def setScript(request: SetScriptV1Request, sender: PublicKeyAccount): Either[ValidationError, SetScriptTransaction] =
    for {
      script <- request.script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      tx <- SetScriptTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        script,
        None,
        Proofs.empty
      )
    } yield tx

  def leaseV1(request: LeaseV1Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransaction] =
    leaseV1(request, wallet, request.sender, time)

  def leaseV1(request: LeaseV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransaction] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransaction.signed(
        1,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        signer
      )
    } yield tx

  def leaseV1(request: LeaseV1Request, sender: PublicKeyAccount): Either[ValidationError, LeaseTransaction] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransaction.create(
        1,
        None,
        0,
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        None,
        Proofs.empty
      )
    } yield tx

  def leaseV2(request: LeaseV2Request, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransaction] =
    leaseV2(request, wallet, request.sender, time)

  def leaseV2(request: LeaseV2Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, LeaseTransaction] =
    for {
      sender       <- wallet.findPrivateKey(request.sender)
      signer       <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        signer
      )
    } yield tx

  def leaseV2(request: LeaseV2Request, sender: PublicKeyAccount): Either[ValidationError, LeaseTransaction] =
    for {
      recipientAcc <- AddressOrAlias.fromString(request.recipient)
      tx <- LeaseTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        recipientAcc,
        request.amount,
        None,
        Proofs.empty
      )
    } yield tx

  def leaseCancelV1(request: CancelLeaseV1Request, wallet: Wallet, time: Time): Either[ValidationError, CancelLeaseTransaction] =
    leaseCancelV1(request, wallet, request.sender, time)

  def leaseCancelV1(request: CancelLeaseV1Request,
                    wallet: Wallet,
                    signerAddress: String,
                    time: Time): Either[ValidationError, CancelLeaseTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- CancelLeaseTransaction.signed(
        1,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        ByteStr.decodeBase58(request.txId).get,
        signer
      )
    } yield tx

  def leaseCancelV1(request: CancelLeaseV1Request, sender: PublicKeyAccount): Either[ValidationError, CancelLeaseTransaction] =
    CancelLeaseTransaction.create(
      1,
      None,
      0,
      sender,
      request.fee,
      ByteStr.decodeBase58(request.txId).get,
      None,
      Proofs.empty
    )

  def leaseCancelV2(request: CancelLeaseV2Request, wallet: Wallet, time: Time): Either[ValidationError, CancelLeaseTransaction] =
    leaseCancelV2(request, wallet, request.sender, time)

  def leaseCancelV2(request: CancelLeaseV2Request,
                    wallet: Wallet,
                    signerAddress: String,
                    time: Time): Either[ValidationError, CancelLeaseTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- CancelLeaseTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        ByteStr.decodeBase58(request.txId).get,
        signer
      )
    } yield tx

  def leaseCancelV2(request: CancelLeaseV2Request, sender: PublicKeyAccount): Either[ValidationError, CancelLeaseTransaction] =
    CancelLeaseTransaction.create(
      request.version,
      None,
      0,
      sender,
      request.fee,
      ByteStr.decodeBase58(request.txId).get,
      None,
      Proofs.empty
    )

  def data(request: DataV1Request, wallet: Wallet, time: Time): Either[ValidationError, DataTransaction] =
    data(request, wallet, request.sender, time)

  def data(request: DataV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, DataTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      tx <- DataTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        request.data,
        signer
      )
    } yield tx

  def data(request: DataV1Request, sender: PublicKeyAccount): Either[ValidationError, DataTransaction] =
    DataTransaction.create(
      request.version,
      None,
      0,
      sender,
      request.fee,
      request.data,
      None,
      Proofs.empty
    )

  def parseAnchors(l: List[String]): Validation[List[ByteStr]] = {
    import cats.implicits._
    l.traverse(s => {
      BroadcastRequest.parseBase58(
        s,
        s"invalid base58 string or anchor too long: max anchor string size = ${AnchorTransaction.MaxAnchorStringSize}",
        AnchorTransaction.MaxAnchorStringSize)
    })
  }

  def anchor(request: AnchorV1Request, wallet: Wallet, time: Time): Either[ValidationError, AnchorTransaction] =
    anchor(request, wallet, request.sender, time)

  def anchor(request: AnchorV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, AnchorTransaction] =
    for {
      sender  <- wallet.findPrivateKey(request.sender)
      signer  <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      anchors <- parseAnchors(request.anchors)
      tx <- AnchorTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        anchors,
        signer
      )
    } yield tx

  def anchor(request: AnchorV1Request, sender: PublicKeyAccount): Either[ValidationError, AnchorTransaction] =
    for {
      anchors <- parseAnchors(request.anchors)
      tx <- AnchorTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        anchors,
        None,
        Proofs.empty
      )
    } yield tx
  
  val IncorectHashMessage = "Incorrect hash length, should be <= 64 bytes"

  def issueAssociation(request: IssueAssociationV1Request, wallet: Wallet, time: Time): Either[ValidationError, IssueAssociationTransaction] =
    issueAssociation(request, wallet, request.sender, time)

  def issueAssociation(request: IssueAssociationV1Request,
                       wallet: Wallet,
                       signerAddress: String,
                       time: Time): Either[ValidationError, IssueAssociationTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipient  <- Address.fromString(request.party)
      hash <- if (request.hash == "") Right(None)
              else parseBase58(request.hash, IncorectHashMessage, IssueAssociationTransaction.StringHashLength).map(Some(_))
      tx <- IssueAssociationTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        request.associationType,
        recipient,
        None,
        hash,
        signer
      )
    } yield tx

  def issueAssociation(request: IssueAssociationV1Request, sender: PublicKeyAccount): Either[ValidationError, IssueAssociationTransaction] =
    for {
      recipient <- Address.fromString(request.party)
      hash <- if (request.hash == "") Right(None)
              else parseBase58(request.hash, IncorectHashMessage, IssueAssociationTransaction.StringHashLength).map(Some(_))
      tx <- IssueAssociationTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        request.associationType,
        recipient,
        None,
        hash,
        None,
        Proofs.empty
      )
    } yield tx

  def revokeAssociation(request: IssueAssociationV1Request, wallet: Wallet, time: Time): Either[ValidationError, RevokeAssociationTransaction] =
    revokeAssociation(request, wallet, request.sender, time)

  def revokeAssociation(request: IssueAssociationV1Request,
                        wallet: Wallet,
                        signerAddress: String,
                        time: Time): Either[ValidationError, RevokeAssociationTransaction] =
    for {
      sender <- wallet.findPrivateKey(request.sender)
      signer <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipient  <- Address.fromString(request.party)
      hash <- if (request.hash == "") Right(None)
              else parseBase58(request.hash, IncorectHashMessage, IssueAssociationTransaction.StringHashLength).map(Some(_))
      tx <- RevokeAssociationTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        request.associationType,
        recipient,
        hash,
        signer
      )
    } yield tx

  def revokeAssociation(request: IssueAssociationV1Request, sender: PublicKeyAccount): Either[ValidationError, RevokeAssociationTransaction] =
    for {
      recipient <- Address.fromString(request.party)
      hash <- if (request.hash == "") Right(None)
              else parseBase58(request.hash, IncorectHashMessage, IssueAssociationTransaction.StringHashLength).map(Some(_))
      tx <- RevokeAssociationTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        request.associationType,
        recipient,
        hash,
        None,
        Proofs.empty
      )
    } yield tx

  def sponsorship(request: SponsorshipV1Request, wallet: Wallet, time: Time): Either[ValidationError, SponsorshipTransaction] =
    sponsorship(request, wallet, request.sender, time)

  def sponsorship(request: SponsorshipV1Request, wallet: Wallet, signerAddress: String, time: Time): Either[ValidationError, SponsorshipTransaction] =
    for {
      sender    <- wallet.findPrivateKey(request.sender)
      signer    <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipient <- Address.fromString(request.recipient)
      tx <- SponsorshipTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        recipient,
        signer
      )
    } yield tx

  def sponsorship(request: SponsorshipV1Request, sender: PublicKeyAccount): Either[ValidationError, SponsorshipTransaction] =
    for {
      recipient <- Address.fromString(request.recipient)
      tx <- SponsorshipTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        recipient,
        None,
        Proofs.empty
      )
    } yield tx

  def cancelSponsorship(request: SponsorshipV1Request, wallet: Wallet, time: Time): Either[ValidationError, CancelSponsorshipTransaction] =
    cancelSponsorship(request, wallet, request.sender, time)

  def cancelSponsorship(request: SponsorshipV1Request,
                        wallet: Wallet,
                        signerAddress: String,
                        time: Time): Either[ValidationError, CancelSponsorshipTransaction] =
    for {
      sender    <- wallet.findPrivateKey(request.sender)
      signer    <- if (request.sender == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
      recipient <- Address.fromString(request.recipient)
      tx <- CancelSponsorshipTransaction.signed(
        request.version,
        request.timestamp.getOrElse(time.getTimestamp()),
        sender,
        request.fee,
        recipient,
        signer
      )
    } yield tx

  def cancelSponsorship(request: SponsorshipV1Request, sender: PublicKeyAccount): Either[ValidationError, CancelSponsorshipTransaction] =
    for {
      recipient <- Address.fromString(request.recipient)
      tx <- CancelSponsorshipTransaction.create(
        request.version,
        None,
        0,
        sender,
        request.fee,
        recipient,
        None,
        Proofs.empty
      )
    } yield tx
}
