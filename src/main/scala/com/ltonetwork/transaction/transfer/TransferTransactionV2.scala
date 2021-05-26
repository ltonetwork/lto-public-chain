package com.ltonetwork.transaction.transfer

import com.google.common.primitives.Bytes
import com.ltonetwork.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.crypto
import com.ltonetwork.state._
import com.ltonetwork.transaction._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

case class TransferTransactionV2 private (version: Byte,
                                          sender: PublicKeyAccount,
                                          recipient: AddressOrAlias,
                                          amount: Long,
                                          timestamp: Long,
                                          fee: Long,
                                          attachment: Array[Byte],
                                          proofs: Proofs)
    extends TransferTransaction
    with ProvenTransaction
    with FastHashId {

  override val builder: TransactionBuilder     = TransferTransactionV2
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId, version) ++ bytesBase())
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

object TransferTransactionV2 extends TransactionBuilder.For[TransferTransactionV2] with TransactionBuilder.MultipleVersions {

  override val typeId: Byte                 = 4
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- TransferTransaction.parseBase(bytes, 0)
        (sender, timestamp, amount, fee, recipient, attachment, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tt     <- TransferTransactionV2.create(version, sender, recipient, amount, timestamp, fee, attachment, proofs)
      } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             fee: Long,
             attachment: Array[Byte],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- TransferTransaction.validate(amount, fee, attachment)
    } yield TransferTransactionV2(version, sender, recipient, amount, timestamp, fee, attachment, proofs)
  }

  def signed(version: Byte,
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             fee: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, sender, recipient, amount, timestamp, fee, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 fee: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(version, sender, recipient, amount, timestamp, fee, attachment, sender)
  }
}
