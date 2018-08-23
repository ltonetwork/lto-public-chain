package one.legalthings.transaction.transfer

import com.google.common.primitives.Bytes
import one.legalthings.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import one.legalthings.crypto
import one.legalthings.state._
import one.legalthings.transaction._
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

  override val builder: TransactionParser     = TransferTransactionV2
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId, version) ++ bytesBase())
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

object TransferTransactionV2 extends TransactionParserFor[TransferTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 4
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- TransferTransaction.parseBase(bytes, 0)
        (sender, timestamp, amount, feeAmount, recipient, attachment, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tt     <- TransferTransactionV2.create(version, sender, recipient, amount, timestamp, feeAmount, attachment, proofs)
      } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- TransferTransaction.validate(amount, feeAmount, attachment)
    } yield TransferTransactionV2(version, sender, recipient, amount, timestamp, feeAmount, attachment, proofs)
  }

  def signed(version: Byte,
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, sender, recipient, amount, timestamp, feeAmount, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(version, sender, recipient, amount, timestamp, feeAmount, attachment, sender)
  }
}
