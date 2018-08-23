package one.legalthings.transaction.transfer

import com.google.common.primitives.Bytes
import one.legalthings.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import one.legalthings.crypto
import one.legalthings.state.ByteStr
import one.legalthings.transaction._
import monix.eval.Coeval
import scorex.crypto.signatures.Curve25519._

import scala.util.{Failure, Success, Try}

case class TransferTransactionV1 private (sender: PublicKeyAccount,
                                          recipient: AddressOrAlias,
                                          amount: Long,
                                          timestamp: Long,
                                          fee: Long,
                                          attachment: Array[Byte],
                                          signature: ByteStr)
    extends TransferTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: TransactionParser     = TransferTransactionV1
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId) ++ bytesBase())
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))
  override val version: Byte                  = 1: Byte
}

object TransferTransactionV1 extends TransactionParserFor[TransferTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 4

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")

      (for {
        parsed <- TransferTransaction.parseBase(bytes, SignatureLength + 1)
        (sender, timestamp, amount, feeAmount, recipient, attachment, _) = parsed
        tt <- TransferTransactionV1.create(sender, recipient, amount, timestamp, feeAmount, attachment, signature)
      } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    TransferTransaction
      .validate(amount, feeAmount, attachment)
      .map(_ => TransferTransactionV1(sender, recipient, amount, timestamp, feeAmount, attachment, signature))
  }

  def signed(sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, recipient, amount, timestamp, feeAmount, attachment, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(sender, recipient, amount, timestamp, feeAmount, attachment, sender)
  }
}
