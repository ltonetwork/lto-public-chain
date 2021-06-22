package com.ltonetwork.transaction.sponsorship

import cats.data.{Validated, ValidatedNel}
import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.{Address, AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.sponsorship.SponsorshipTransaction.supportedVersions
import com.ltonetwork.transaction.{Proofs, Transaction, TransactionBuilder, TransactionSerializer, TxValidator, ValidationError}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

// Base class for Sponsorship and CancelSponsorship transaction
abstract class SponsorshipTransactionBase extends Transaction {
  def recipient: Address
}

object SponsorshipTransactionBase {

  val supportedVersions: Set[Byte] = Set(1)
  val HashLength                   = 64
  def networkByte: Byte = AddressScheme.current.chainId

  trait Validator[T <: SponsorshipTransactionBase] extends TxValidator[T] {
    def validate(tx: T): ValidatedNel[ValidationError, T] = {
      import tx._
      seq(tx)(
        Validated.condNel(supportedVersions.contains(version), None, ValidationError.UnsupportedVersion(version)),
        Validated.condNel(chainId == networkByte, None, ValidationError.WrongChainId(chainId)),
        Validated.condNel(sender.address != recipient.address, None, ValidationError.GenericError("Can't sponsor oneself")),
        Validated.condNel(fee > 0, None, ValidationError.InsufficientFee()),
        Validated.condNel(sponsor.isEmpty || version >= 3, None, ValidationError.UnsupportedFeature(s"Sponsored transaction not supported for tx v$version")),
      )
    }
  }

}