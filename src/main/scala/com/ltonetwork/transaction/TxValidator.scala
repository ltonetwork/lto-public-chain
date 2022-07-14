package com.ltonetwork.transaction

import cats.implicits._
import cats.data.{Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import com.ltonetwork.state.DataEntry
import com.ltonetwork.transaction.data.DataTransaction.MaxEntryCount

import scala.annotation.implicitNotFound
import scala.util.Try

@implicitNotFound("No impllicit transaction validator found for transaction ${T}")
trait TxValidator[T <: Transaction] {
  def validate(tx: T): ValidatedNel[ValidationError, T]

  protected def seq(tx: T)(validations: ValidatedNel[ValidationError, Any]*): ValidatedNel[ValidationError, T] = {
    validations.map(_.map(_ => tx)).fold(Validated.validNel(tx)) {
      case (Invalid(leftErrs), Invalid(rightErrs)) => Invalid(leftErrs.concatNel(rightErrs))
      case (invalid @ Invalid(_), _)               => invalid
      case (_, invalid @ Invalid(_))               => invalid
      case (Valid(_), Valid(_))                    => Valid(tx)
    }
  }
}
