package one.legalthings.api.http

import one.legalthings.state.ByteStr
import one.legalthings.transaction.ValidationError
import one.legalthings.transaction.ValidationError.Validation

trait BroadcastRequest {
  protected def parseBase58(v: String, error: String, maxLength: Int): Validation[ByteStr] =
    if (v.length > maxLength) Left(ValidationError.GenericError(error))
    else ByteStr.decodeBase58(v).toOption.toRight(ValidationError.GenericError(error))

  protected def parseBase58(v: Option[String], error: String, maxLength: Int): Validation[ByteStr] =
    v.fold[Either[ValidationError, ByteStr]](Right(ByteStr(Array.emptyByteArray)))(_v => parseBase58(_v, error, maxLength))

  protected def parseBase58ToOption(v: Option[String], error: String, maxLength: Int): Validation[Option[ByteStr]] =
    v.fold[Either[ValidationError, Option[ByteStr]]](Right(None)) { s =>
      parseBase58(s, error, maxLength).map(b => Option(b))
    }
}
