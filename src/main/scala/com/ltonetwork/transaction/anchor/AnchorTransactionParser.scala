package com.ltonetwork.transaction.anchor

import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.{TransactionParser, ValidationError}

trait AnchorTransactionParser extends TransactionParser.MultipleVersions {
  override val typeId: Byte                 = 15

  val EntryLength       = List(16, 20, 32, 48, 64)
  val NewMaxEntryLength = 64
  val MaxBytes          = 150 * 1024
  val MaxEntryCount     = 100

  def validate(version: Byte,
             data: List[ByteStr],
             feeAmount: Long): Either[ValidationError, None.type] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (data.lengthCompare(MaxEntryCount) > 0) {
      Left(ValidationError.TooBigArray)
    } else if (data.exists(a => !EntryLength.contains(a.arr.length))) {
      Left(ValidationError.GenericError(s"Anchor can only be of length $EntryLength Bytes"))
    } else if (data.distinct.lengthCompare(data.size) < 0) {
      Left(ValidationError.GenericError("Duplicate anchor in one tx found"))
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(None)
    }
  }
}
