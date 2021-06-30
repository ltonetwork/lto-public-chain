package com.ltonetwork.transaction.genesis

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.Address
import com.ltonetwork.state._
import com.ltonetwork.transaction.TransactionBuilders.{AmountLength, TimestampLength, TypeLength}
import com.ltonetwork.transaction.genesis.GenesisTransaction.create
import com.ltonetwork.transaction.TransactionSerializer
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

object GenesisSerializer extends TransactionSerializer.For[GenesisTransaction] {
  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    val typeBytes      = Array(GenesisTransaction.typeId)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
    val rcpBytes       = recipient.bytes.arr
    require(rcpBytes.length == Address.AddressLength)
    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
    require(res.length == TypeLength + GenesisTransaction.BASE_LENGTH)

    res
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      require(bytes.length >= GenesisTransaction.BASE_LENGTH, "Data does not match base length")

      var position = 0

      val timestampBytes = java.util.Arrays.copyOfRange(bytes, position, position + TimestampLength)
      val timestamp      = Longs.fromByteArray(timestampBytes)
      position += TimestampLength

      val recipientBytes = java.util.Arrays.copyOfRange(bytes, position, position + GenesisTransaction.RECIPIENT_LENGTH)
      val recipient      = Address.fromBytes(recipientBytes).explicitGet()
      position += GenesisTransaction.RECIPIENT_LENGTH

      val amountBytes = java.util.Arrays.copyOfRange(bytes, position, position + AmountLength)
      val amount      = Longs.fromByteArray(amountBytes)

      create(recipient, amount, timestamp).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
}
