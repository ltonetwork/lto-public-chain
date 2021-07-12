package com.ltonetwork.transaction.genesis

import com.google.common.primitives.{Bytes, Longs}
import com.ltonetwork.account.Address
import com.ltonetwork.serialization._
import com.ltonetwork.transaction.genesis.GenesisTransaction.create
import com.ltonetwork.transaction.TransactionSerializer

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

object GenesisSerializer extends TransactionSerializer.For[GenesisTransaction] {
  override def bodyBytes(tx: TransactionT): Array[Byte] = {
    import tx._

    require(recipient.bytes.length == Address.AddressLength)

    Bytes.concat(
      Array(GenesisTransaction.typeId),
      Longs.toByteArray(timestamp),
      recipient.bytes.arr,
      Longs.toByteArray(amount),
    )
  }

  override def parseBytes(version: Byte, bytes: Array[Byte]): Try[TransactionT] = Try {
    require(bytes.length >= Longs.BYTES + Address.AddressLength + Longs.BYTES, "Data does not match base length")

    val buf = ByteBuffer.wrap(bytes)

    val timestamp = buf.getLong
    val recipient = buf.getAddress
    val amount    = buf.getLong

    create(recipient, amount, timestamp)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten
}
