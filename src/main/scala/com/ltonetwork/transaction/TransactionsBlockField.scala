package com.ltonetwork.transaction

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import com.ltonetwork.network.TransactionSpec
import play.api.libs.json.{JsArray, JsObject, Json}
import com.ltonetwork.block.BlockField

trait TransactionsBlockField extends BlockField[Seq[Transaction]]

object TransactionsBlockField {
  def apply(version: Int, value: Seq[Transaction]): TransactionsBlockField = version match {
    case 1 | 2 => Version1or2(value)
    case _     => Version3up(value)
  }

  def serTxs(value: Seq[Transaction], serTxCount: Array[Byte]): Array[Byte] = {
    val byteBuffer = new ByteArrayOutputStream(value.size * TransactionSpec.maxLength / 2)
    byteBuffer.write(serTxCount, 0, serTxCount.length)
    value.foreach { tx =>
      val txBytes = tx.bytes()
      val txSize  = Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0)
      byteBuffer.write(txSize, 0, txSize.length)
      byteBuffer.write(txBytes, 0, txBytes.length)
    }
    byteBuffer.toByteArray
  }

  case class Version1or2(override val value: Seq[Transaction]) extends TransactionsBlockField {
    override val name = "transactions"

    override def j: JsObject = Json.obj(name -> JsArray(value.map(_.json())))

    override def b: Array[Byte] = TransactionsBlockField.serTxs(value, Array(value.size.toByte))
  }

  case class Version3up(override val value: Seq[Transaction]) extends TransactionsBlockField {
    override val name = "transactions"

    override def j: JsObject = Json.obj(name -> JsArray(value.map(_.json())))

    override def b: Array[Byte] = {
      val txCount = value.size
      val bb      = ByteBuffer.allocate(4)
      TransactionsBlockField.serTxs(value, bb.putInt(txCount).array)
    }
  }
}
