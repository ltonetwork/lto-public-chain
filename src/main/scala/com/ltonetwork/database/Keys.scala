package com.ltonetwork.database

import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.ltonetwork.account.Address
import com.ltonetwork.block.{Block, BlockHeader}
import com.ltonetwork.serialization.Deser
import com.ltonetwork.state._
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.smart.script.{Script, ScriptReader}

import java.nio.ByteBuffer

object Keys {
  private def h(prefix: Short, height: Int): Array[Byte] =
    ByteBuffer.allocate(6).putShort(prefix).putInt(height).array()

  private def hBytes(prefix: Short, height: Int, bytes: Array[Byte]) =
    ByteBuffer.allocate(6 + bytes.length).putShort(prefix).putInt(height).put(bytes).array()

  private def bytes(prefix: Short, bytes: Array[Byte]) =
    ByteBuffer.allocate(2 + bytes.length).putShort(prefix).put(bytes).array()

  private def addr(prefix: Short, addressId: BigInt) = bytes(prefix, addressId.toByteArray)

  private def hash(prefix: Short, hashBytes: ByteStr) = bytes(prefix, hashBytes.arr)

  private def hAddr(prefix: Short, height: Int, addressId: BigInt): Array[Byte] = hBytes(prefix, height, addressId.toByteArray)

  private def historyKey(prefix: Short, b: Array[Byte]) = Key(bytes(prefix, b), readIntSeq, writeIntSeq)

  private def intKey(prefix: Short, default: Int = 0): Key[Int] =
    Key(Shorts.toByteArray(prefix), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  private def bytesSeqNr(prefix: Short, b: Array[Byte]): Key[Int] =
    Key(bytes(prefix, b), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

  private def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)

  // actual key definition

  val version: Key[Int]               = intKey(0, default = 1)
  val height: Key[Int]                = intKey(1)
  def score(height: Int): Key[BigInt] = Key(h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  private def blockAtHeight(height: Int) = h(3, height)

  def blockAt(height: Int): Key[Option[Block]]          = Key.opt[Block](blockAtHeight(height), Block.parseBytes(_).get, _.bytes())
  def blockBytes(height: Int): Key[Option[Array[Byte]]] = Key.opt[Array[Byte]](blockAtHeight(height), identity, identity)
  def blockHeader(height: Int): Key[Option[(BlockHeader, Int)]] =
    Key.opt[(BlockHeader, Int)](blockAtHeight(height), b => (BlockHeader.parseBytes(b).get._1, b.length), unsupported("Can't write block headers")) // this dummy encoder is never used: we only store blocks, not block headers

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int](hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

  def ltoBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(5, addressId.toByteArray)
  def ltoBalance(addressId: BigInt)(height: Int): Key[Long] =
    Key(hAddr(6, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(12, addressId.toByteArray)
  def leaseBalance(addressId: BigInt)(height: Int): Key[LeaseBalance] =
    Key(hAddr(13, height, addressId), readLeaseBalance, writeLeaseBalance)
  def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey(14, leaseId.arr)
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key(hBytes(15, height, leaseId.arr), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  def sponsorshipHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(48, addressId.toByteArray)
  def sponsorshipStatus(addressId: BigInt)(height: Int): Key[List[Address]] =
    Key(
      hAddr(49, height, addressId),
      arr => Deser.parseArrays(arr).map(a => Address.fromBytes(a).explicitGet()).toList,
      (l: List[Address]) => Deser.serializeArrays(l.map(_.bytes.arr))
    )
  def transactionInfo(txId: ByteStr): Key[Option[(Int, Transaction)]] = Key.opt(hash(18, txId), readTransactionInfo, writeTransactionInfo)
  def transactionHeight(txId: ByteStr): Key[Option[Int]] =
    Key.opt(hash(18, txId), readTransactionHeight, unsupported("Can't write transaction height only"))

  def carryFee(height: Int): Key[Long] = Key(h(19, height), Longs.fromByteArray, Longs.toByteArray)

  def changedAddresses(height: Int): Key[Seq[BigInt]] = Key(h(21, height), readBigIntSeq, writeBigIntSeq)

  def transactionIdsAtHeight(height: Int): Key[Seq[ByteStr]] = Key(h(22, height), readTxIds, writeTxIds)

  val lastAddressId: Key[Option[BigInt]] = Key.opt(Array[Byte](0, 24), BigInt(_), _.toByteArray)

  def addressId(address: Address): Key[Option[BigInt]] = Key.opt(bytes(25, address.bytes.arr), BigInt(_), _.toByteArray)
  def idToAddress(id: BigInt): Key[Address]            = Key(bytes(26, id.toByteArray), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  def addressScriptHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(27, addressId.toByteArray)
  def addressScript(addressId: BigInt)(height: Int): Key[Option[Script]] =
    Key.opt(hAddr(28, height, addressId), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  def approvedFeatures: Key[Map[Short, Int]]  = Key(Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  def activatedFeatures: Key[Map[Short, Int]] = Key(Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: BigInt): Key[Int] = Key(addr(31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: BigInt, chunkNo: Int): Key[Seq[String]] =
    Key(addr(32, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  def dataHistory(addressId: BigInt, key: String): Key[Seq[Int]] = historyKey(33, addressId.toByteArray ++ key.getBytes(UTF_8))
  def data(addressId: BigInt, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt(hBytes(34, height, addressId.toByteArray ++ key.getBytes(UTF_8)), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)

  val addressesForLtoSeqNr: Key[Int]                = intKey(37)
  def addressesForLto(seqNr: Int): Key[Seq[BigInt]] = Key(h(38, seqNr), readBigIntSeq, writeBigIntSeq)

  def addressesForAssetSeqNr(assetId: ByteStr): Key[Int]                = bytesSeqNr(39, assetId.arr)
  def addressesForAsset(assetId: ByteStr, seqNr: Int): Key[Seq[BigInt]] = Key(hBytes(40, seqNr, assetId.arr), readBigIntSeq, writeBigIntSeq)

  def addressTransactionSeqNr(addressId: BigInt): Key[Int] = bytesSeqNr(41, addressId.toByteArray)
  def addressTransactionIds(addressId: BigInt, seqNr: Int): Key[Seq[(Int, ByteStr)]] =
    Key(hBytes(42, seqNr, addressId.toByteArray), readTransactionIds, writeTransactionIds)

  def outgoingAssociationsSeqNr(address: ByteStr): Key[Int] = bytesSeqNr(44, address.arr)
  def outgoingAssociationTransactionId(addressBytes: ByteStr, seqNr: Int): Key[Array[Byte]] =
    Key(hBytes(45, seqNr, addressBytes.arr), identity, identity)

  def incomingAssociationsSeqNr(address: ByteStr): Key[Int] = bytesSeqNr(46, address.arr)
  def incomingAssociationTransactionId(addressBytes: ByteStr, seqNr: Int): Key[Array[Byte]] =
    Key(hBytes(47, seqNr, addressBytes.arr), identity, identity)

  def feePrice(height: Int): Key[Option[Long]] = Key.opt[Long](h(48, height), Longs.fromByteArray, Longs.toByteArray)
}
