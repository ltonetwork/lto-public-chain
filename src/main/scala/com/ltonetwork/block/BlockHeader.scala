package com.ltonetwork.block

import com.google.common.primitives.Longs
import com.ltonetwork.account.PublicKeyAccount
import com.ltonetwork.block.fields.FeaturesBlockField
import com.ltonetwork.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData}
import com.ltonetwork.serialization._
import com.ltonetwork.state._
import com.ltonetwork.utils.ScorexLogging
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519._

import java.nio.ByteBuffer
import scala.util.{Failure, Try}

class BlockHeader(val timestamp: Long,
                  val version: Byte,
                  val reference: ByteStr,
                  val signerData: SignerData,
                  val consensusData: NxtLikeConsensusBlockData,
                  val transactionCount: Int,
                  val featureVotes: Set[Short],
                  val feeVote: Byte) {
  protected val versionField: ByteBlockField               = ByteBlockField("version", version)
  protected val timestampField: LongBlockField             = LongBlockField("timestamp", timestamp)
  protected val referenceField: BlockIdField               = BlockIdField("reference", reference.arr)
  protected val signerField: SignerDataBlockField          = SignerDataBlockField("signature", signerData)
  protected val consensusField: NxtConsensusBlockField     = NxtConsensusBlockField(consensusData)
  protected val supportedFeaturesField: FeaturesBlockField = FeaturesBlockField(version, featureVotes)
  protected val feeVoteField: ByteBlockField               = ByteBlockField("feeVote", feeVote)

  val headerJson: Coeval[JsObject] = Coeval.evalOnce(
    versionField.json() ++
      timestampField.json() ++
      referenceField.json() ++
      consensusField.json() ++
      supportedFeaturesField.json() ++
      feeVoteField.json() ++
      signerField.json())
}

object BlockHeader extends ScorexLogging {
  def parseBytes(bytes: Array[Byte]): Try[(BlockHeader, Array[Byte])] =
    Try {

      val buf = ByteBuffer.wrap(bytes)

      val version = buf.get
      val timestamp = buf.getLong
      val reference = buf.getSignature

      val cBytesLength = buf.getInt
      val cBytes = buf.getByteArray(cBytesLength)
      val consData =
        NxtLikeConsensusBlockData(Longs.fromByteArray(cBytes.take(Block.BaseTargetLength)), ByteStr(cBytes.takeRight(Block.GeneratorSignatureLength)))

      val tBytesLength = buf.getInt
      val tBytes = buf.getByteArray(tBytesLength)

      val txCount = version match {
        case 1 | 2 => tBytes.head
        case 3 | 4 => ByteBuffer.wrap(tBytes, 0, 4).getInt()
      }

      val supportedFeaturesIds =
        if (version > 2) {
          val featuresCount = buf.getInt
          buf.getShortArray(featuresCount).toSet
        } else {
          Set.empty[Short]
        }

      val feeVote = if (version >= 4) buf.get else 0: Byte

      val genPK = buf.getByteArray(KeyLength)
      val signature = buf.getSignature

      val blockHeader =
        new BlockHeader(timestamp, version, reference, SignerData(PublicKeyAccount(genPK), signature), consData, txCount, supportedFeaturesIds, feeVote)
      (blockHeader, tBytes)
    }.recoverWith {
      case t: Throwable =>
        log.error("Error when parsing block", t)
        Failure(t)
    }

  def json(bh: BlockHeader, blockSize: Int): JsObject =
    bh.headerJson() ++
      Json.obj(
        "blocksize"        -> blockSize,
        "transactionCount" -> bh.transactionCount
      )

}
