package com.ltonetwork.settings

import com.ltonetwork.network.BlockCheckpoint
import com.typesafe.config.Config
import com.ltonetwork.state.ByteStr
import com.ltonetwork.utils.Base58
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class CheckpointsSettings(publicKey: Option[ByteStr], blocks: Seq[BlockCheckpoint])

object CheckpointsSettings {
  val configPath: String = "lto.checkpoints"

  def fromConfig(config: Config): CheckpointsSettings = {
    val publicKey = config.as[Option[ByteStr]](s"$configPath.public-key")
    val blocks = config.as[Seq[BlockCheckpoint]](s"$configPath.blocks")

    CheckpointsSettings(publicKey, blocks)
  }

  implicit val checkpointReader: ValueReader[BlockCheckpoint] = ValueReader.relative { block =>
    BlockCheckpoint(
      block.as[Int]("height"),
      Base58.decode(block.as[String]("signature")).get
    )
  }
}
