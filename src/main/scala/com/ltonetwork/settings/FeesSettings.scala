package com.ltonetwork.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

import scala.collection.JavaConverters._

case class FeeSettings(asset: String, fee: Long)
case class FeesSettings(fees: Map[Byte, Seq[FeeSettings]])

object FeesSettings {
  val configPath: String = "lto.fees"

  def fromConfig(config: Config): FeesSettings = {
    val fees: Map[Byte, Seq[FeeSettings]] = config
      .getObject(configPath)
      .entrySet()
      .asScala
      .flatMap { entry =>
        if (transactionTypes.contains(entry.getKey)) {
          val rawFees = config.as[Map[String, Long]](s"$configPath.${entry.getKey}")
          val fees    = rawFees.map { case (asset, fee) => FeeSettings(asset, fee) }(collection.breakOut)
          Some(transactionTypes(entry.getKey) -> fees)
        } else
          throw new NoSuchElementException(entry.getKey)
      }(collection.breakOut)

    FeesSettings(fees)
  }
}
