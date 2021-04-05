package com.ltonetwork

import java.io.File
import java.nio.ByteBuffer
import java.util

import com.typesafe.config.ConfigFactory
import com.ltonetwork.account.{Address, AddressScheme}
import com.ltonetwork.database.{Keys, LevelDBWriter}
import com.ltonetwork.db.openDB
import com.ltonetwork.settings.{WavesSettings, loadConfig}
import com.ltonetwork.state.{ByteStr, EitherExt2}
import com.ltonetwork.utils.{Base58, Base64, ScorexLogging}
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.collection.JavaConverters._
import scala.util.Try

object Explorer extends ScorexLogging {
  case class Stats(entryCount: Long, totalKeySize: Long, totalValueSize: Long)

  private val keys = Array(
    "version",
    "height",
    "score",
    "block-at-height",
    "height-of",
    "waves-balance-history",
    "waves-balance",
    "assets-for-address",
    "asset-balance-history",
    "asset-balance",
    "asset-info-history",
    "asset-info",
    "lease-balance-history",
    "lease-balance",
    "lease-status-history",
    "lease-status",
    "filled-volume-and-fee-history",
    "filled-volume-and-fee",
    "transaction-info",
    "address-transaction-history",
    "address-transaction-ids-at-height",
    "changed-addresses",
    "transaction-ids-at-height",
    "address-id-of-alias",
    "last-address-id",
    "address-to-id",
    "id-of-address",
    "address-script-history",
    "address-script",
    "approved-features",
    "activated-features",
    "data-key-chunk-count",
    "data-key-chunk",
    "data-history",
    "data",
    "sponsorship-history",
    "sponsorship",
    "addresses-for-waves-seq-nr",
    "addresses-for-waves",
    "addresses-for-asset-seq-nr",
    "addresses-for-asset",
    "address-transaction-ids-seq-nr",
    "address-transaction-ids",
    "alias-is-disabled"
  )

  def main(args: Array[String]): Unit = {
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename = Try(args(0)).toOption.getOrElse("lto-testnet.conf")

    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    log.info(s"Data directory: ${settings.dataDirectory}")

    val db     = openDB(settings.dataDirectory)
    val reader = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")
    try {

      val flag = args(1).toUpperCase

      flag match {
        case "B" =>
          val maybeBlockId = Base58.decode(args(2)).toOption.map(ByteStr.apply)
          if (maybeBlockId.isDefined) {
            val kBlockHeight     = Keys.heightOf(maybeBlockId.get)
            val blockHeightBytes = db.get(kBlockHeight.keyBytes)
            val maybeBlockHeight = kBlockHeight.parse(blockHeightBytes)
            maybeBlockHeight.foreach { h =>
              val kBlock     = Keys.blockBytes(h)
              val blockBytes = db.get(kBlock.keyBytes)
              log.info(s"BlockId=${maybeBlockId.get} at h=$h: ${Base64.encode(blockBytes)}")
            }
          } else log.error("No block ID was provided")

        case "A" =>
          val address   = Address.fromString(args(2)).explicitGet()
          val aid       = Keys.addressId(address)
          val addressId = aid.parse(db.get(aid.keyBytes)).get
          log.info(s"Address id = $addressId")

          val kwbh = Keys.wavesBalanceHistory(addressId)
          val wbh  = kwbh.parse(db.get(kwbh.keyBytes))

          val balances = wbh.map { h =>
            val k = Keys.wavesBalance(addressId)(h)
            h -> k.parse(db.get(k.keyBytes))
          }
          balances.foreach(b => log.info(s"h = ${b._1}: balance = ${b._2}"))

        case "AC" =>
          val lastAddressId = Keys.lastAddressId.parse(db.get(Keys.lastAddressId.keyBytes))
          log.info(s"Last address id: $lastAddressId")

        case "AD" =>
          val result        = new util.HashMap[Address, java.lang.Integer]()
          val lastAddressId = Keys.lastAddressId.parse(db.get(Keys.lastAddressId.keyBytes))
          for (id <- BigInt(1) to lastAddressId.getOrElse(BigInt(0))) {
            val k       = Keys.idToAddress(id)
            val address = k.parse(db.get(k.keyBytes))
            result.compute(address,
                           (_, prev) =>
                             prev match {
                               case null    => 1
                               case notNull => 1 + notNull
                           })
          }

          for ((k, v) <- result.asScala if v > 1) {
            log.info(s"$k,$v")
          }

        case "S" =>
          log.info("Collecting DB stats")
          val iterator = db.iterator()
          val result   = new util.HashMap[Short, Stats]
          iterator.seekToFirst()
          while (iterator.hasNext) {
            val entry     = iterator.next()
            val keyPrefix = ByteBuffer.wrap(entry.getKey).getShort
            result.compute(
              keyPrefix,
              (_, maybePrev) =>
                maybePrev match {
                  case null => Stats(1, entry.getKey.length, entry.getValue.length)
                  case prev => Stats(prev.entryCount + 1, prev.totalKeySize + entry.getKey.length, prev.totalValueSize + entry.getValue.length)
              }
            )
          }
          iterator.close()

          log.info("key-space,entry-count,total-key-size,total-value-size")
          for ((prefix, stats) <- result.asScala) {
            log.info(s"${keys(prefix)},${stats.entryCount},${stats.totalKeySize},${stats.totalValueSize}")
          }
      }
    } finally db.close()
  }
}
