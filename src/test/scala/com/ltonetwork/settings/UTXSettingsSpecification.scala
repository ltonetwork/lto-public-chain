package com.ltonetwork.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class UTXSettingsSpecification extends AnyFlatSpec with Matchers {
  "UTXSettings" should "read values" in {
    val config   = ConfigFactory.parseString("""lto {
        |  utx {
        |    max-size = 100
        |    max-transaction-age = 100m
        |    cleanup-interval = 10m
        |    blacklist-sender-addresses = ["a"]
        |    allow-blacklisted-transfer-to = ["b"]
        |  }
        |}""".stripMargin).resolve()
    val settings = config.as[UtxSettings]("lto.utx")
    settings.maxSize should be(100)
    settings.maxTransactionAge shouldBe 100.minutes
    settings.cleanupInterval shouldBe 10.minutes
    settings.blacklistSenderAddresses shouldBe Set("a")
    settings.allowBlacklistedTransferTo shouldBe Set("b")
  }
}
