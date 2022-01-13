package com.ltonetwork.settings

import com.typesafe.config.ConfigException.WrongType
import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FeesSettingsSpecification extends AnyFlatSpec with Matchers {
  "FeesSettings" should "read values" in {
    val config = ConfigFactory.parseString("""lto {
        |  network.file = "xxx"
        |  fees {
        |    transfer.BASE = 100000
        |  }
        |  miner.timeout = 10
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(1)
    settings.fees(4) should be(List(FeeSettings("BASE", 100000)))
  }

  it should "combine read few fees for one transaction type" in {
    val config = ConfigFactory.parseString("""lto.fees {
        |  transfer {
        |    VAR = 444
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(1)
    settings.fees(4).toSet should equal(Set(FeeSettings("VAR", 444)))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString("lto.fees {}".stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
    val config = ConfigFactory
      .parseString("lto.fees.transfer.BASE = 100001")
      .withFallback(
        ConfigFactory.parseString("""lto.fees {
          |  transfer.BASE = 100000
          |}
        """.stripMargin)
      )
      .resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(1)
    settings.fees(4).toSet should equal(Set(FeeSettings("BASE", 100001)))
  }

  it should "fail on incorrect long values" in {
    val config = ConfigFactory.parseString("""lto.fees {
        |  transfer.BASE=N/A
        |}""".stripMargin).resolve()
    intercept[WrongType] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "not fail on long values as strings" in {
    val config   = ConfigFactory.parseString("""lto.fees {
        |  transfer.BASE="1000"
        |}""".stripMargin).resolve()
    val settings = FeesSettings.fromConfig(config)
    settings.fees(4).toSet should equal(Set(FeeSettings("BASE", 1000)))
  }

  it should "fail on unknown transaction type" in {
    val config = ConfigFactory.parseString("""lto.fees {
        |  shmayment.BASE=100
        |}""".stripMargin).resolve()
    intercept[NoSuchElementException] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "override values from default config" in {
    val defaultConfig = ConfigFactory.load()
    val config        = ConfigFactory.parseString("""
        |lto.fees {
        |  transfer {
        |    BASE = 300000
        |  }
        |  lease {
        |    BASE = 700000
        |  }
        |  cancel-lease {
        |    BASE = 800000
        |  }
        |  mass-transfer {
        |    BASE = 10000
        |    VAR  = 5000
        |  }
        |  # set-script {
        |  #  BASE = 300000
        |  # }
        |}
      """.stripMargin).withFallback(defaultConfig).resolve()
    val settings      = FeesSettings.fromConfig(config)
    settings.fees(4).toSet should equal(Set(FeeSettings("BASE", 300000)))
    settings.fees(8).toSet should equal(Set(FeeSettings("BASE", 700000)))
    settings.fees(9).toSet should equal(Set(FeeSettings("BASE", 800000)))
    settings.fees(11).toSet should equal(Set(FeeSettings("BASE", 10000), FeeSettings("VAR", 5000)))
  }
}
