package one.legalthings.settings

import com.typesafe.config.ConfigException.WrongType
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class FeesSettingsSpecification extends FlatSpec with Matchers {
  "FeesSettings" should "read values" in {
    val config = ConfigFactory.parseString("""lto {
        |  network.file = "xxx"
        |  fees {
        |    transfer.WAVES = 100000
        |  }
        |  miner.timeout = 10
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(1)
    settings.fees(4) should be(List(FeeSettings("WAVES", 100000)))
  }

  it should "combine read few fees for one transaction type" in {
    val config = ConfigFactory.parseString("""lto.fees {
        |  transfer {
        |    WAVES4 = 444
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(1)
    settings.fees(4).toSet should equal(Set(FeeSettings("WAVES4", 444)))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString("lto.fees {}".stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
    val config = ConfigFactory
      .parseString("lto.fees.transfer.WAVES = 100001")
      .withFallback(
        ConfigFactory.parseString("""lto.fees {
          |  transfer.WAVES = 100000
          |}
        """.stripMargin)
      )
      .resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(1)
    settings.fees(4).toSet should equal(Set(FeeSettings("WAVES", 100001)))
  }

  it should "fail on incorrect long values" in {
    val config = ConfigFactory.parseString("""lto.fees {
        |  transfer.WAVES=N/A
        |}""".stripMargin).resolve()
    intercept[WrongType] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "not fail on long values as strings" in {
    val config   = ConfigFactory.parseString("""lto.fees {
        |  transfer.WAVES="1000"
        |}""".stripMargin).resolve()
    val settings = FeesSettings.fromConfig(config)
    settings.fees(4).toSet should equal(Set(FeeSettings("WAVES", 1000)))
  }

  it should "fail on unknown transaction type" in {
    val config = ConfigFactory.parseString("""lto.fees {
        |  shmayment.WAVES=100
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
        |    LTO = 300000
        |    "6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL" = 1
        |  }
        |  lease {
        |    LTO = 700000
        |  }
        |  lease-cancel {
        |    LTO = 800000
        |  }
        |  mass-transfer {
        |    LTO = 10000
        |  }
        |  data {
        |    LTO = 200000
        |  }
        |  # set-script {
        |  #  LTO = 300000
        |  # }
        |}
      """.stripMargin).withFallback(defaultConfig).resolve()
    val settings      = FeesSettings.fromConfig(config)
    settings.fees(4).toSet should equal(Set(FeeSettings("LTO", 300000), FeeSettings("6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL", 1)))
    settings.fees(8).toSet should equal(Set(FeeSettings("LTO", 700000)))
    settings.fees(9).toSet should equal(Set(FeeSettings("LTO", 800000)))
    settings.fees(11).toSet should equal(Set(FeeSettings("LTO", 10000)))
    settings.fees(12).toSet should equal(Set(FeeSettings("LTO", 200000)))
  }
}
