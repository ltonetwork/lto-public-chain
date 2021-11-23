package com.ltonetwork.settings

import com.typesafe.config.ConfigFactory
import com.ltonetwork.state.ByteStr
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckpointsSettingsSpecification extends AnyFlatSpec with Matchers {
  "CheckpointsSettings" should "read values" in {
    val config   = ConfigFactory.parseString("""
        |lto {
        |  checkpoints {
        |    public-key: "BASE58PUBKEY"
        |  }
        |}
      """.stripMargin).resolve()
    val settings = CheckpointsSettings.fromConfig(config)

    settings.publicKey should be(ByteStr.decodeBase58("BASE58PUBKEY").get)
  }
}
