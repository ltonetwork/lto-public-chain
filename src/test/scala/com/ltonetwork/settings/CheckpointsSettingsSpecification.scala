package com.ltonetwork.settings

import com.ltonetwork.network.BlockCheckpoint
import com.typesafe.config.ConfigFactory
import com.ltonetwork.utils.Base58
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckpointsSettingsSpecification extends AnyFlatSpec with Matchers {
  "CheckpointsSettings" should "read values" in {
    val config   = ConfigFactory.parseString("""
        |lto {
        |  checkpoints {
        |    public-key = "BASE58PUBKEY"
        |    blocks = [
        |       { height: 1, signature: 47pP5r1Kh159XmxcfG2eQVj6dKNhub3mvGgpJovcw7EcZyJswFLYyKGYNV21BGJ8pwkajA75ZLMWFBdv3BzMRMk },
        |       { height: 300, signature: 4zU5E7VPwwoVpiEgGHnB9Qo5CLVgerKzXe1ny1NpkhrbB7Kgi7y3YV3qQNi1ct5zrGVuKxRAALXNEACtGcBhDDVg }
        |    ]
        |  }
        |}
      """.stripMargin).resolve()
    val settings = CheckpointsSettings.fromConfig(config)

    settings.blocks should be(Seq(
      BlockCheckpoint(1, Base58.decode("47pP5r1Kh159XmxcfG2eQVj6dKNhub3mvGgpJovcw7EcZyJswFLYyKGYNV21BGJ8pwkajA75ZLMWFBdv3BzMRMk").get),
      BlockCheckpoint(300, Base58.decode("4zU5E7VPwwoVpiEgGHnB9Qo5CLVgerKzXe1ny1NpkhrbB7Kgi7y3YV3qQNi1ct5zrGVuKxRAALXNEACtGcBhDDVg").get),
    ))
  }
}
