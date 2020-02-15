package com.wavesplatform.settings

import java.io.File

import com.wavesplatform.state.ByteStr

case class WalletSettings(
                           file: Option[File],
                           password: String,
                           seed: Option[ByteStr],
                           seedPhrase: Option[String],
                           accountSeed:Option[ByteStr]
                         ) {
  require(List(seed,seedPhrase,accountSeed).count(_.nonEmpty) <= 1, "Can only have one: seed, seedPhrase, accountSeed")
  def miningWalletSeed: Option[ByteStr] = seed // or else use seedPhrase
}
