package com.wavesplatform.settings

import com.wavesplatform.Version
import com.wavesplatform.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "lto"
  val AgentName       = s"LTO v${Version.VersionString}"

  val UnitsInWave = 100000000L
  val TotalWaves  = 100000000L

  val UnitsInLTO = 100000000L
  val TotalLTO   = 1000000000L
}
