package com.ltonetwork.settings

import com.ltonetwork.Version
import com.ltonetwork.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "lto"
  val AgentName       = s"LTO v${Version.VersionString}"

  val UnitsInLTO = 100000000L
  val TotalLTO   = 1000000000L

  val TotalLTOMain = 500000000L
}
