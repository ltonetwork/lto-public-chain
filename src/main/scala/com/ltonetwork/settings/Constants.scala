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

  lazy val TransactionNames: Map[Byte, String] =
    Map(
      (1: Byte)  -> "genesis",
      (4: Byte)  -> "transfer",
      (8: Byte)  -> "lease",
      (9: Byte)  -> "cancel lease",
      (11: Byte) -> "mass transfer",
      (12: Byte) -> "data",
      (13: Byte) -> "set script",
      (15: Byte) -> "anchor",
      (16: Byte) -> "issue association",
      (17: Byte) -> "revoke association",
      (18: Byte) -> "sponsorship",
      (19: Byte) -> "cancel sponsorship",
      (20: Byte) -> "register",
      (21: Byte) -> "burn",
      (22: Byte) -> "mapped anchor"
    )
}
