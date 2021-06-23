package com.ltonetwork

// Please, update the fallback version every major and minor releases.
// This version is used then building from sources without Git repository
// In case of not updating the version nodes build from headless sources will fail to connect to newer versions

object Version {
  val VersionString                 = "1.3.0"
  val VersionTuple: (Int, Int, Int) = (1, 3, 0)
}
