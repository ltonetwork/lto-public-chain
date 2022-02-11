package com.ltonetwork.http

import com.ltonetwork.TestWallet
import com.ltonetwork.settings.LtoSettings
import com.ltonetwork.api.ApiKeyNotValid

class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet {
  private val sampleConfig = com.typesafe.config.ConfigFactory.load()
  private val ltoSettings  = LtoSettings.fromConfig(sampleConfig)
  private val configObject = sampleConfig.root()
  private val route =
    DebugApiRoute(ltoSettings, null, null, null, null, null, null, null, null, null, null, null, null, configObject).route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }
}
