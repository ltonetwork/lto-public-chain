package com.ltonetwork.http

import com.typesafe.config.ConfigFactory
import com.ltonetwork.crypto
import com.ltonetwork.settings.RestAPISettings
import com.ltonetwork.utils.Base58

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"

  lazy val restAPISettings = {
    val keyHash = Base58.encode(crypto.secureHash(apiKey.getBytes()))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"lto.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }
}
