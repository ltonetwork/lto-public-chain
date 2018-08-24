package one.legalthings.http

import com.typesafe.config.ConfigFactory
import one.legalthings.crypto
import one.legalthings.settings.RestAPISettings
import one.legalthings.utils.Base58

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
