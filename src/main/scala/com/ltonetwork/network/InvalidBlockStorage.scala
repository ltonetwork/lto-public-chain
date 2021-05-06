package com.ltonetwork.network

import com.google.common.cache.CacheBuilder
import com.ltonetwork.network.InvalidBlockStorageImpl._
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.ValidationError

import scala.concurrent.duration.FiniteDuration

trait InvalidBlockStorage {
  def add(blockId: ByteStr, validationError: ValidationError): Unit

  def find(blockId: ByteStr): Option[ValidationError]
}

class InvalidBlockStorageImpl(settings: InvalidBlockStorageSettings) extends InvalidBlockStorage {
  private val cache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(settings.timeout.length, settings.timeout.unit)
    .build[ByteStr, ValidationError]()

  override def add(blockId: ByteStr, validationError: ValidationError): Unit = cache.put(blockId, validationError)

  override def find(blockId: ByteStr): Option[ValidationError] = Option(cache.getIfPresent(blockId))
}

object InvalidBlockStorageImpl {

  case class InvalidBlockStorageSettings(maxSize: Int, timeout: FiniteDuration)

}
