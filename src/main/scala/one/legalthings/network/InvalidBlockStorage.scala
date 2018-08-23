package one.legalthings.network

import com.google.common.cache.CacheBuilder
import one.legalthings.network.InvalidBlockStorageImpl._
import one.legalthings.state.ByteStr
import one.legalthings.transaction.ValidationError

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
