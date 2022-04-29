package com.ltonetwork.history

import com.ltonetwork.crypto
import com.ltonetwork.db.{CheckpointCodec, PropertiesStorage, SubStorage}
import com.ltonetwork.network.Checkpoint
import com.ltonetwork.settings.CheckpointsSettings
import com.ltonetwork.transaction.ValidationError.GenericError
import com.ltonetwork.transaction.{CheckpointService, ValidationError}
import org.iq80.leveldb.DB

class CheckpointServiceImpl(db: DB, settings: CheckpointsSettings)
    extends SubStorage(db, "checkpoints")
    with PropertiesStorage
    with CheckpointService {

  private val CheckpointProperty = "checkpoint"

  private def getFromDB: Option[Checkpoint] = getProperty(CheckpointProperty)
    .flatMap(b => CheckpointCodec.decode(b).toOption.map(r => r.value))

  override def get: Checkpoint = getFromDB
    .getOrElse(Checkpoint(settings.blocks, Array.emptyByteArray))

  override def set(cp: Checkpoint): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(!(get.signature sameElements cp.signature), (), GenericError("Checkpoint already applied"))
      _ <- Either.cond(
        settings.publicKey.exists(key => crypto.verify(cp.signature, cp.toSign, key)),
        putProperty(CheckpointProperty, CheckpointCodec.encode(cp), None),
        GenericError("Invalid checkpoint signature")
      )
    } yield ()

}
