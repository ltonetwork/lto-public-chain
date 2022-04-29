package com.ltonetwork.transaction

import com.ltonetwork.network.{BlockCheckpoint, Checkpoint}
import com.ltonetwork.state._

trait CheckpointService {

  def set(checkpoint: Checkpoint): Either[ValidationError, Unit]

  def get: Checkpoint
}

object CheckpointService {

  implicit class CheckpointServiceExt(cs: CheckpointService) {
    def isBlockValid(candidateSignature: ByteStr, estimatedHeight: Int): Boolean =
      !cs.get.items.exists {
        case BlockCheckpoint(h, sig) =>
          h == estimatedHeight && candidateSignature != ByteStr(sig)
      }
  }

}
