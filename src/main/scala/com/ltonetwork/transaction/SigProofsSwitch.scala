package com.ltonetwork.transaction

import com.ltonetwork.state.ByteStr

trait SigProofsSwitch extends Transaction { self: VersionedTransaction =>
  def usesLegacySignature: Boolean =
    self.version == 1

  def signature: ByteStr = proofs.toSignature
}
