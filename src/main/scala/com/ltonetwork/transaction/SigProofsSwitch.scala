package com.ltonetwork.transaction

import com.ltonetwork.state.ByteStr

trait SigProofsSwitch extends Transaction {
  def usesLegacySignature: Boolean = version == 1
  def signature: ByteStr = proofs.toSignature
}
