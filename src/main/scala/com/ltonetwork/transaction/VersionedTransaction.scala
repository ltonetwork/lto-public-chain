package com.ltonetwork.transaction

trait VersionedTransaction {
  def version: Byte
}
