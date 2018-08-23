package one.legalthings.transaction

trait VersionedTransaction {
  def version: Byte
}
