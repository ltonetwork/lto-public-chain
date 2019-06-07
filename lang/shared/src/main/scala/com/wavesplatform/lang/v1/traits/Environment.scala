package com.wavesplatform.lang.v1.traits

trait Environment {
  def height: Int
  def networkByte: Byte
  def transaction: Tx
}
