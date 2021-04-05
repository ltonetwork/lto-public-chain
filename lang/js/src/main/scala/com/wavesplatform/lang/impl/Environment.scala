package com.wavesplatform.lang.impl

import com.wavesplatform.lang.v1.traits.Tx

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.{js => platform}

@platform.native
@JSGlobalScope
object Environment extends scalajs.js.Object {
  def height: Int       = platform.native
  def networkByte: Byte = platform.native
  def transaction: Tx                                     = platform.native
}
