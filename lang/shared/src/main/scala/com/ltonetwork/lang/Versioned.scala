package com.ltonetwork.lang

trait Versioned {
  type V <: ScriptVersion
  val version: V
}
