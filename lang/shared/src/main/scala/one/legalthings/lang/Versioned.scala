package one.legalthings.lang

trait Versioned {
  type V <: ScriptVersion
  val version: V
}
