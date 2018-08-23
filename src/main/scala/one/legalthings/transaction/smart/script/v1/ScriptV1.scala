package one.legalthings.transaction.smart.script.v1

import one.legalthings.crypto
import one.legalthings.lang.ScriptVersion.Versions.V1
import one.legalthings.lang.v1.compiler.Terms._
import one.legalthings.lang.v1.evaluator.FunctionIds._
import one.legalthings.lang.v1.{FunctionHeader, ScriptEstimator, Serde}
import one.legalthings.state.ByteStr
import one.legalthings.utils.functionCosts
import monix.eval.Coeval
import one.legalthings.transaction.smart.script.Script

object ScriptV1 {
  private val checksumLength = 4
  private val maxComplexity  = 20 * functionCosts(FunctionHeader.Native(SIGVERIFY))()
  private val maxSizeInBytes = 8 * 1024

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= maxSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $maxSizeInBytes bytes")

  def apply(x: EXPR, checkSize: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(functionCosts, x)
      _                <- Either.cond(scriptComplexity <= maxComplexity, (), s"Script is too complex: $scriptComplexity > $maxComplexity")
      s = new ScriptV1(x)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  private class ScriptV1(override val expr: EXPR) extends Script {
    override type V = V1.type
    override val version: V   = V1
    override val text: String = expr.toString
    override val bytes: Coeval[ByteStr] =
      Coeval.evalOnce {
        val s = Array(version.value.toByte) ++ Serde.serialize(expr)
        ByteStr(s ++ crypto.secureHash(s).take(ScriptV1.checksumLength))
      }
  }
}
