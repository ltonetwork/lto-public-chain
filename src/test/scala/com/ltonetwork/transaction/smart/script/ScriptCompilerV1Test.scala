package com.ltonetwork.transaction.smart.script

import cats.implicits._
import com.ltonetwork.lang.v1.FunctionHeader
import com.ltonetwork.lang.v1.compiler.Terms._
import com.ltonetwork.state.EitherExt2
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.lang.v1.evaluator.FunctionIds._
import com.ltonetwork.lang.v1.evaluator.ctx.impl.PureContext

class ScriptCompilerV1Test extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import ScriptCompilerV1Test._

  property("compile script with specified version") {
    val script = scriptWithVersion("1".some)
    ScriptCompiler(script) shouldBe Right((compiledScript, 13))
  }

  property("use version 1 if not specified") {
    val script = scriptWithVersion(none)
    ScriptCompiler(script) shouldBe Right((compiledScript, 13))
  }

  property("fails on unsupported version") {
    val script = scriptWithVersion("2".some)
    ScriptCompiler(script) shouldBe Left("Unsupported language version")
  }

  property("fails on incorrect version value") {
    val script = scriptWithVersion("oOooOps".some)
    ScriptCompiler(script) shouldBe Left("Can't parse language version")
  }
}

object ScriptCompilerV1Test {
  def script: String = scriptWithVersion(None)

  def scriptWithVersion(versionStr: Option[String]): String = {
    val directive =
      versionStr
        .map(v => s"{-# LANGUAGE_VERSION $v #-}")
        .getOrElse("")

    s"""
       | $directive
       |
       | let x = 10
       | 20 == x + x
       |
      """.stripMargin
  }

  val compiledExpr: BLOCK = BLOCK(
    LET("x", CONST_LONG(10)),
    FUNCTION_CALL(
      PureContext.eq.header,
      List(
        CONST_LONG(20),
        FUNCTION_CALL(
          FunctionHeader.Native(SUM_LONG),
          List(REF("x"), REF("x"))
        )
      )
    )
  )

  val compiledScript: Script = ScriptV1(compiledExpr).explicitGet()
}
