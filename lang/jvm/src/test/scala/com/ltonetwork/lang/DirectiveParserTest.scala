package com.ltonetwork.lang

import com.ltonetwork.lang.directives.DirectiveKey.LANGUAGE_VERSION
import com.ltonetwork.lang.directives.{Directive, DirectiveParser}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers


class DirectiveParserTest extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  def parse(s: String): List[Directive] = DirectiveParser(s)

  property("parse LANGUAGE_VERSION directive") {
    parse("{-# LANGUAGE_VERSION 10 #-}") shouldBe List(Directive(LANGUAGE_VERSION, "10"))
    parse("""
        |
        |{-# LANGUAGE_VERSION 10 #-}
        |
      """.stripMargin) shouldBe List(Directive(LANGUAGE_VERSION, "10"))
  }
}
