package com.ltonetwork.http

import com.ltonetwork.crypto
import com.ltonetwork.http.ApiMarshallers._
import com.ltonetwork.lang.v1.compiler.Terms._
import com.ltonetwork.lang.v1.evaluator.FunctionIds._
import com.ltonetwork.lang.v1.evaluator.ctx.impl.PureContext
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.state.diffs.CommonValidation
import com.ltonetwork.utils.{Base58, Time}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.{JsObject, JsValue}
import com.ltonetwork.api.http.UtilsApiRoute
import com.ltonetwork.transaction.smart.script.Script
import com.ltonetwork.transaction.smart.script.v1.ScriptV1

class UtilsRouteSpec extends RouteSpec("/utils") with RestAPISettingsHelper with ScalaCheckDrivenPropertyChecks {
  private val route = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()
      def getTimestamp(): Long  = System.currentTimeMillis()
    },
    restAPISettings
  ).route

  val script = FUNCTION_CALL(
    function = PureContext.eq.header,
    args = List(CONST_LONG(1), CONST_LONG(2))
  )

  routePath("/script/compile") in {
    Post(routePath("/script/compile"), "1 == 2") ~> route ~> check {
      val json           = responseAs[JsValue]
      val expectedScript = ScriptV1(script).explicitGet()

      Script.fromBase64String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }
  }

  routePath("/script/estimate") in {
    val base64 = ScriptV1(script).explicitGet().bytes().base64

    Post(routePath("/script/estimate"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe base64
      (json \ "scriptText").as[String] shouldBe s"FUNCTION_CALL(Native($EQ),List(CONST_LONG(1), CONST_LONG(2)))"
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }
  }

  for ((hash, f) <- Seq[(String, String => Array[Byte])](
         "secure" -> crypto.secureHash,
         "fast"   -> crypto.fastHash
       )) {
    val uri = routePath(s"/hash/$hash")
    uri in {
      forAll(Gen.alphaNumStr) { s =>
        Post(uri, s) ~> route ~> check {
          val r = responseAs[JsObject]
          (r \ "message").as[String] shouldEqual s
          (r \ "hash").as[String] shouldEqual Base58.encode(f(s))
        }
      }
    }
  }
}
