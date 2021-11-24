package com.ltonetwork.http

import com.ltonetwork.http.ApiMarshallers._
import com.ltonetwork.lang.v1.compiler.Terms._
import com.ltonetwork.state.diffs.CommonValidation
import com.ltonetwork.state.{Blockchain, EitherExt2}
import com.ltonetwork.utils.Base58
import com.ltonetwork.utx.UtxPool
import com.ltonetwork.{NoShrink, TestTime, TestWallet, crypto}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json._
import com.ltonetwork.account.Address
import com.ltonetwork.api.http.{AddressApiRoute, ApiKeyNotValid}
import com.ltonetwork.settings.TestFunctionalitySettings
import com.ltonetwork.transaction.smart.script.v1.ScriptV1

class AddressRouteSpec
    extends RouteSpec("/addresses")
    with PathMockFactory
    with ScalaCheckDrivenPropertyChecks
    with TableDrivenPropertyChecks
    with RestAPISettingsHelper
    with TestWallet
    with NoShrink {

  private val allAccounts  = testWallet.privateKeyAccounts
  private val allAddresses = allAccounts.map(_.address)
  private val blockchain   = stub[Blockchain]

  private val route = AddressApiRoute(
    restAPISettings,
    testWallet,
    blockchain,
    mock[UtxPool],
    mock[ChannelGroup],
    new TestTime,
    TestFunctionalitySettings.Stub,
    _ => Seq.empty
  ).route

  private val generatedMessages = for {
    account <- Gen.oneOf(allAccounts).label("account")
    length  <- Gen.chooseNum(10, 1000)
    message <- Gen.listOfN(length, Gen.alphaNumChar).map(_.mkString).label("message")
  } yield (account, message)

  routePath("/validate/{address}") in {
    val t = Table(("address", "valid"), allAddresses.map(_ -> true) :+ "invalid-address" -> false: _*)

    forAll(t) { (a, v) =>
      Get(routePath(s"/validate/$a")) ~> route ~> check {
        val r = responseAs[AddressApiRoute.Validity]
        r.address shouldEqual a
        r.valid shouldBe v
      }
    }
  }

  private def testVerify(path: String, encode: Boolean): Unit = {

    forAll(generatedMessages) {
      case (account, message) =>
        val uri          = routePath(s"/$path/${account.address}")
        val messageBytes = message.getBytes()
        val signature    = crypto.sign(account, messageBytes)
        val validBody = Json.obj(
          "message"   -> JsString(if (encode) Base58.encode(messageBytes) else message),
          "publickey" -> JsString(Base58.encode(account.publicKey)),
          "signature" -> JsString(Base58.encode(signature))
        )

        val emptySignature =
          Json.obj("message" -> JsString(""), "publickey" -> JsString(Base58.encode(account.publicKey)), "signature" -> JsString(""))

        Post(uri, validBody) ~> route should produce(ApiKeyNotValid)
        Post(uri, emptySignature) ~> api_key(apiKey) ~> route ~> check {
          (responseAs[JsObject] \ "valid").as[Boolean] shouldBe false
        }
        Post(uri, validBody) ~> api_key(apiKey) ~> route ~> check {
          (responseAs[JsObject] \ "valid").as[Boolean] shouldBe true
        }
    }
  }

  routePath("/verifyText/{address}") in testVerify("verifyText", false)
  routePath("/verify/{address}") in testVerify("verify", true)

  routePath(s"/scriptInfo/${allAddresses(1)}") in {
    (blockchain.accountScript _).when(allAccounts(1).toAddress).onCall((_: Address) => Some(ScriptV1(TRUE).explicitGet()))
    Get(routePath(s"/scriptInfo/${allAddresses(1)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(1)
      (response \ "script").as[String] shouldBe "base64:AQaEZXhA"
      (response \ "scriptText").as[String] shouldBe "TRUE"
      (response \ "complexity").as[Long] shouldBe 1
      (response \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }

    (blockchain.accountScript _).when(allAccounts(2).toAddress).onCall((_: Address) => None)
    Get(routePath(s"/scriptInfo/${allAddresses(2)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(2)
      (response \ "script").asOpt[String] shouldBe None
      (response \ "scriptText").asOpt[String] shouldBe None
      (response \ "complexity").as[Long] shouldBe 0
      (response \ "extraFee").as[Long] shouldBe 0
    }
  }
}
