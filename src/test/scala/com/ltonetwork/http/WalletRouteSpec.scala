package com.ltonetwork.http

import com.ltonetwork.{TestWallet, crypto}
import com.ltonetwork.http.ApiMarshallers._
import play.api.libs.json.JsObject
import com.ltonetwork.api.{ApiKeyNotValid, WalletApiRoute}
import com.ltonetwork.utils.Base58
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class WalletRouteSpec extends RouteSpec("/wallet") with RestAPISettingsHelper with TestWallet with ScalaCheckDrivenPropertyChecks {
  private val route = WalletApiRoute(restAPISettings, testWallet).route

  private val allAccounts  = testWallet.privateKeyAccounts
  private val allAddresses = allAccounts.map(_.address)

  private val generatedMessages = for {
    account <- Gen.oneOf(allAccounts).label("account")
    length  <- Gen.chooseNum(10, 1000)
    message <- Gen.listOfN(length, Gen.alphaNumChar).map(_.mkString).label("message")
  } yield (account, message)

  private def testSign(path: String, encode: Boolean): Unit =
    forAll(generatedMessages) {
      case (account, message) =>
        val uri = routePath(s"/$path/${account.address}")
        Post(uri, message) ~> route should produce(ApiKeyNotValid)
        Post(uri, message) ~> ApiKey(apiKey) ~> route ~> check {
          val resp      = responseAs[JsObject]
          val signature = Base58.decode((resp \ "signature").as[String]).get

          (resp \ "message").as[String] shouldEqual (if (encode) Base58.encode(message.getBytes) else message)
          (resp \ "publicKey").as[String] shouldEqual Base58.encode(account.publicKey)

          crypto.verify(signature, message.getBytes, account.publicKey) shouldBe true
        }
    }

  routePath("/addresses/seq/{from}/{to}") in {
    val r1 = Get(routePath("/addresses/seq/1/4")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 3
      allAddresses should contain allElementsOf response
      response
    }

    val r2 = Get(routePath("/addresses/seq/5/9")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 4
      allAddresses should contain allElementsOf response
      response
    }

    r1 shouldNot contain allElementsOf r2
  }

  routePath("/sign/{address}") in testSign("sign", true)
  routePath("/signText/{address}") in testSign("signText", false)

  // Tests that modify the wallet addresses must be done after the other tests, or they'll fail

  routePath("/addresses") in {
    Post(routePath("/addresses")) ~> route should produce(ApiKeyNotValid)
    Post(routePath("/addresses")) ~> ApiKey(apiKey) ~> route ~> check {
      allAddresses should not contain (responseAs[JsObject] \ "address").as[String]
    }
  }

  routePath("/addresses/{address}") in {
    Delete(routePath(s"/addresses/${allAddresses.head}")) ~> ApiKey(apiKey) ~> route ~> check {
      (responseAs[JsObject] \ "deleted").as[Boolean] shouldBe true
    }
  }
}
