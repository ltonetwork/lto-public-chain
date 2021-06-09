package com.ltonetwork.api.http.requests.smart

import com.ltonetwork.transaction.smart.SetScriptTransaction
import play.api.libs.json.{Format, JsNumber, JsObject, Json}

case class SetScriptV1Request(version: Byte, sender: String, script: Option[String], fee: Long, timestamp: Option[Long] = None) {}

object SetScriptV1Request {
  implicit val jsonFormat: Format[SetScriptV1Request] = Json.format
  implicit class SetScriptRequestExt(val self: SetScriptV1Request) extends AnyVal {
    def toJsObject: JsObject = Json.toJson(self).as[JsObject] + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))
  }
}
