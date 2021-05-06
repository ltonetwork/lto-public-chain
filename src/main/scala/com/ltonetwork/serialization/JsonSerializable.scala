package com.ltonetwork.serialization

import monix.eval.Coeval
import play.api.libs.json.JsObject

trait JsonSerializable {

  val json: Coeval[JsObject]
}
