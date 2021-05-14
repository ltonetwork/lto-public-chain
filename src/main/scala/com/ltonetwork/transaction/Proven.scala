package com.ltonetwork.transaction

import play.api.libs.json.{JsObject, Json}

trait Proven extends Authorized {
  def proofs: Proofs
}
