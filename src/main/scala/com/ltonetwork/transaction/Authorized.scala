package com.ltonetwork.transaction

import com.ltonetwork.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
