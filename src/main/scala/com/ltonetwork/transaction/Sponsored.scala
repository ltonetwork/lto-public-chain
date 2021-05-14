package com.ltonetwork.transaction

import com.ltonetwork.account.PublicKeyAccount

trait Sponsored {
  val sponsor: PublicKeyAccount
}
