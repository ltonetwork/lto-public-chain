package one.legalthings.transaction

import one.legalthings.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
