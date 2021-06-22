package com.ltonetwork.state.reader

import com.ltonetwork.account.{Address, PublicKeyAccount}

case class LeaseDetails(sender: PublicKeyAccount, recipient: Address, height: Int, amount: Long, isActive: Boolean)
