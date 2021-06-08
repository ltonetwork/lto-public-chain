package com.ltonetwork.transaction.association

import com.ltonetwork.account.Address
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.Transaction

// Base class for issue and revoke association transactions
abstract class AssociationTransaction extends Transaction  {
  def assocType: Int
  def recipient: Address
  def hash: Option[ByteStr]

  def assoc: (Int, Address, Option[ByteStr]) = (assocType, recipient, hash)
}
