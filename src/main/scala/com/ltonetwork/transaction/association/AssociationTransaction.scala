package com.ltonetwork.transaction.association

import com.ltonetwork.account.AddressOrAlias
import com.ltonetwork.state.ByteStr
import com.ltonetwork.transaction.Transaction

// Base class for issue and revoke association transactions
abstract class AssociationTransaction extends Transaction  {
  def assocType: Int
  def recipient: AddressOrAlias
  def hash: Option[ByteStr]

  def assoc: (Int, AddressOrAlias, Option[ByteStr]) = (assocType, recipient, hash)
}
