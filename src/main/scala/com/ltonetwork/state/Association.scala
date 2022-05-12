package com.ltonetwork.state

import com.ltonetwork.account.Address

case class Association (assocType: Int,
                        recipient: Address,
                        hash: Option[ByteStr],
                        timestamp: Long,
                        expires: Option[Long],
                        data: List[DataEntry[_]]) {

  def assoc: (Int, Address, Option[ByteStr]) = (assocType, recipient, hash)

  def isExpired(time: Long): Boolean = expires.exists(_ <= time);
}
