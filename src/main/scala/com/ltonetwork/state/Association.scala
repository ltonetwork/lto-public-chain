package com.ltonetwork.state

import com.ltonetwork.account.Address
import com.ltonetwork.transaction.association.{AssociationTransaction, IssueAssociationTransaction, RevokeAssociationTransaction}
import play.api.libs.json._

case class Association (sender: Address,
                        assocType: Long,
                        recipient: Address,
                        subject: Option[ByteStr],
                        timestamp: Long,
                        expires: Option[Long],
                        data: List[DataEntry[_]]) {
  def assoc: (Long, Address, Option[ByteStr]) = (assocType, recipient, subject)

  def isExpired(time: Long): Boolean = expires.exists(_ <= time)
}

object Association {
  def apply(timestamp: Long, tx: IssueAssociationTransaction): Association =
    Association(
      tx.sender,
      tx.assocType,
      tx.recipient,
      tx.subject,
      timestamp,
      tx.expires,
      tx.data
    )

  implicit val jsonWrites: Writes[Association] = Writes { a => Json.obj(
    "sender" -> a.sender,
    "type" -> a.assocType,
    "recipient" -> a.recipient,
    "subject" -> a.subject,
    "timestamp" -> a.timestamp,
    "expires" -> a.expires,
    "data" -> a.data,
  )}
}

case class Associations(address: Address, outgoing: List[Association], incoming: List[Association]) {
  import com.ltonetwork.state.Associations.buildList

  def update(currentTime: Long, txs: List[(Long, AssociationTransaction)]): Associations =
    Associations(
      address,
      buildList(outgoing.map(a => (a.assoc, a)).toMap)(currentTime, txs.filter(_._2.sender.toAddress == address)),
      buildList(incoming.map(a => (a.assoc, a)).toMap)(currentTime, txs.filter(_._2.recipient == address))
    )
}

object Associations {
  def empty(address: Address): Associations = Associations(address, List.empty[Association], List.empty[Association])

  def build(address: Address, currentTime: Long, txs: List[(Long, AssociationTransaction)]): Associations =
    build(
      address,
      currentTime,
      outgoing = txs.filter(_._2.sender.toAddress == address),
      incoming = txs.filter(_._2.recipient == address)
    )

  def build(address: Address, currentTime: Long, outgoing: List[(Long, AssociationTransaction)], incoming: List[(Long, AssociationTransaction)]): Associations =
    Associations(address, buildList(currentTime, outgoing), buildList(currentTime, incoming))

  private def buildList: (Long, List[(Long, AssociationTransaction)]) => List[Association] =
    buildList(Map.empty[(Long, Address, Option[ByteStr]), Association])

  private def buildList(initial: Map[(Long, Address, Option[ByteStr]), Association])(currentTime: Long, txs: List[(Long, AssociationTransaction)]): List[Association] =
    txs
      .sortBy { case (ts, tx) => (ts, tx.typeId) }
      .foldLeft(initial) {
        case (acc, (ts, tx: AssociationTransaction)) =>
          tx match {
            case itx: IssueAssociationTransaction =>
              if (itx.expires.exists(_ <= currentTime))
                acc - tx.assoc
              else
                acc + (tx.assoc -> Association(ts, itx))
            case _: RevokeAssociationTransaction => acc - tx.assoc
          }
      }
      .values.toList

  implicit val jsonWrites: Writes[Associations] = Json.writes[Associations]
}
