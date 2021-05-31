package com.ltonetwork.consensus.nxt

import com.ltonetwork.state.{ByteStr, EitherExt2}
import org.scalatest.{Assertions, Matchers, PropSpec}
import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.consensus.TransactionsOrdering
import com.ltonetwork.transaction.transfer._

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  ignore("TransactionsOrdering.InUTXPool should sort correctly") {
    val txsDifferentById = (0 to 3)
      .map(
        i =>
          TransferTransaction
            .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                        Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                        100000,
                        5,
                        125L,
                        Array(i.toByte))
            .right
            .get)
      .sortBy(t => t.id().base58)

    val correctSeq = txsDifferentById ++ Seq(
      TransferTransaction
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    1,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransaction
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    1,
                    123L,
                    Array.empty)
        .right
        .get,
      TransferTransaction
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    2,
                    123L,
                    Array.empty)
        .right
        .get,
      TransferTransaction
        .selfSigned(
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
          100000,
          1,
          124L,
          Array.empty
        )
        .right
        .get,
      TransferTransaction
        .selfSigned(
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
          100000,
          2,
          124L,
          Array.empty
        )
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InBlock should sort txs by decreasing block timestamp") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    124L,
                    1,
                    Array())
        .right
        .get,
      TransferTransaction
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    123L,
                    1,
                    Array())
        .right
        .get
    )

    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock) shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort txs by ascending block timestamp") {
    val correctSeq = Seq(
      TransferTransaction
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    123L,
                    1,
                    Array())
        .right
        .get,
      TransferTransaction
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    124L,
                    1,
                    Array())
        .right
        .get
    )
    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool) shouldBe correctSeq
  }
}
