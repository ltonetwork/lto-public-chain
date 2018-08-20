package com.wavesplatform.consensus.nxt

import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest.{Assertions, Matchers, PropSpec}
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.transaction.transfer._

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  property("TransactionsOrdering.InBlock should sort correctly") {
    val txsDifferentById = (0 to 3)
      .map(
        i =>
          TransferTransactionV1
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
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    1,
                    125L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    2,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    1,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
          100000,
          2,
          124L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          PrivateKeyAccount(Array.fill(32)(0)),
          Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
          100000,
          1,
          124L,
          Array.empty
        )
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort correctly") {
    val txsDifferentById = (0 to 3)
      .map(
        i =>
          TransferTransactionV1
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
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    1,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    1,
                    123L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    2,
                    123L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
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
      TransferTransactionV1
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
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    124L,
                    1,
                    Array())
        .right
        .get,
      TransferTransactionV1
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
      TransferTransactionV1
        .selfSigned(PrivateKeyAccount(Array.fill(32)(0)),
                    Address.fromString("3Mr31XDsqdktAdNQCdSd8ieQuYoJfsnLVFg").explicitGet(),
                    100000,
                    123L,
                    1,
                    Array())
        .right
        .get,
      TransferTransactionV1
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
