package com.ltonetwork.block

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.mining.Miner
import com.ltonetwork.state._
import com.ltonetwork.state.diffs.produce
import com.ltonetwork.transaction.transfer._
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.verbs.ShouldVerb

import scala.util.Random

class MicroBlockSpecification extends AnyFunSuite with Matchers with MockFactory with ShouldVerb {

  val prevResBlockSig  = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  val totalResBlockSig = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  val reference        = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
  val sender           = PrivateKeyAccount(reference.dropRight(2))
  val gen              = PrivateKeyAccount(reference)

  test("MicroBlock with txs bytes/parse roundtrip") {

    val ts                       = System.currentTimeMillis() - 5000
    val tr: TransferTransaction  = TransferTransaction.signed(1, ts + 1, sender, 2, gen, 5, Array()).explicitGet()
    val tr2: TransferTransaction = TransferTransaction.signed(1, ts + 2, sender, 2, gen, 5, Array()).explicitGet()

    val transactions = Seq(tr, tr2)

    val microBlock  = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig).explicitGet()
    val parsedBlock = MicroBlock.parseBytes(microBlock.bytes()).get

    assert(microBlock.signaturesValid().isRight)
    assert(parsedBlock.signaturesValid().isRight)

    assert(microBlock.signature == parsedBlock.signature)
    assert(microBlock.sender == parsedBlock.sender)
    assert(microBlock.totalResBlockSig == parsedBlock.totalResBlockSig)
    assert(microBlock.prevResBlockSig == parsedBlock.prevResBlockSig)
    assert(microBlock.transactionData == parsedBlock.transactionData)
    assert(microBlock == parsedBlock)
  }

  test("MicroBlock cannot be created with zero transactions") {

    val transactions       = Seq.empty[TransferTransaction]
    val eitherBlockOrError = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig)

    eitherBlockOrError should produce("cannot create empty MicroBlock")
  }

  test("MicroBlock cannot contain more than Miner.MaxTransactionsPerMicroblock") {

    val transaction  = TransferTransaction.signed(1, System.currentTimeMillis(), sender, 5, gen, 1000, Array()).explicitGet()
    val transactions = Seq.fill(Miner.MaxTransactionsPerMicroblock + 1)(transaction)

    val eitherBlockOrError = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig)

    eitherBlockOrError should produce("too many txs in MicroBlock")
  }
}
