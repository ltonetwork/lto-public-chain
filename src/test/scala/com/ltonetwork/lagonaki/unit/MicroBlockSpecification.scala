package com.ltonetwork.lagonaki.unit

import com.ltonetwork.mining.Miner
import com.ltonetwork.state._
import com.ltonetwork.state.diffs.produce
import org.scalamock.scalatest.MockFactory
import org.scalatest.words.ShouldVerb
import org.scalatest.{FunSuite, Matchers}
import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.block.{Block, MicroBlock}
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.transfer._

import scala.util.Random

class MicroBlockSpecification extends FunSuite with Matchers with MockFactory with ShouldVerb {

  val prevResBlockSig  = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  val totalResBlockSig = ByteStr(Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte))
  val reference        = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
  val sender           = PrivateKeyAccount(reference.dropRight(2))
  val gen              = PrivateKeyAccount(reference)

  test("MicroBlock with txs bytes/parse roundtrip") {

    val ts                         = System.currentTimeMillis() - 5000
    val tr: TransferTransaction  = TransferTransaction.selfSigned(sender, gen, 5, ts + 1, 2, Array()).explicitGet()
    val assetId                    = Some(ByteStr(Array.fill(AssetIdLength)(Random.nextInt(100).toByte)))
    val tr2: TransferTransaction = TransferTransaction.selfSigned(sender, gen, 5, ts + 2, 2, Array()).explicitGet()

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

    val transaction  = TransferTransaction.selfSigned(sender, gen, 5, System.currentTimeMillis(), 1000, Array()).explicitGet()
    val transactions = Seq.fill(Miner.MaxTransactionsPerMicroblock + 1)(transaction)

    val eitherBlockOrError = MicroBlock.buildAndSign(sender, transactions, prevResBlockSig, totalResBlockSig)

    eitherBlockOrError should produce("too many txs in MicroBlock")
  }
}
