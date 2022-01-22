package com.ltonetwork.lagonaki.unit

import com.ltonetwork.metrics.Instrumented
import com.ltonetwork.state._
import com.ltonetwork.state.diffs.produce
import com.ltonetwork.{NoShrink, TransactionGen, crypto}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import com.ltonetwork.block.Block
import com.ltonetwork.consensus.nxt.NxtLikeConsensusBlockData
import com.ltonetwork.transaction.transfer._

class BlockSpecification extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with TransactionGen with Matchers with NoShrink {

  val time = System.currentTimeMillis() - 5000

  val blockGen = for {
    baseTarget                <- arbitrary[Long]
    reference                 <- byteArrayGen(Block.BlockIdLength).map(r => ByteStr(r))
    generationSignature       <- byteArrayGen(Block.GeneratorSignatureLength)
    sender                    <- accountGen
    recipient                 <- accountGen
    paymentTransaction        <- ltoTransferGeneratorP(time, sender, recipient)
    transferTrancation        <- transferGeneratorP(1 + time, sender, recipient)
    anotherPaymentTransaction <- ltoTransferGeneratorP(2 + time, sender, recipient)
    transactionData = Seq(paymentTransaction, transferTrancation, anotherPaymentTransaction)
  } yield (baseTarget, reference, ByteStr(generationSignature), recipient, transactionData)

  def bigBlockGen(amt: Int): Gen[Block] =
    for {
      baseTarget                              <- arbitrary[Long]
      reference                               <- byteArrayGen(Block.BlockIdLength).map(r => ByteStr(r))
      generationSignature                     <- byteArrayGen(Block.GeneratorSignatureLength)
      sender                                  <- accountGen
      recipient                               <- accountGen
      paymentTransaction: TransferTransaction <- ltoTransferGeneratorP(time, sender, recipient)
    } yield
      Block
        .buildAndSign(
          version = 3,
          timestamp = time,
          reference = reference,
          consensusData = NxtLikeConsensusBlockData(baseTarget, ByteStr(generationSignature)),
          transactionData = Seq.fill(amt)(paymentTransaction),
          signer = recipient,
          featureVotes = Set.empty,
          feeVote = 0
        )
        .explicitGet()

  property(" block with txs bytes/parse roundtrip version 1,2") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen) {
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          val block = Block
            .buildAndSign(version, time, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), transactionData, recipient, Set.empty, 0)
            .explicitGet()
          val parsedBlock = Block.parseBytes(block.bytes()).get
          assert(block.signaturesValid().isRight)
          assert(parsedBlock.signaturesValid().isRight)
          assert(parsedBlock.consensusData.generationSignature == generationSignature)
          assert(parsedBlock.version.toInt == version)
          assert(parsedBlock.signerData.generator.publicKey.sameElements(recipient.publicKey))
      }
    }
  }

  property(" block version 1,2 could not contain feature votes") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen) {
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          Block.buildAndSign(version, time, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), transactionData, recipient, Set(1), 0) should produce(
            "could not contain feature votes")
      }
    }
  }

  property(s" feature flags limit is ${Block.MaxFeaturesInBlock}") {
    val version: Byte     = 3
    val supportedFeatures = (0 to Block.MaxFeaturesInBlock * 2).map(_.toShort).toSet
    val feeVote: Byte     = 0

    forAll(blockGen) {
      case (baseTarget, reference, generationSignature, recipient, transactionData) =>
        Block.buildAndSign(
          version,
          time,
          reference,
          NxtLikeConsensusBlockData(baseTarget, generationSignature),
          transactionData,
          recipient,
          supportedFeatures,
          0
        ) should produce(s"Block could not contain more than ${Block.MaxFeaturesInBlock} feature votes")
    }
  }

  property(" block with txs bytes/parse roundtrip version 3 and 4") {
    val versionGen: Gen[Byte] = Gen.choose(3, 4)
    val faetureSetGen: Gen[Set[Short]] = Gen.choose(0, Block.MaxFeaturesInBlock).flatMap(fc => Gen.listOfN(fc, arbitrary[Short])).map(_.toSet)
    val feeVoteGen: Gen[Byte] = Gen.choose(-1, 1)

    forAll(versionGen, blockGen, faetureSetGen, feeVoteGen) {
      case (version, (baseTarget, reference, generationSignature, recipient, transactionData), featureVotes, feeVote) =>
        val block = Block
          .buildAndSign(
            version,
            time,
            reference,
            NxtLikeConsensusBlockData(baseTarget, generationSignature),
            transactionData,
            recipient,
            featureVotes,
            if (version >= 4) feeVote else 0
          ).explicitGet()
        val parsedBlock = Block.parseBytes(block.bytes()).get
        assert(block.signaturesValid().isRight)
        assert(parsedBlock.signaturesValid().isRight)
        assert(parsedBlock.consensusData.generationSignature == generationSignature)
        assert(parsedBlock.version.toInt == version)
        assert(parsedBlock.signerData.generator.publicKey.sameElements(recipient.publicKey))
        assert(parsedBlock.featureVotes == featureVotes)
        if (version >= 4) assert(parsedBlock.feeVote == feeVote)
    }
  }

  ignore("sign time for 60k txs") {
    forAll(randomTransactionsGen(60000), accountGen, byteArrayGen(Block.BlockIdLength), byteArrayGen(Block.GeneratorSignatureLength)) {
      case ((txs, acc, ref, gs)) =>
        val (block, t0) =
          Instrumented.withTime(Block.buildAndSign(3, 1, ByteStr(ref), NxtLikeConsensusBlockData(1, ByteStr(gs)), txs, acc, Set.empty, 0).explicitGet())
        val (bytes, t1) = Instrumented.withTime(block.bytesWithoutSignature())
        val (hash, t2)  = Instrumented.withTime(crypto.fastHash(bytes))
        val (sig, t3)   = Instrumented.withTime(crypto.sign(acc, hash))
        println((t0, t1, t2, t3))
    }
  }

  ignore("serialize and deserialize big block") {
    forAll(bigBlockGen(100 * 1000)) {
      case block =>
        val parsedBlock = Block.parseBytes(block.bytes()).get
        block.signaturesValid() shouldBe 'right
        parsedBlock.signaturesValid() shouldBe 'right
    }
  }
}
