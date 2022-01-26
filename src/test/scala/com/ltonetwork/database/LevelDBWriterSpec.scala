package com.ltonetwork.database

import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.block.{Block, TestBlock}
import com.ltonetwork.lang.v1.compiler.Terms
import com.ltonetwork.settings.{LtoSettings, TestFunctionalitySettings, loadConfig}
import com.ltonetwork.state.diffs.ENOUGH_AMT
import com.ltonetwork.state.{BlockchainUpdaterImpl, EitherExt2}
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.utils.{Time, TimeImpl}
import com.ltonetwork.{RequestGen, WithDB}
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AnyFreeSpec

class LevelDBWriterSpec extends AnyFreeSpec with Matchers with WithDB with RequestGen {
  "Slice" - {
    "drops tail" in {
      LevelDBWriter.slice(Seq(10, 7, 4), 7, 10) shouldEqual Seq(10, 7)
    }
    "drops head" in {
      LevelDBWriter.slice(Seq(10, 7, 4), 4, 8) shouldEqual Seq(7, 4)
    }
    "includes Genesis" in {
      LevelDBWriter.slice(Seq(10, 7), 5, 11) shouldEqual Seq(10, 7, 1)
    }
  }
  "Merge" - {
    "correctly joins height ranges" in {
      LevelDBWriter.merge(Seq(15, 12, 3), Seq(12, 5)) shouldEqual Seq((15, 12), (12, 12), (3, 5))
      LevelDBWriter.merge(Seq(12, 5), Seq(15, 12, 3)) shouldEqual Seq((12, 15), (12, 12), (5, 3))
      LevelDBWriter.merge(Seq(8, 4), Seq(8, 4)) shouldEqual Seq((8, 8), (4, 4))
    }
  }
  "hasScript" - {
    "returns false if a script was not set" in {
      val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
      writer.hasScript(accountGen.sample.get.toAddress) shouldBe false
    }

    "returns false if a script was set and then unset" in {

      resetTest { (_, account) =>
        val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
        writer.hasScript(account) shouldBe false
      }
    }

    "returns true" - {
      "if there is a script in db" in {

        test { (_, account) =>
          val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
          writer.hasScript(account) shouldBe true
        }
      }

      "if there is a script in cache" in {

        test { (defaultWriter, account) =>
          defaultWriter.hasScript(account) shouldBe true
        }
      }
    }

    def gen(ts: Long): Gen[(PrivateKeyAccount, Seq[Block])] = baseGen(ts).map {
      case (master, blocks) =>
        val nextBlock = TestBlock.create(ts + 1, blocks.last.uniqueId, Seq())
        (master, blocks :+ nextBlock)
    }

    def resetGen(ts: Long): Gen[(PrivateKeyAccount, Seq[Block])] = baseGen(ts).map {
      case (master, blocks) =>
        val unsetScriptTx = SetScriptTransaction
          .signed(1, ts + 1, master, 100 * 1000 * 1000L, None)
          .explicitGet()

        val block1 = TestBlock.create(ts + 1, blocks.last.uniqueId, Seq(unsetScriptTx))
        val block2 = TestBlock.create(ts + 2, block1.uniqueId, Seq())
        (master, blocks ++ List(block1, block2))
    }

    def baseGen(ts: Long): Gen[(PrivateKeyAccount, Seq[Block])] = accountGen.map { master =>
      val genesisTx = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      val setScriptTx = SetScriptTransaction
        .signed(1, ts, master, 100 * 1000 * 1000L, Some(ScriptV1(Terms.TRUE).explicitGet()))
        .explicitGet()

      val block = TestBlock.create(ts, Seq(genesisTx, setScriptTx))
      (master, Seq(block))
    }

    def test(f: (LevelDBWriter, PrivateKeyAccount) => Unit): Unit = baseTest(t => gen(t.correctedTime()))(f)

    def resetTest(f: (LevelDBWriter, PrivateKeyAccount) => Unit): Unit = baseTest(t => resetGen(t.correctedTime()))(f)

  }

  def baseTest(gen: Time => Gen[(PrivateKeyAccount, Seq[Block])])(f: (LevelDBWriter, PrivateKeyAccount) => Unit): Unit = {
    val time          = new TimeImpl
    val defaultWriter = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
    val settings0     = LtoSettings.fromConfig(loadConfig(ConfigFactory.load()))
    val settings      = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, settings, time)
    try {
      val (account, blocks) = gen(time).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      bcu.shutdown()
      f(defaultWriter, account)
    } finally {
      time.close()
      bcu.shutdown()
      db.close()
    }
  }

  "addressTransactions" - {
    "return txs in correct ordering" in {
      val preconditions = (ts: Long) => {
        for {
          master    <- accountGen
          recipient <- accountGen
          genesisBlock = TestBlock
            .create(ts, Seq(GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()))
          block1 = TestBlock
            .create(
              ts + 3,
              genesisBlock.uniqueId,
              Seq(
                createTransfer(master, recipient.toAddress, ts + 1),
                createTransfer(master, recipient.toAddress, ts + 2)
              )
            )
          emptyBlock = TestBlock.create(ts + 5, block1.uniqueId, Seq())
        } yield (master, List(genesisBlock, block1, emptyBlock))
      }

      baseTest(time => preconditions(time.correctedTime())) { (writer, account) =>
        val txs = writer
          .addressTransactions(account.toAddress, Set(TransferTransaction.typeId), 3, 0)

        val ordering = Ordering
          .by[(Int, Transaction), (Int, Long)]({ case (h, t) => (-h, -t.timestamp) })

        txs.length shouldBe 2

        txs.sorted(ordering) shouldBe txs
      }
    }

    def createTransfer(master: PrivateKeyAccount, recipient: Address, ts: Long): TransferTransaction = {
      TransferTransaction
        .signed(1, ts, master, 100 * 1000 * 1000L, recipient, ENOUGH_AMT / 5, Array.emptyByteArray)
        .explicitGet()
    }
  }
}
