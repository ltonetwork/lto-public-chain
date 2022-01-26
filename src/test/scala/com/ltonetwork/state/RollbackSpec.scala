package com.ltonetwork.state

import com.ltonetwork.account.{Address, PrivateKeyAccount}
import com.ltonetwork.block.TestBlock
import com.ltonetwork.crypto.signatureLength
import com.ltonetwork.db.WithState
import com.ltonetwork.features._
import com.ltonetwork.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.ltonetwork.state.reader.LeaseDetails
import com.ltonetwork.transaction.genesis.GenesisTransaction
import com.ltonetwork.transaction.lease.{CancelLeaseTransaction, LeaseTransaction}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.transfer._
import com.ltonetwork.transaction._
import com.ltonetwork.transaction.anchor.AnchorTransaction
import com.ltonetwork.transaction.data.DataTransaction
import com.ltonetwork.transaction.sponsorship.{CancelSponsorshipTransaction, SponsorshipTransaction}
import com.ltonetwork.{NoShrink, TestTime, TransactionGen, history}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AnyFreeSpec

class RollbackSpec extends AnyFreeSpec with Matchers with WithState with TransactionGen with ScalaCheckDrivenPropertyChecks with NoShrink {
  private val time   = new TestTime
  private def nextTs = time.getTimestamp()

  private def genesisBlock(genesisTs: Long, address: Address, initialBalance: Long) = TestBlock.create(
    genesisTs,
    ByteStr(Array.fill[Byte](signatureLength)(0)),
    Seq(GenesisTransaction.create(address, initialBalance, genesisTs).explicitGet())
  )
  private def genesisBlock(genesisTs: Long, balances: Seq[(Address, Long)]) =
    TestBlock.create(genesisTs, ByteStr(Array.fill[Byte](signatureLength)(0)), balances.map {
      case (addr, amt) => GenesisTransaction.create(addr, amt, genesisTs).explicitGet()
    })

  private val enoughFee = 100000000L
  private def transfer(sender: PrivateKeyAccount, recipient: Address, amount: Long) =
    TransferTransaction.signed(1, nextTs, sender, enoughFee, recipient, amount, Array.empty[Byte]).explicitGet()

  private def randomOp(sender: PrivateKeyAccount, recipient: Address, amount: Long, op: Int) = {
    import com.ltonetwork.transaction.transfer.MassTransferTransaction.ParsedTransfer
    op match {
      case 1 =>
        val lease = LeaseTransaction.signed(1, nextTs, sender, enoughFee, recipient, amount).explicitGet()
        List(lease, CancelLeaseTransaction.signed(1, nextTs, sender, enoughFee, lease.id()).explicitGet())
      case 2 =>
        List(
          MassTransferTransaction
            .signed(1, nextTs, sender, enoughFee, List(ParsedTransfer(recipient, amount), ParsedTransfer(recipient, amount)), Array.empty[Byte])
            .explicitGet())
      case _ => List(TransferTransaction.signed(1, nextTs, sender, enoughFee, recipient, amount, Array.empty[Byte]).explicitGet())
    }
  }

  "Rollback resets" - {
    "Rollback save dropped blocks order" in forAll(accountGen, positiveLongGen, Gen.choose(1, 10)) {
      case (sender, initialBalance, blocksCount) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisSignature = d.lastBlockId
          def newBlocks(i: Int): List[ByteStr] = {
            if (i == blocksCount) {
              Nil
            } else {
              val block = TestBlock.create(nextTs + i, d.lastBlockId, Seq())
              d.appendBlock(block)
              block.uniqueId :: newBlocks(i + 1)
            }
          }
          val blocks        = newBlocks(0)
          val droppedBlocks = d.removeAfter(genesisSignature)
          droppedBlocks(0).reference shouldBe genesisSignature
          droppedBlocks.map(_.uniqueId).toList shouldBe blocks
          droppedBlocks foreach d.appendBlock
        }
    }

    "forget rollbacked transaction for quering" in forAll(accountGen, accountGen, Gen.nonEmptyListOf(Gen.choose(1, 10))) {
      case (sender, recipient, txCount) =>
        val settings = createSettings()
        val ltoSettings =
          history.DefaultLtoSettings.copy(blockchainSettings = history.DefaultLtoSettings.blockchainSettings.copy(functionalitySettings = settings))
        withDomain(ltoSettings) { d =>
          d.appendBlock(genesisBlock(nextTs, sender, com.ltonetwork.state.diffs.ENOUGH_AMT))

          val genesisSignature = d.lastBlockId

          val transferAmount = 100
          val transfers      = txCount.map(tc => Seq.fill(tc)(randomOp(sender, recipient, transferAmount, tc % 3)).flatten)

          for (transfer <- transfers) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                transfer
              ))
          }

          val stransactions1 = d.addressTransactions(sender).sortBy(_._2.timestamp)
          val rtransactions1 = d.addressTransactions(recipient).sortBy(_._2.timestamp)

          d.removeAfter(genesisSignature)

          for (transfer <- transfers) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                transfer
              ))
          }

          val stransactions2 = d.addressTransactions(sender).sortBy(_._2.timestamp)
          val rtransactions2 = d.addressTransactions(recipient).sortBy(_._2.timestamp)

          stransactions1 shouldBe stransactions2
          rtransactions1 shouldBe rtransactions2
        }
    }

    "lto balances" in forAll(accountGen, positiveLongGen, accountGen, Gen.nonEmptyListOf(Gen.choose(1, 10))) {
      case (sender, initialBalance, recipient, txCount) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))

          val genesisSignature = d.lastBlockId

          d.portfolio(sender.toAddress).balance shouldBe initialBalance
          d.portfolio(recipient.toAddress).balance shouldBe 0

          val totalTxCount   = txCount.sum
          val transferAmount = initialBalance / (totalTxCount * 2)

          for (tc <- txCount) {
            d.appendBlock(
              TestBlock.create(
                nextTs,
                d.lastBlockId,
                Seq.fill(tc)(transfer(sender, recipient, transferAmount))
              ))
          }

          d.portfolio(recipient).balance shouldBe (transferAmount * totalTxCount)

          d.removeAfter(genesisSignature)

          d.portfolio(sender).balance shouldBe initialBalance
          d.portfolio(recipient).balance shouldBe 0
        }
    }

    "lease balances and states" in forAll(accountGen, positiveLongGen suchThat (_ > enoughFee * 2), accountGen) {
      case (sender, initialBalance, recipient) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          val leaseAmount = initialBalance - enoughFee * 2
          val lt          = LeaseTransaction.signed(1, nextTs, sender, enoughFee, recipient, leaseAmount).explicitGet()
          d.appendBlock(TestBlock.create(nextTs, genesisBlockId, Seq(lt)))
          val blockWithLeaseId = d.lastBlockId
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender, recipient, 2, leaseAmount, true))
          d.portfolio(sender).lease.out shouldEqual leaseAmount
          d.portfolio(recipient).lease.in shouldEqual leaseAmount

          d.appendBlock(
            TestBlock.create(
              nextTs,
              blockWithLeaseId,
              Seq(CancelLeaseTransaction.signed(1, nextTs, sender, enoughFee, lt.id()).explicitGet())
            ))
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender, recipient, 2, leaseAmount, false))
          d.portfolio(sender).lease.out shouldEqual 0
          d.portfolio(recipient).lease.in shouldEqual 0

          d.removeAfter(blockWithLeaseId)
          d.blockchainUpdater.leaseDetails(lt.id()) should contain(LeaseDetails(sender, recipient, 2, leaseAmount, true))
          d.portfolio(sender).lease.out shouldEqual leaseAmount
          d.portfolio(recipient).lease.in shouldEqual leaseAmount

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.leaseDetails(lt.id()) shouldBe 'empty
          d.portfolio(sender).lease.out shouldEqual 0
          d.portfolio(recipient).lease.in shouldEqual 0
        }
    }

    "data transaction" in (forAll(accountGen, positiveLongGen, dataEntryGen(1000)) {
      case (sender, initialBalance, dataEntry) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(DataTransaction.signed(1, nextTs, sender, enoughFee, List(dataEntry)).explicitGet())
            ))

          d.blockchainUpdater.accountData(sender, dataEntry.key) should contain(dataEntry)

          d.removeAfter(genesisBlockId)
          d.blockchainUpdater.accountData(sender, dataEntry.key) shouldBe 'empty
        }
    })

    "anchor transaction" in (forAll(accountGen, positiveLongGen, Gen.choose(0, AnchorTransaction.MaxEntryCount).flatMap(Gen.listOfN(_, bytes64gen))) {
      case (sender, initialBalance, anchors) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          val genesisBlockId = d.lastBlockId

          val tx = AnchorTransaction.signed(1, nextTs, sender, enoughFee, anchors.map(ByteStr(_))).explicitGet()
          d.appendBlock(
            TestBlock.create(
              nextTs,
              genesisBlockId,
              Seq(tx)
            ))

          d.blockchainUpdater.containsTransaction(tx.id()) shouldBe true

          d.removeAfter(genesisBlockId)

          d.blockchainUpdater.containsTransaction(tx.id()) shouldBe false
        }
    })

    "sponsorship transaction" in forAll(accountGen, accountGen) {

      case (sponsor, sender) =>
        import com.ltonetwork.state.diffs.ENOUGH_AMT
        val settings = createSettings(BlockchainFeatures.SponsorshipTransaction -> 0, BlockchainFeatures.SmartAccounts -> 0)
        val ltoSettings =
          history.DefaultLtoSettings.copy(blockchainSettings = history.DefaultLtoSettings.blockchainSettings.copy(functionalitySettings = settings))
        val tx  = SponsorshipTransaction.signed(1, nextTs, sponsor, 5 * 100000000L, sender).explicitGet()
        val tx2 = CancelSponsorshipTransaction.signed(1, nextTs, sponsor, 5 * 100000000L, sender).explicitGet()

        withDomain(ltoSettings) { d =>
          d.appendBlock(genesisBlock(nextTs, Seq((sponsor, ENOUGH_AMT), (sender, ENOUGH_AMT))))

          def appendTx(tx: Transaction) = {
            val block = TestBlock.create(
              nextTs,
              d.lastBlockId,
              Seq(tx)
            )
            d.appendBlock(block)
            block.uniqueId
          }

          withClue("rollback sponsorship") {
            val prev = d.lastBlockId
            appendTx(tx)
            d.blockchainUpdater.sponsorOf(sender) shouldBe List(sponsor.toAddress)
            d.removeAfter(prev)
            d.blockchainUpdater.sponsorOf(sender) shouldBe List.empty
          }
          withClue("rollback sponsorship cancel") {
            appendTx(tx)
            d.blockchainUpdater.sponsorOf(sender) shouldBe List(sponsor.toAddress)
            val prev = d.lastBlockId
            appendTx(tx2)
            d.blockchainUpdater.sponsorOf(sender) shouldBe List.empty
            d.removeAfter(prev)
            d.blockchainUpdater.sponsorOf(sender) shouldBe List(sponsor.toAddress)
          }
        }
    }

    "address script" in pendingUntilFixed(forAll(accountGen, positiveLongGen, scriptGen) {
      case (sender, initialBalance, script) =>
        withDomain() {
          d =>
            d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
            val genesisBlockId = d.lastBlockId

            d.blockchainUpdater.accountScript(sender) shouldBe 'empty
            d.appendBlock(
              TestBlock.create(
                nextTs,
                genesisBlockId,
                Seq(SetScriptTransaction.signed(1, nextTs, sender, 1, Some(script)).explicitGet())
              ))

            val blockWithScriptId = d.lastBlockId

            d.blockchainUpdater.accountScript(sender) should contain(script)

            d.appendBlock(
              TestBlock.create(
                nextTs,
                genesisBlockId,
                Seq(SetScriptTransaction.signed(1, nextTs, sender, 1, None).explicitGet())
              ))

            d.blockchainUpdater.accountScript(sender) shouldBe 'empty

            d.removeAfter(blockWithScriptId)
            d.blockchainUpdater.accountScript(sender) should contain(script)

            d.removeAfter(genesisBlockId)
            d.blockchainUpdater.accountScript(sender) shouldBe 'empty
        }
    })

    def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): FunctionalitySettings =
      TestFunctionalitySettings.Enabled
        .copy(
          preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }(collection.breakOut),
          blocksForFeatureActivation = 1,
          featureCheckBlocksPeriod = 1
        )
  }
}
