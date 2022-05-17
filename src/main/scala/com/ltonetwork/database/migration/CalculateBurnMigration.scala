package com.ltonetwork.database.migration
import com.ltonetwork.account.Address
import com.ltonetwork.block.Block.{CloserBlockFeePart, OpenerBlockFeePart}
import com.ltonetwork.block.{Block, BlockRewardCalculator}
import com.ltonetwork.database.Keys
import com.ltonetwork.features.BlockchainFeatures
import com.ltonetwork.settings.FunctionalitySettings
import com.ltonetwork.state.EitherExt2
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.transaction.transfer.MassTransferTransaction
import com.ltonetwork.utils.{MigrationError, forceStopApplication}
import org.iq80.leveldb.DB

case class CalculateBurnMigration(writableDB: DB, fs: FunctionalitySettings) extends Migration {
  override val id: Int = 1
  override val description: String = "Calculate burned LTO for every block"

  var burned: Long = 0L

  val burnFeetureHeight: Int = readOnly(_.get(Keys.activatedFeatures))
    .getOrElse(BlockchainFeatures.BurnFeeture.id, Int.MaxValue)

  def isBurnAddress(address: Address): Boolean = fs.burnAddresses.contains(address.toString)

  def addressId(address: Address): BigInt = readOnly(_.get(Keys.addressId(address))).get
  def burnedAt(height: Int): Long = readOnly(_.get(Keys.burned(height)))

  protected def burnAddressTransfers(block: Block): Long =
    block.transactionData.foldLeft(0L)((burned, tx) => burned + (tx match {
      case t: TransferTransaction => if (isBurnAddress(t.recipient)) t.amount else 0L
      case mt: MassTransferTransaction => mt.transfers.filter(t => isBurnAddress(t.address)).map(_.amount).sum
      case _ => 0L
    }).toLong)

  protected def transactionBurn(height: Int, block: Block): Long =
    if (height > burnFeetureHeight)
      block.transactionCount * BlockRewardCalculator.feeBurnAmt
    else if (height == burnFeetureHeight)
      CloserBlockFeePart(block.transactionCount * BlockRewardCalculator.feeBurnAmt)
    else
      0L

  override protected def before(height: Int): Unit = {
    if (readOnly(_.get(Keys.activatedFeatures)).get(BlockchainFeatures.Juicy.id).exists(_ <= maxHeight)) {
      log.error("Unable to apply calculate burn migration: Juicy is already active. Please sync from genesis.")
      forceStopApplication(MigrationError)
    }

    if (height > 0)
      burned = burnedAt(height)
  }

  override protected def applyTo(height: Int, block: Block): Unit = readWrite { rw =>
    burned += burnAddressTransfers(block) + transactionBurn(height, block)
    rw.put(Keys.burned(height), burned)

    setHeight(rw)(height)
  }

  override protected def after(height: Int): Unit = readWrite { rw =>
    fs.burnAddresses
      .map(a => addressId(Address.fromString(a).explicitGet()))
      .foreach(addressId => {
        rw.get(Keys.ltoBalanceHistory(addressId)).foreach(height => rw.delete(Keys.ltoBalance(addressId)(height)))
        rw.delete(Keys.ltoBalanceHistory(addressId))
      })
  }
}
