package com.ltonetwork.it

import com.ltonetwork.it.util._
import com.ltonetwork.state.DataEntry

package object sync {
  val minFee                     = 1.lto
  val leasingFee                 = 1.lto
  val smartFee                   = 1.lto
  val issueFee                   = 1.lto
  val burnFee                    = 1.lto
  val sponsorFee                 = 1.lto
  val transferAmount             = 10.lto
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 1.lto
  val someAssetAmount            = 9999999999999l
  val supportedVersions          = List(null, "2") //sign and broadcast use default for V1

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee + 0.001.lto * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }
}
