package com.wavesplatform.it

import com.wavesplatform.it.util._
import com.wavesplatform.state.DataEntry

package object sync {
  val minFee                     = 1.waves
  val leasingFee                 = 1.waves
  val smartFee                   = 1.waves
  val issueFee                   = 1.waves
  val burnFee                    = 1.waves
  val sponsorFee                 = 1.waves
  val transferAmount             = 10.waves
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 1.waves
  val someAssetAmount            = 9999999999999l
  val supportedVersions          = List(null, "2") //sign and broadcast use default for V1

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee + 0.001.waves * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }
}
