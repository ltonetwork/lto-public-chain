package com.ltonetwork

import com.ltonetwork.settings.WalletSettings
import com.ltonetwork.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = Wallet(WalletSettings(None, "123", None, None,None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
