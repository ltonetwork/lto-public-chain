package one.legalthings

import one.legalthings.settings.WalletSettings
import one.legalthings.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = Wallet(WalletSettings(None, "123", None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
