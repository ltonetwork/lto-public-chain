package com.ltonetwork.database

import com.ltonetwork.settings.BlockchainSettings
import org.iq80.leveldb.DB

package object migration {
  def runMigrations(writableDB: DB, settings: BlockchainSettings): Unit = {
    CalculateBurnMigration(writableDB, settings.functionalitySettings).run()
  }
}
