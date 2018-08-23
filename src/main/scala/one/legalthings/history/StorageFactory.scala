package one.legalthings.history

import one.legalthings.database.{Keys, LevelDBWriter, RW}
import one.legalthings.settings.WavesSettings
import one.legalthings.state.{BlockchainUpdaterImpl, NG}
import one.legalthings.transaction.BlockchainUpdater
import one.legalthings.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import org.iq80.leveldb.DB

object StorageFactory extends ScorexLogging {
  private val StorageVersion = 1

  def apply(settings: WavesSettings, db: DB, time: Time): BlockchainUpdater with NG = {
    checkVersion(db)
    val levelDBWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings, settings.maxCacheSize)
    new BlockchainUpdaterImpl(levelDBWriter, settings, time)
  }

  private def checkVersion(db: DB) = {
    val rw      = new RW(db)
    val version = rw.get(Keys.version)
    val height  = rw.get(Keys.height)
    if (version != StorageVersion) {
      if (height == 0) {
        // The storage is empty, set current version
        rw.put(Keys.version, StorageVersion)
      } else {
        // Here we've detected that the storage is not empty and doesn't contain version
        log.error(
          s"Storage version $version is not compatible with expected version $StorageVersion! Please, rebuild node's state, use import or sync from scratch.")
        log.error("FOR THIS REASON THE NODE STOPPED AUTOMATICALLY")
        forceStopApplication(UnsupportedFeature)
      }
    }
    rw.close()
  }
}
