package com.ltonetwork.database.migration

import com.ltonetwork.block.Block
import com.ltonetwork.database.{Keys, RW, ReadOnlyDB}
import com.ltonetwork.settings.{BlockchainSettings, FunctionalitySettings}
import com.ltonetwork.utils.ScorexLogging
import org.iq80.leveldb.{DB, ReadOptions}

trait Migration extends ScorexLogging {
  import Migration._

  val id: Int
  val description: String
  val writableDB: DB
  val fs: FunctionalitySettings

  val currentHeight: Int = readOnly(_.get(Keys.migration(id)))
  val maxHeight: Int = readOnly(_.get(Keys.height))

  def isDone: Boolean = currentHeight >= SKIPPED
  def isApplied: Boolean = currentHeight == APPLIED
  def isSkipped: Boolean = currentHeight == SKIPPED

  protected def setHeight(height: Int): Unit = readWrite(_.put(Keys.migration(id), height))

  protected def readOnly[A](f: ReadOnlyDB => A): A = {
    val s = writableDB.getSnapshot
    try f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
    finally s.close()
  }

  protected def readWrite[A](f: RW => A): A = {
    val rw = new RW(writableDB)
    try f(rw)
    finally rw.close()
  }

  protected def before(): Unit = {}
  protected def after(): Unit = {}

  protected def applyTo(height: Int, block: Block): Unit

  protected def apply(): Unit = {
    if (currentHeight == 0) before()

    log.info(s"${description} from block ${currentHeight + 1} to ${maxHeight}")

    for (height <- Range.inclusive(currentHeight + 1, maxHeight)) {
      val block = readOnly(_.get(Keys.blockAt(height))).get
      if (height % 10000 == 0)
        log.info(s"Block ${height}")

      applyTo(height, block)
      setHeight(height)
    }

    after()
  }

  def run(): Unit = {
    if (maxHeight == 0) {
      setHeight(Migration.SKIPPED)
    } else if (!isDone) {
      apply()
      setHeight(Migration.APPLIED)
    }
  }
}

object Migration {
  val SKIPPED: Int = Int.MaxValue - 1
  val APPLIED: Int = Int.MaxValue
}
