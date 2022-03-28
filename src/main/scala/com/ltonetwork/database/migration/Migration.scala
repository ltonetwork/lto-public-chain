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

  val maxHeight: Int = readOnly(_.get(Keys.height))

  protected def setHeight(height: Int): Unit = readWrite(rw => setHeight(rw)(height))
  protected def setHeight(rw: RW)(height: Int): Unit = rw.put(Keys.migration(id), height)

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

  protected def before(height: Int): Unit = {}
  protected def after(height: Int): Unit = {}

  protected def applyTo(height: Int, block: Block): Unit

  protected def apply(height: Int = 1): Unit = {
    log.info(s"${description} from block ${height} to ${maxHeight}")
    before(height)

    for (height <- Range.inclusive(height, maxHeight)) {
      val block = readOnly(_.get(Keys.blockAt(height))).get
      if (height % 10000 == 0)
        log.info(s"Block ${height}")

      applyTo(height, block)
    }

    after(height)
  }

  def run(): Unit = {
    val height: Int = readOnly(_.get(Keys.migration(id)))

    if (maxHeight == 0) {
      setHeight(SKIPPED)
    } else if (height < maxHeight) {
      apply(height + 1)
      setHeight(APPLIED)
    }
  }
}

object Migration {
  val SKIPPED: Int = Int.MaxValue - 1
  val APPLIED: Int = Int.MaxValue
}
