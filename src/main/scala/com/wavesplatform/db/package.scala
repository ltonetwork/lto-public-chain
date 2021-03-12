package com.wavesplatform

import java.io.File

import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.database.{ReadOnlyDB, RW}
import org.iq80.leveldb.{DB, Options, ReadOptions}

package object db extends ScorexLogging {

  def openDB(path: String, recreate: Boolean = false): DB = {
    log.debug(s"Open DB at $path")
    val file = new File(path)
    val options = new Options()
      .createIfMissing(true)
      .paranoidChecks(true)

    if (recreate) {
      LevelDBFactory.factory.destroy(file, options)
    }

    file.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

  implicit class DBExt(val db: DB) extends AnyVal {
    def readOnly[A](f: ReadOnlyDB => A): A = {
      val snapshot = db.getSnapshot
      try f(new ReadOnlyDB(db, new ReadOptions().snapshot(snapshot)))
      finally snapshot.close()
    }
  }
}
