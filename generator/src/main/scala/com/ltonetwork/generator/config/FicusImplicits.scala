package com.ltonetwork.generator.config

import com.google.common.base.CaseFormat
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import com.ltonetwork.transaction.{TransactionBuilder, TransactionBuilders}

trait FicusImplicits {
  private[this] val byName: Map[String, TransactionBuilder] = TransactionBuilders.all.map {
    case (_, builder) => builder.getClass.getSimpleName.replaceAll("\\$$", "") -> builder
  }

  private def by(name: String): Option[TransactionBuilder] = byName.get(name)

  implicit val distributionsReader: ValueReader[Map[TransactionBuilder, Double]] = {
    val converter                                 = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toTxType(key: String): TransactionBuilder = by(converter.convert(key)).get

    CollectionReaders.mapValueReader[Double].map { xs =>
      xs.map { case (k, v) => toTxType(k) -> v }
    }
  }
}
