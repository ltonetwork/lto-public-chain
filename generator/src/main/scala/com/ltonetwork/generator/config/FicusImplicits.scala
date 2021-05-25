package com.ltonetwork.generator.config

import com.google.common.base.CaseFormat
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import com.ltonetwork.transaction.{TransactionBuilder, TransactionBuilders}

trait FicusImplicits {

  implicit val distributionsReader: ValueReader[Map[TransactionBuilder, Double]] = {
    val converter                                = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toTxType(key: String): TransactionBuilder = TransactionBuilders.by(converter.convert(key)).get

    CollectionReaders.mapValueReader[Double].map { xs =>
      xs.map { case (k, v) => toTxType(k) -> v }
    }
  }
}
