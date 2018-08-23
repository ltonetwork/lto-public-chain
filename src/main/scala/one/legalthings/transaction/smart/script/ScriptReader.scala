package one.legalthings.transaction.smart.script

import one.legalthings.crypto
import one.legalthings.lang.ScriptVersion.Versions.V1
import one.legalthings.lang.ScriptVersion
import one.legalthings.lang.v1.Serde
import one.legalthings.transaction.ValidationError.ScriptParseError
import one.legalthings.transaction.smart.script.v1.ScriptV1

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val version          = bytes.head
    val scriptBytes      = bytes.drop(1).dropRight(checksumLength)

    for {
      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      sv <- ScriptVersion
        .fromInt(version)
        .fold[Either[ScriptParseError, ScriptVersion]](Left(ScriptParseError(s"Invalid version: $version")))(v => Right(v))
      script <- sv match {
        case V1 =>
          ScriptV1
            .validateBytes(scriptBytes)
            .flatMap { _ =>
              Serde.deserialize(scriptBytes).flatMap(ScriptV1(_, checkSize = false))
            }
            .left
            .map(ScriptParseError)
      }
    } yield script
  }

}
