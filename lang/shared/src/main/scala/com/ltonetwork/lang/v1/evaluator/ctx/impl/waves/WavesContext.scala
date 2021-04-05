package com.ltonetwork.lang.v1.evaluator.ctx.impl.waves

import cats.data.EitherT
import com.ltonetwork.lang.v1.compiler.Terms._
import com.ltonetwork.lang.v1.compiler.Types.{BYTEVECTOR, STRING, _}
import com.ltonetwork.lang.v1.evaluator.FunctionIds._
import com.ltonetwork.lang.v1.evaluator.ctx._
import com.ltonetwork.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext}
import com.ltonetwork.lang.v1.traits._
import com.ltonetwork.lang.v1.{CTX, FunctionHeader}
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  import Bindings._
  import Types._

  def build(env: Environment): CTX = {
    def secureHashExpr(xs: EXPR): EXPR = FUNCTION_CALL(
      FunctionHeader.Native(SHA256),
      List(
        FUNCTION_CALL(
          FunctionHeader.Native(BLAKE256),
          List(xs)
        )
      )
    )

    val addressFromPublicKeyF: BaseFunction = UserFunction("addressFromPublicKey", addressType.typeRef, "@publicKey" -> BYTEVECTOR) {
      FUNCTION_CALL(
        FunctionHeader.User("Address"),
        List(
          BLOCK(
            LET(
              "@afpk_withoutChecksum",
              FUNCTION_CALL(
                PureContext.sumByteVector,
                List(
                  CONST_BYTEVECTOR(ByteVector(EnvironmentFunctions.AddressVersion, env.networkByte)),
                  // publicKeyHash
                  FUNCTION_CALL(
                    PureContext.takeBytes,
                    List(
                      secureHashExpr(REF("@publicKey")),
                      CONST_LONG(EnvironmentFunctions.HashLength)
                    )
                  )
                )
              )
            ),
            // bytes
            FUNCTION_CALL(
              PureContext.sumByteVector,
              List(
                REF("@afpk_withoutChecksum"),
                FUNCTION_CALL(
                  PureContext.takeBytes,
                  List(
                    secureHashExpr(REF("@afpk_withoutChecksum")),
                    CONST_LONG(EnvironmentFunctions.ChecksumLength)
                  )
                )
              )
            )
          )
        )
      )
    }

    def removePrefixExpr(str: EXPR, prefix: String): EXPR = IF(
      FUNCTION_CALL(
        PureContext.eq,
        List(
          FUNCTION_CALL(PureContext.takeString, List(str, CONST_LONG(prefix.length))),
          CONST_STRING(prefix)
        )
      ),
      FUNCTION_CALL(PureContext.dropString, List(str, CONST_LONG(prefix.length))),
      str
    )

    def verifyAddressChecksumExpr(addressBytes: EXPR): EXPR = FUNCTION_CALL(
      PureContext.eq,
      List(
        // actual checksum
        FUNCTION_CALL(PureContext.takeRightBytes, List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength))),
        // generated checksum
        FUNCTION_CALL(
          PureContext.takeBytes,
          List(
            secureHashExpr(FUNCTION_CALL(PureContext.dropRightBytes, List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength)))),
            CONST_LONG(EnvironmentFunctions.ChecksumLength)
          )
        )
      )
    )

    val addressFromStringF: BaseFunction = UserFunction("addressFromString", optionAddress, "@string" -> STRING) {
      BLOCK(
        LET("@afs_addrBytes",
            FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(removePrefixExpr(REF("@string"), EnvironmentFunctions.AddressPrefix)))),
        IF(
          FUNCTION_CALL(
            PureContext.eq,
            List(
              FUNCTION_CALL(PureContext.sizeBytes, List(REF("@afs_addrBytes"))),
              CONST_LONG(EnvironmentFunctions.AddressLength)
            )
          ),
          IF(
            // version
            FUNCTION_CALL(
              PureContext.eq,
              List(
                FUNCTION_CALL(PureContext.takeBytes, List(REF("@afs_addrBytes"), CONST_LONG(1))),
                CONST_BYTEVECTOR(ByteVector(EnvironmentFunctions.AddressVersion))
              )
            ),
            IF(
              // networkByte
              FUNCTION_CALL(
                PureContext.eq,
                List(
                  FUNCTION_CALL(
                    PureContext.takeBytes,
                    List(
                      FUNCTION_CALL(PureContext.dropBytes, List(REF("@afs_addrBytes"), CONST_LONG(1))),
                      CONST_LONG(1)
                    )
                  ),
                  CONST_BYTEVECTOR(ByteVector(env.networkByte))
                )
              ),
              IF(
                verifyAddressChecksumExpr(REF("@afs_addrBytes")),
                FUNCTION_CALL(FunctionHeader.User("Address"), List(REF("@afs_addrBytes"))),
                REF("unit")
              ),
              REF("unit")
            ),
            REF("unit")
          ),
          REF("unit")
        )
      )
    }

    val txCoeval: Coeval[Either[String, CaseObj]]  = Coeval.evalOnce(Right(transactionObject(env.transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val vars: Map[String, (FINAL, LazyVal)] = Map(
      ("height", (com.ltonetwork.lang.v1.compiler.Types.LONG, LazyVal(EitherT(heightCoeval)))),
      ("tx", (outgoingTransactionType, LazyVal(EitherT(txCoeval))))
    )

    val functions = Seq(
      addressFromPublicKeyF,
      addressFromStringF,
    )

    CTX(Types.wavesTypes, vars, functions)
  }
}
