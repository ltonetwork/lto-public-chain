package com.ltonetwork.state.diffs.smart

import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.evaluator.EvaluatorV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.transaction.Transaction
import com.ltonetwork.transaction.smart.BlockchainContext
import com.ltonetwork.transaction.transfer.TransferTransaction
import com.ltonetwork.utils.dummyCompilerContext
import fastparse.core.Parsed.Success
import monix.eval.Coeval

package object predef {
  val networkByte: Byte = 'u'

  def runScript[T](script: String, tx: Transaction = null, networkByte: Byte = networkByte): Either[String, T] = {
    val Success(expr, _) = Parser(script)
    for {
      compileResult <- CompilerV1(dummyCompilerContext, expr)
      (typedExpr, tpe) = compileResult
      r <- EvaluatorV1.applywithLogging[T](BlockchainContext.build(networkByte, Coeval(tx), Coeval(???), null), typedExpr)._2
    } yield r
  }

  private def dropLastLine(str: String): String = str.replace("\r", "").split('\n').init.mkString("\n")

  def scriptWithAllFunctions(t: TransferTransaction): String =
    s"""${dropLastLine(scriptWithPureFunctions(t))}
       |${dropLastLine(scriptWithWavesFunctions(t))}
       |${dropLastLine(scriptWithCryptoFunctions)}
       |if rnd then pure && waves else crypto""".stripMargin

  def scriptWithPureFunctions(t: TransferTransaction): String =
    s"""
       | # Pure context
       | # 1) basic(+ eq) -> mulLong, divLong, modLong, sumLong, subLong, sumString, sumByteVector
       |
       | let rnd = tx.timestamp % 2 == 0
       | let longAll = 1000 * 2 == 2000 && 1000 / 2 == 500 && 1000 % 2 == 0 && 1000 + 2 == 1002 && 1000 - 2 == 998
       | let sumString = "ha" + "-" +"ha" == "ha-ha"
       | let sumByteVector = match tx {
       |     case _: TransferTransaction => true
       |     case _ => false
       |   }
       |
       | let eqUnion = match tx {
       |   case t0: TransferTransaction => t0.recipient == Address(base58'${t.recipient.bytes.base58}')
       |   case _ => false
       | }
       |
       | let basic = longAll && sumString && sumByteVector && eqUnion
       |
       | # 2) ne
       | let nePrim = 1000 != 999 && "ha" +"ha" != "ha-ha" && tx.bodyBytes != base64'hahaha'
       | let neDataEntryAndGetElement = match tx {
       |    case _: TransferTransaction => true
       |    case _ => false
       |  }
       |
       | let neOptionAndExtractHeight =  true
       |
       | let ne = nePrim && neDataEntryAndGetElement && neOptionAndExtractHeight
       |
       |# 3) gt, ge
       | let gteLong = 1000 > 999 && 1000 >= 999
       |
       |# 4) getListSize
       | let getListSize = match tx {
       |    case _: TransferTransaction => true
       |    case _ => false
       |  }
       |
       |# 5) unary
       | let unary = -1 == -1 && false == !true
       |
       |# 6) fraction, sizeBytes, takeBytes, dropBytes, takeRightBytes, dropRightBytes, sizeString, takeString, dropString,
       |#    takeRightString, dropRightString, isDefined
       | let frAction = fraction(12, 3, 4) == 9
       | let bytesOps = match tx {
       |     case t1: TransferTransaction => true
       |     case _ => false
       |   }
       | let strOps = size("haha") != 0 && take("haha", 1) != "" && drop("haha", 0) != "" && takeRight("haha", 1) != "" && dropRight("haha", 0) != ""
       |
       | let pure = basic && ne && gteLong && getListSize && unary && frAction && bytesOps && strOps
       | pure""".stripMargin

  def scriptWithWavesFunctions(t: TransferTransaction): String =
    s""" # Waves context
       | let entries = match tx {
       |   case _: TransferTransaction => true
       |   case _ => false
       | }
       |
       | let aFromPK = addressFromPublicKey(tx.senderPublicKey) == tx.sender
       | let aFromStrOrRecip = match tx {
       |   case t1: TransferTransaction => t1.recipient == Address(base58'${t.recipient.bytes.base58}')
       |   case _ => false
       | }
       |
       | let waves = entries && aFromPK && aFromStrOrRecip && height > 0
       | waves""".stripMargin

  def scriptWithCryptoFunctions: String =
    s"""
       | # Crypto context
       | let bks = blake2b256(base58'') != base58'' && keccak256(base58'') != base58'' && sha256(base58'') != base58''
       | let sig = sigVerify(base58'333', base58'123', base58'567') != true
       | let str58 = fromBase58String(toBase58String(tx.id)) == tx.id
       | let str64 = fromBase64String(toBase64String(tx.id)) == tx.id
       |
       | let crypto = bks && sig && str58 && str64
       | crypto""".stripMargin

}
