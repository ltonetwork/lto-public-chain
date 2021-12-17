package com.ltonetwork.api.http

import akka.http.scaladsl.server.Directive1
import com.ltonetwork.block.Block
import com.ltonetwork.state.{Blockchain, ByteStr}
import com.ltonetwork.transaction.TransactionBuilders

trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(blockchain: Blockchain, encodedSignature: String): Directive1[Block] =
    if (encodedSignature.length > TransactionBuilders.SignatureStringLength) complete(InvalidSignature)
    else {
      ByteStr
        .decodeBase58(encodedSignature)
        .toOption
        .toRight(InvalidSignature)
        .flatMap(s => blockchain.blockById(s).toRight(BlockDoesNotExist)) match {
        case Right(b) => provide(b)
        case Left(e)  => complete(e)
      }
    }
}
