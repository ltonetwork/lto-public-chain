package com.wavesplatform.transaction
import com.wavesplatform.state.ByteStr
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TransactionFactoryTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  property("accepts 64/128/256/384/512") {
    val lstGen = for {
      a64  <- byteArrayGen(64)
      a128 <- byteArrayGen(128)
      a256 <- byteArrayGen(256)
      a384 <- byteArrayGen(384)
      a512 <- byteArrayGen(512)
    } yield List(a64, a128, a256, a384, a512).map(ByteStr(_).toString)

    forAll(lstGen) { l =>
      TransactionFactory.parseAnchors(l) shouldBe 'right
    }
  }

  property("accepts < 64, prepends zeros") {
    forAll(genBoundedBytes(0, 64)) { b =>
      {
        val r = TransactionFactory.parseAnchors(List(ByteStr(b).toString)).explicitGet()
        r.head.arr.dropWhile(_ == (0: Byte)) sameElements b
        r.head.arr.takeWhile(_ == (0: Byte)).length == 64 - b.length
      }
    }
  }
}
