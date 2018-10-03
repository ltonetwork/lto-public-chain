package com.wavesplatform.transaction
import com.wavesplatform.state.ByteStr
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TransactionFactoryTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
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
