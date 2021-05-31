package com.ltonetwork.state.diffs.smart.predef

import com.ltonetwork.account.PrivateKeyAccount
import com.ltonetwork.lang.Global
import com.ltonetwork.lang.v1.compiler.CompilerV1
import com.ltonetwork.lang.v1.parser.Parser
import com.ltonetwork.state._
import com.ltonetwork.state.diffs.smart.smartEnabledFS
import com.ltonetwork.state.diffs.{ENOUGH_AMT, assertDiffAndState}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.utils.{Base58, dummyCompilerContext}
import com.ltonetwork.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.ltonetwork.transaction.smart.SetScriptTransaction
import com.ltonetwork.transaction.smart.script.v1.ScriptV1
import com.ltonetwork.transaction.genesis.GenesisTransaction
import org.scalacheck.Gen

class ContextFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndPayments = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis1 = GenesisTransaction.create(master, ENOUGH_AMT * 3, ts).explicitGet()
    genesis2 = GenesisTransaction.create(recipient, ENOUGH_AMT * 3, ts).explicitGet()
    transfer <- transferGeneratorP(ts, master, recipient.toAddress, 100000000L)

    untypedScript <- Gen
      .choose(1, 3)
      .map {
        case 1 => scriptWithPureFunctions(transfer)
        case 2 => scriptWithLtoFunctions(transfer)
        case 3 => scriptWithCryptoFunctions
      }
      .map(x => Parser(x).get.value)

    typedScript = {
      val compilerScript = CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1
      ScriptV1(compilerScript).explicitGet()
    }
    setScriptTransaction: SetScriptTransaction = SetScriptTransaction.selfSigned(1, recipient, Some(typedScript), 1000000000L, ts).explicitGet()

  } yield (Seq(genesis1, genesis2), setScriptTransaction, transfer)

  property("validation of all functions from contexts") {
    forAll(preconditionsAndPayments) {
      case (genesis, setScriptTransaction, transfer) =>
        assertDiffAndState(smartEnabledFS) { append =>
          append(genesis).explicitGet()
          append(Seq(setScriptTransaction)).explicitGet()
          append(Seq(transfer)).explicitGet()
        }
    }
  }

  property("base64 amplification") {
    val script =
      """
        |let a = base58'7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy7kPFrHDiGw1rCm7LPszuECwWYL3dMf6iMifLRDJQZMzy'
        |let b = toBase64String( a ); let c = toBytes( b )
        |let d = toBase64String( c ); let e = toBytes( d )
        |let f = toBase64String( e ); let g = toBytes( f )
        |let h = toBase64String( g ); let i = toBytes( h )
        |let j = toBase64String( i ); let k = toBytes( j )
        |let l = toBase64String( k ); let m = toBytes( l )
        |let n = toBase64String( m ); let o = toBytes( n )
        |let p = toBase64String( o ); let q = toBytes( p )
        |let r = toBase64String( q ); let s = toBytes( r )
        |let t = toBase64String( s ); let u = toBytes( t )
        |let v = toBase64String( u ); let w = toBytes( v )
        |let x = toBase64String( w ); let y = toBytes( x )
        |let z = toBase64String( y ); let a0 = toBytes( z )
        |let a1 = toBase64String( a0 ); let a2 = toBytes( a1 )
        |let a3 = toBase64String( a2 ); let a4 = toBytes( a3 )
        |let a5 = toBase64String( a4 ); let a6 = toBytes( a5 )
        |let a7 = toBase64String( a6 ); let a8 = toBytes( a7 )
        |let a9 = toBase64String( a8 ); let aa = toBytes( a9 )
        |let ab = toBase64String( aa ); let ac = toBytes( ab )
        |let ad = toBase64String( ac ); let ae = toBytes( ad )
        |let af = toBase64String( ae ); let ag = toBytes( af )
        |let ah = toBase64String( ag ); let ai = toBytes( ah )
        |let aj = toBase64String( ai ); let ak = toBytes( aj )
        |let al = toBase64String( ak ); let am = toBytes( al )
        |let an = toBase64String( am ); let ao = toBytes( an )
        |let ap = toBase64String( ao ); let aq = toBytes( ap )
        |let ar = toBase64String( aq ); let as = toBytes( ar )
        |let at = toBase64String( as ); let au = toBytes( at )
        |let av = toBase64String( au ); let aw = toBytes( av )
        |let ax = toBase64String( aw ); let ay = toBytes( ax )
        |let az = toBase64String( ay ); let b0 = toBytes( az )
        |let b1 = toBase64String( b0 ); let b2 = toBytes( b1 )
        |let b3 = toBase64String( b2 ); let b4 = toBytes( b3 )
        |let b5 = toBase64String( b4 ); let b6 = toBytes( b5 )
        |let b7 = toBase64String( b6 ); let b8 = toBytes( b7 )
        |let b9 = toBase64String( b8 ); let ba = toBytes( b9 )
        |let bb = toBase64String( ba ); let bc = toBytes( bb )
        |let bd = toBase64String( bc ); let be = toBytes( bd )
        |let bf = toBase64String( be ); let bg = toBytes( bf )
        |let bh = toBase64String( bg ); let bi = toBytes( bh )
        |let bj = toBase64String( bi ); let bk = toBytes( bj )
        |let bl = toBase64String( bk ); let bm = toBytes( bl )
        |let bn = toBase64String( bm ); let bo = toBytes( bn )
        |let bp = toBase64String( bo ); let bq = toBytes( bp )
        |let br = toBase64String( bq ); let bs = toBytes( br )
        |let bt = toBase64String( bs ); let bu = toBytes( bt )
        |let bv = toBase64String( bu ); let bw = toBytes( bv )
        |let bx = toBase64String( bw ); let by = toBytes( bx )
        |let bz = toBase64String( by ); let c0 = toBytes( bz )
        |let c1 = toBase64String( c0 ); let c2 = toBytes( c1 )
        |let c3 = toBase64String( c2 ); let c4 = toBytes( c3 )
        |let c5 = toBase64String( c4 ); let c6 = toBytes( c5 )
        |let c7 = toBase64String( c6 ); let c8 = toBytes( c7 )
        |let c9 = toBase64String( c8 ); let ca = toBytes( c9 )
        |let cb = toBase64String( ca ); let cc = toBytes( cb )
        |let cd = toBase64String( cc ); let ce = toBytes( cd )
        |let cf = toBase64String( ce ); let cg = toBytes( cf )
        |let ch = toBase64String( cg ); let ci = toBytes( ch )
        |let cj = toBase64String( ci ); let ck = toBytes( cj )
        |let cl = toBase64String( ck ); let cm = toBytes( cl )
        |let cn = toBase64String( cm ); let co = toBytes( cn )
        |let cp = toBase64String( co ); let cq = toBytes( cp )
        |let cr = toBase64String( cq ); let cs = toBytes( cr )
        |let ct = toBase64String( cs ); let cu = toBytes( ct )
        |let cv = toBase64String( cu ); let cw = toBytes( cv )
        |let cx = toBase64String( cw ); let cy = toBytes( cx )
        |let cz = toBase64String( cy ); let d0 = toBytes( cz )
        |let d1 = toBase64String( d0 ); let d2 = toBytes( d1 )
        |let d3 = toBase64String( d2 ); let d4 = toBytes( d3 )
        |let d5 = toBase64String( d4 ); let d6 = toBytes( d5 )
        |let d7 = toBase64String( d6 ); let d8 = toBytes( d7 )
        |let d9 = toBase64String( d8 ); let da = toBytes( d9 )
        |let db = toBase64String( da ); let dc = toBytes( db )
        |let dd = toBase64String( dc ); let de = toBytes( dd )
        |sha256( de ) != base58'123'
      """.stripMargin
    runScript[Boolean](script) shouldBe Left(s"base64Encode input exceeds ${Global.MaxBase64Bytes}")
  }
}
