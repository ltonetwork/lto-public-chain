package com.ltonetwork.state

import java.io.File
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.typesafe.config.ConfigFactory
import com.ltonetwork.account.{Address, AddressScheme}
import com.ltonetwork.database.LevelDBWriter
import com.ltonetwork.db.LevelDBFactory
import com.ltonetwork.lang.v1.traits.Environment
import com.ltonetwork.settings.{LtoSettings, loadConfig}
import com.ltonetwork.state.bench.DataTestData
import com.ltonetwork.transaction.smart.LtoEnvironment
import com.ltonetwork.utils.Base58
import monix.eval.Coeval
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations._
import scodec.bits.BitVector

import scala.io.Codec

/**
  * Tests over real database. How to test:
  * 1. Download a database
  * 2. Import it
  * 3. Run ExtractInfo to collect queries for tests
  * 4. Make Caches.MaxSize = 1
  * 5. Run this test
  */
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class LtoEnvironmentBenchmark {

  /*
  @Benchmark
  def resolveAddress_test(st: ResolveAddressSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.resolveAlias(st.aliases.random))
  }

  @Benchmark
  def transactionById_test(st: TransactionByIdSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.transactionById(st.allTxs.random))
  }

  @Benchmark
  def transactionHeightById_test(st: TransactionByIdSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.transactionById(st.allTxs.random))
  }

  @Benchmark
  def accountBalanceOf_lto_test(st: AccountBalanceOfLtoSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.accountBalanceOf(LangAddress(ByteVector(st.accounts.random)), None))
  }

  @Benchmark
  def accountBalanceOf_asset_test(st: AccountBalanceOfAssetSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.accountBalanceOf(LangAddress(ByteVector(st.accounts.random)), Some(st.assets.random)))
  }

  @Benchmark
  def data_test(st: DataSt, bh: Blackhole): Unit = {
    val x = st.data.random
    bh.consume(st.environment.data(LangAddress(x.addr), x.key, x.dataType))
  }
 */
}

object LtoEnvironmentBenchmark {

  @State(Scope.Benchmark)
  class TransactionByIdSt extends BaseSt {
    val allTxs: Vector[Array[Byte]] = load("transactionById", benchSettings.restTxsFile)(x => Base58.decode(x).get)
  }

  @State(Scope.Benchmark)
  class TransactionHeightByIdSt extends TransactionByIdSt

  @State(Scope.Benchmark)
  class AccountBalanceOfLtoSt extends BaseSt {
    val accounts: Vector[Array[Byte]] = load("accounts", benchSettings.accountsFile)(x => Address.fromString(x).explicitGet().bytes.arr)
  }

  @State(Scope.Benchmark)
  class AccountBalanceOfAssetSt extends AccountBalanceOfLtoSt {
    val assets: Vector[Array[Byte]] = load("assets", benchSettings.assetsFile)(x => Base58.decode(x).get)
  }

  @State(Scope.Benchmark)
  class DataSt extends BaseSt {
    val data: Vector[DataTestData] = load("data", benchSettings.dataFile) { line =>
      DataTestData.codec.decode(BitVector.fromBase64(line).get).require.value
    }
  }

  @State(Scope.Benchmark)
  class BaseSt {
    protected val benchSettings: Settings = Settings.fromConfig(ConfigFactory.load())
    private val ltoSettings: LtoSettings = {
      val config = loadConfig(ConfigFactory.parseFile(new File(benchSettings.networkConfigFile)))
      LtoSettings.fromConfig(config)
    }

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = ltoSettings.blockchainSettings.addressSchemeCharacter.toByte
    }

    private val db: DB = {
      val dir = new File(ltoSettings.dataDirectory)
      if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${ltoSettings.dataDirectory}'")
      LevelDBFactory.factory.open(dir, new Options)
    }

    val environment: Environment = {
      val state = new LevelDBWriter(db, ltoSettings.blockchainSettings.functionalitySettings)
      new LtoEnvironment(
        AddressScheme.current.chainId,
        Coeval.raiseError(new NotImplementedError("tx is not implemented")),
        Coeval(state.height),
        state
      )
    }

    @TearDown
    def close(): Unit = {
      db.close()
    }

    protected def load[T](label: String, absolutePath: String)(f: String => T): Vector[T] = {
      scala.io.Source
        .fromFile(absolutePath)(Codec.UTF8)
        .getLines()
        .map(f)
        .toVector
    }
  }

  implicit class VectorOps[T](self: Vector[T]) {
    def random: T = self(ThreadLocalRandom.current().nextInt(self.size))
  }

}
