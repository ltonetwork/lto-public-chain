package com.ltonetwork.settings

import com.ltonetwork.state.ByteStr
import scala.concurrent.duration._

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET: GenesisSettings = GenesisSettings(
    1547303338475L,
    1547303338475L,
    Constants.UnitsInLTO * Constants.TotalLTOMain,
    ByteStr.decodeBase58("3xfb8SHvXK1eUT73kqq89Ayvt2w5ivy9CM2HHVYZ6H5zjgmbukadkFhis74vXsfak9YjwUCBMQUKsKmiRPmAWaB3").toOption,
    List(
      GenesisTransactionSettings("3JpzrZcSDhzRVeENoqqB98c6hTNg7WJaBKt", Constants.UnitsInLTO * 1000L),
      GenesisTransactionSettings("3JqBJaDet2MWisRPNLtN5snCxkGHtKaLRHv", Constants.UnitsInLTO * 1000L),
      GenesisTransactionSettings("3JygettiPvCrb7rSoWDzRHbBWKdMva2d5tu", Constants.UnitsInLTO * 499997000L),
      GenesisTransactionSettings("3JyxAP1fpeYXv77FzxihgLsDVMccwLE64rd", Constants.UnitsInLTO * 1000L)
    ),
    153722867L,
    60.seconds
  )

  val TESTNET: GenesisSettings = GenesisSettings(
    1534497076380L,
    1534497076380L,
    Constants.UnitsInLTO * Constants.TotalLTO,
    ByteStr.decodeBase58("47pP5r1Kh159XmxcfG2eQVj6dKNhub3mvGgpJovcw7EcZyJswFLYyKGYNV21BGJ8pwkajA75ZLMWFBdv3BzMRMk").toOption,
    List(
      GenesisTransactionSettings("3N6mZMgGqYn9EVAR2Vbf637iej4fFipECq8", (Constants.UnitsInLTO * Constants.TotalLTO * 0.01).toLong),
      GenesisTransactionSettings("3N51gbw5W3xvSkcAXtLnXc3SQh2m9e6TBcy", (Constants.UnitsInLTO * Constants.TotalLTO * 0.01).toLong),
      GenesisTransactionSettings("3NAxYD4nFbYqHo8gz9Hsfj13s283xNYvGNi", (Constants.UnitsInLTO * Constants.TotalLTO * 0.9).toLong),
      GenesisTransactionSettings("3Mv7ajrPLKewkBNqfxwRZoRwW6fziehp7dQ", (Constants.UnitsInLTO * Constants.TotalLTO * 0.01).toLong),
      GenesisTransactionSettings("3NARPnCPG4egZbFUQENZ6VDojQqMCpGEG9i", Constants.UnitsInLTO * (Constants.TotalLTO * 0.07).round),
    ),
    100,
    60.seconds
  )
}