![github-banner](https://user-images.githubusercontent.com/100821/108692834-6a115200-74fd-11eb-92df-ee07bf62b386.png)

# Public Chain 
In the `master` branch there is a code with functions that is under development. The latest release for each network can be found in the [Releases section](https://github.com/ltonetwork/lto-public-chain/releases), you can switch to the corresponding tag and build the application.

To build the node localy, run
```
sbt build
```

It will produce `lto-public-all-*.jar` in the `target` folder.

For further information please read the [LTO Network documentation](https://docs.ltonetwork.com).

# Docker

## Build

```
docker build . -t ltonetwork/public-node
```

## Run

The simplest way to run a container:
```
docker run -it ltonetwork/public-node
```

**For MAINNET:**
```
docker run -p 6869:6869 -p 6868:6868 -e LTO_HEAP_SIZE=2g -e LTO_ENABLE_REST_API -v YOUR_LOCAL_PATH_HERE:/lto ltonetwork/public-node    
```

**For TESTNET:**
```
docker run -p 6869:6869 -p 6863:6863 -e LTO_NETWORK=TESTNET -e LTO_HEAP_SIZE=2g -v YOUR_LOCAL_PATH_HERE:/lto ltonetwork/public-node
``` 

**You can run the container with these environment variables:**

| Env variable                  | Description                                                                                                                                                                                                                                                                                                                                         |
|-------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `LTO_WALLET_SEED`             | Plain text seed for node wallet. Container converts it to base58.                                                                                                                                                                                                                                                                                   |
| `LTO_WALLET_SEED_BASE58`      | Base58 encoded seed. (Supersedes LTO_WALLET_SEED)                                                                                                                                                                                                                                                                                                   |
| `LTO_WALLET_PASSWORD`         | Password for wallet file.                                                                                                                                                                                                                                                                                                                           |
| `LTO_API_KEY`                 | ApiKey used for the rest api authentication                                                                                                                                                                                                                                                                                                         |
| `LTO_NETWORK`                 | Available values are `MAINNET`, `TESTNET` and `CUSTOM`. (Default: `MAINNET`)                                                                                                                                                                                                                                                                        |
| `LTO_LOG_LEVEL`               | Node logging level, available values: `OFF`, `ERROR`, `WARN`, `INFO`, `DEBUG`, `TRACE`.                                                                                                                                                                                                                                                             |
| `LTO_HEAP_SIZE`               | Java Heap Size limit in -X Command-line Options notation (`-Xms=[your value]`). More details [here](https://docs.oracle.com/cd/E13150_01/jrockit_jvm/jrockit/jrdocs/refman/optionX.html)                                                                                                                                                            |
| `LTO_CONFIG_FILE`             | Path to your LTO Configuration file.                                                                                                                                                                                                                                                                                                                |
| `LTO_DECLARED_ADDRESS`        | String with IP address and port to send as external address during handshake. Could be set automatically if UPnP is enabled. If `declared-address` is set, which is the common scenario for nodes running in the cloud, the node will just listen to incoming connections on `bind-address:port` and broadcast its `declared-address` to its peers. |
| `LTO_NODE_NAME`               | Node name used in the handshake when connecting to other nodes                                                                                                                                                                                                                                                                                      |
| `LTO_ENABLE_REST_API`         | To enable the REST API. (For `MAINNET` default is `false` for `TESTNET` default is `true`)                                                                                                                                                                                                                                                          |
| `LTO_ENABLE_MINING`           | To enable PoS mining (default is `true`)                                                                                                                                                                                                                                                                                                            |
| `LTO_FEATURES`                | Features you wish to vote. E.g. set to 4 to start voting for the Smart Accounts feature. You can also vote for multiple features at by comma seperating them (e.g. 4,5)                                                                                                                                                                             |

_Note: All variables are optional._  

**The following variables can be used to control fee voting:**

| Env variable            | Description                                                                                                                                                                     |
|-------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `COINMARKETCAP_API_KEY` | [CoinMarketCap API key](https://pro.coinmarketcap.com/). When supplied, the script will query CoinMarketCap in addition to CoinGecko to prevent voting based on incorrect data. |
| `LTO_FEE_TARGET`        | Value to determine how to vote. See [Fee Prices](https://blog.ltonetwork.com/tokenomics-update/#fee-prices).                                                                    |

# Debian

To package the build as debian package, run
```
sbt packageAll -Dnetwork=mainnet                            # Mainnet
sbt packageAll -Dnetwork=testnet -DpackageName=lto-testnet  # Testnet
```

## Installation

The `.deb` package can be downloaded from the [GitHub release page](https://github.com/ltonetwork/lto-public-chain/releases).

    wget https://github.com/ltonetwork/lto-public-chain/releases/download/v1.6.3/lto_1.6.3_all.deb 
    dpkg -i lto_1.6.3_all.deb

## Fee vote

The debian package installs a python script named `lto-fee-vote`. Please participate in
[Fee voting](https://blog.ltonetwork.com/tokenomics-update/#fee-voting) by calling this script through a cronjob once an
hour.

    0 * * * *  root  /usr/bin/lto-fee-vote http://localhost:6869 /var/lib/lto/lto/fee-vote

_For testnet use the path `/var/lib/lto-testnet/lto/fee-vote` instead._

If you choose not to enable the REST API on your node, you can query `https://nodes.lto.network` instead of
`http://localhost:6869`.

# Tests

## Unit tests

```
sbt test
```

## End-to-end tests

Install the requirements:
```
pip install -r e2e/requirements.txt
```

Running the whole suite:
```
behave
```

Running a specific feature:
```
behave -i feature_name.feature
```

Running a specific scenario:
```
behave -n 'scenario title'
```

