![LTO Network](https://user-images.githubusercontent.com/100821/108692834-6a115200-74fd-11eb-92df-ee07bf62b386.png)

There are 4 configuration variations of the node. Each configuration serves a different need of the node:

| Node Type            | Description                                                                                                   |
| -------------------- | ------------------------------------------------------------------------------------------------------------- |
| **Public Node only** | Contains only the public node. This configuration can be used for mining                                      |
| [Anchor Node]        | Contains both the public node and the indexer. The indexer is configured for the anchoring of hashes          |
| [Identity Node]      | Contains both the public node and the indexer. The indexer is configured for DID documents and trust networks |
| [Full Node]          | Contains all services. This configuration can used to run decentralized workflows                             |

[anchor node]: https://github.com/ltonetwork/lto-anchor-node
[identity node]: https://github.com/ltonetwork/lto-identity-node
[full node]: https://github.com/ltonetwork/lto-full-node

# LTO Network Public Node

## Configuration

Before you run the node you will need to configure a few environment variables per service:

**Public Node Container**

1. LTO_WALLET_SEED: The seed of your wallet. Your account will need at least 1000 LTO to be able to start mining.
2. LTO_WALLET_SEED_BASE58: The seed of your wallet but then base58 encoded. This will overwrite the LTO_WALLET_SEED
3. LTO_WALLET_PASSWORD: This password is used to encrypt your seed on disk.
4. LTO_API_KEY: Choose an api-key to be able to perform certain actions in the GUI.
5. LTO_ENABLE_REST_API: To enable the REST API. (For Mainnet default is false for Testnet default is true)
6. LTO_NETWORK: Choose the network you want to connect your node to. The options are: `MAINNET` and `TESTNET` (default is`MAINNET`).
7. LTO_DECLARED_ADDRESS: If you have your node running on a VPS set you public ip here with your port. E.g. `52.50.202.26:6868`

For other options check out the [Docker container of the Public Node on Github](https://github.com/legalthings/docker-public-node).

## Run on a (virtual) machine

```bash
docker-compose up
```

Docker compose is configured to run the node on a local machine on port 80. If you would like to run the node on different
port you will need to change the `docker-compose.yml` to

```bash
ports:
    - <your-port>:6869
```

This way the node will be accessible via port 80.

Or you can use a reverse proxy like NGINX to make the node publicly available. This is highly recommended.

## Run in AWS Elastic Beanstalk

Take to following steps to install the node on EB:

1. Zip the Dockerrun.aws.json file
2. Create an application
3. Inside the created application, create an environment: `webserver environment`
4. Select following settings:

   - Platform: Docker
   - Upload the zipped file

5. Configure more options
6. Instances -> Instance type: Choose an instance with atleast 2 gb of memory (E.g. t2.small)
7. Software -> Environment properties:
   - Name: `LTO_WALLET_PASSWORD`, Value: `Your wallet password`
   - Name: `LTO_WALLET_SEED` or `LTO_WALLET_SEED_BASE58`, Value: `Wallet Seed`

Now your node is should good to go!

## Documentation

You can find the API documentation on the url where your node is deployed.

## Running a node

### Testnet

If you wish to start testing with our testnet. Please create a wallet on: [https://testnet-wallet.lto.network](https://testnet-wallet.lto.network)

Send you address to our [support](mailto:support@legalthings.io) so we will deposit some LTO for testing on the testnet. Or request your tokens via our [telegram](https://t.me/joinchat/AJWQTUDKtDlsuGHVFb40eQ) channel.

To view all the transactions on the testnet you can check out our explorer on: [https://testnet-explorer.lto.network](https://testnet-explorer.lto.network)

### Mainnet

For mainnet you will have to buy tokens. You can then use these tokens by sending them to the wallet you will link to your node.

The wallet can be found here: [https://wallet.lto.network](https://wallet.lto.network)

The explorer can be found here: [https://explorer.lto.network](https://explorer.lto.network)
