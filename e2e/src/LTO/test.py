from LTO.AccountFactory import AccountFactory
from LTO.Accounts.AccountFactoryED25519 import AccountED25519
factory = AccountED25519('T')
account = factory.createFromSeed('cool strike recall mother true topic road bright nature dilemma glide shift return mesh strategy')
from LTO.Transactions.Anchor import Anchor
from LTO.Transactions.Transfer import Transfer
from LTO.PublicNode import PublicNode
hash = '01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b'
#transaction = Transfer(recipient='3N6MFpSbbzTozDcfkTUT5zZ2sNbJKFyRtRj', amount = 100000000)
transaction = Anchor(hash)
transaction.signWith(account)
transaction.broadcastTo(PublicNode('https://testnet.lto.network'))

