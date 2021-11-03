from LTO.Accounts.AccountFactoryED25519 import AccountED25519 as AccountFactory
from LTO.PublicNode import PublicNode
from LTO.Transactions.Transfer import Transfer
from LTO.Transactions.Anchor import Anchor
import random
import polling
import requests
import hashlib

CHAIN_ID = 'T'
URL = 'https://testnet.lto.network'
NODE = PublicNode(URL)
ROOT_SEED = 'fragile because fox snap picnic mean art observe vicious program chicken purse text hidden chest'
TRANSFER_FEE = 100000000


def getBalance(seed):
    account = AccountFactory(CHAIN_ID).createFromSeed(getSeed(seed))
    return NODE.balance(account.address)


def pollTx(id):
    return polling.poll(
        lambda: requests.get('%s%s' % (URL, ('/transactions/info/%s' % id)), headers='').json(),
        check_success=lambda response: 'id' in response,
        step=1,
        timeout=180
    )

def transferTo(recipient=ROOT_SEED, amount=0, sender=ROOT_SEED):
    recipientAccount = AccountFactory(CHAIN_ID).createFromSeed(getSeed(recipient))
    senderAccount    = AccountFactory(CHAIN_ID).createFromSeed(getSeed(sender))
    transaction = Transfer(recipientAccount.address, amount)
    transaction.signWith(senderAccount)
    return(transaction.broadcastTo(NODE))

def anchor(seed=ROOT_SEED):
    account = AccountFactory(CHAIN_ID).createFromSeed(getSeed(seed))
    randomString = ''.join(random.choice('qwertyuioplkjhgfds') for _ in range(6))
    hash = hashlib.sha256(randomString.encode('utf-8')).hexdigest()
    transaction = Anchor(hash)
    transaction.signWith(account)
    return(transaction.broadcastTo(NODE))

def getSeed(name):
    if len(name) >= 15:
        return name
    else:
        while len(name) < 15:
            name = name + "a"
        return name