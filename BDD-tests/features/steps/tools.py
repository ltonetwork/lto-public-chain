from LTO.Accounts.AccountFactoryED25519 import AccountED25519 as AccountFactory
from LTO.PublicNode import PublicNode
from LTO.Transactions.Transfer import Transfer
from LTO.Transactions.Anchor import Anchor
from LTO.Transactions.Sponsorship import Sponsorship
from LTO.Transactions.CancelSponsorship import CancelSponsorship
from LTO.Transactions.Lease import Lease
from LTO.Transactions.CancelLease import CancelLease
from LTO.Transactions.MassTransfer import MassTransfer
from LTO.Transactions.RevokeAssociation import RevokeAssociation
from LTO.Transactions.Association import Association
from LTO import crypto
import base58
import random
import polling
import requests
import hashlib

CHAIN_ID = 'T'
URL = 'https://testnet.lto.network'
NODE = PublicNode(URL)
ROOT_SEED = 'fragile because fox snap picnic mean art observe vicious program chicken purse text hidden chest'
ROOT_ACCOUNT = AccountFactory(CHAIN_ID).createFromSeed(ROOT_SEED)
lastTransactionSuccess = None
USERS = {}

def generateAccount():
    return AccountFactory(CHAIN_ID).create()

def getBalance(user):
    return NODE.balance(USERS[user].address)

def fundsForTransaction(user, txFee):
    balance = getBalance(user)
    if txFee > balance:
        transferTo(user, txFee - balance)


def pollTx(id):
    return polling.poll(
        lambda: requests.get('%s%s' % (URL, ('/transactions/info/%s' % id)), headers='').json(),
        check_success=lambda response: 'id' in response,
        step=1,
        timeout=180
    )


def transferTo(recipient="", amount=0, sender=""):
    global lastTransactionSuccess

    if not recipient:
        recipientAccount = ROOT_ACCOUNT
    else:
        recipientAccount = USERS[recipient]

    if not sender:
        senderAccount = ROOT_ACCOUNT
    else:
        senderAccount = USERS[sender]

    transaction = Transfer(recipientAccount.address, amount)
    transaction.signWith(senderAccount)
    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise



def anchor(user="", hash=""):
    global lastTransactionSuccess

    if not user:
        account = ROOT_ACCOUNT
    else:
        account = USERS[user]

    if not hash:
        hash = ''.join(random.choice('qwertyuioplkjhgfds') for _ in range(6))
    transaction = Anchor(encodeHash(hash))
    transaction.signWith(account)

    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise



def isLastTransactionSuccessful():
    return lastTransactionSuccess


def getSeed(name):
    if len(name) >= 15:
        return name
    else:
        while len(name) < 15:
            name = name + "a"
        return name


def convertBalance(balance):
    return int(float(balance) * 100000000)


def encodeHash(hash):
    return hashlib.sha256(hash.encode('utf-8')).hexdigest()


def isSponsoring(account1, account2):
    account1 = USERS[account1]
    account2 = USERS[account2]
    return account1.address in NODE.sponsorshipList(account2.address)['sponsor']


def isLeasing(account1, account2):
    account1 = USERS[account1]
    account2 = USERS[account2]
    leasList = NODE.leaseList(account1.address)
    for lease in leasList:
        if lease['recipient'] == account2.address:
            return True
    return False


def getLeaseId(account1, account2):
    leasList = NODE.leaseList(account1.address)
    for lease in leasList:
        if lease['recipient'] == account2.address:
            return lease['id']
    raise Exception("No Lease Id Found")


def sponsor(sponsored, sponsoring):
    global lastTransactionSuccess

    sponsored = USERS[sponsored]
    sponsoring = USERS[sponsoring]
    transaction = Sponsorship(sponsored.address)
    transaction.signWith(sponsoring)

    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise


def cancelSponsorship(sponsored, sponsoring):
    global lastTransactionSuccess
    sponsored = USERS[sponsored]
    sponsoring = USERS[sponsoring]
    transaction = CancelSponsorship(sponsored.address)
    transaction.signWith(sponsoring)
    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise

def cancelLease(account1, account2):
    global lastTransactionSuccess

    account1 = USERS[account1]
    account2 = USERS[account2]

    leaseId = getLeaseId(account1, account2)
    transaction = CancelLease(leaseId)
    transaction.signWith(account1)
    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise


def lease(account1, account2, amount=100000000):
    global lastTransactionSuccess

    amount = int(amount)
    account1 = USERS[account1]
    account2 = USERS[account2]

    transaction = Lease(recipient=account2.address, amount=amount)
    transaction.signWith(account1)
    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise

def processInput(transfers):
    transferLsit = []
    for transfer in transfers:
        transferLsit.append({'recipient': USERS[transfer[0]].address,
                             'amount': convertBalance(transfer[1])})
    return transferLsit

def massTransfer(transfers, sender):
    global lastTransactionSuccess
    sender = USERS[sender]
    transaction = MassTransfer(processInput(transfers))
    transaction.signWith(sender)
    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise

def isAssociated(user1, user2):
    user1 = USERS[user1]
    user2 = USERS[user2]

    listOutgoing = NODE.wrapper(api='/associations/status/{}'.format(user1.address))['outgoing']
    assType = []
    for association in listOutgoing:
        if 'revokeTransactionId' not in association and association['party'] == user2.address:
            assType.append([association['associationType'], association['hash']])
    if not assType:
        return False
    else:
        return assType

def revokeAssociation(user1, user2, typeHash):
    global lastTransactionSuccess

    user1 = USERS[user1]
    user2 = USERS[user2]

    type = typeHash[0]
    hash = crypto.bytes2str(base58.b58decode(typeHash[1]))

    transaction = RevokeAssociation(recipient=user2.address, associationType=type, anchor=hash)
    transaction.signWith(user1)

    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise

def randomTypeAndHash():
    type = ''.join(random.choice('123456789') for _ in range(2))
    hash = ''.join(random.choice('qwertyuiopasdfghjklzxcvbnm') for _ in range(9))
    return [int(type), hash]

def association(user1, user2):
    global lastTransactionSuccess

    user1 = USERS[user1]
    user2 = USERS[user2]

    type, hash = randomTypeAndHash()

    transaction = Association(user2.address, associationType=type, anchor=hash)
    transaction.signWith(user1)

    try:
        tx = transaction.broadcastTo(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise



