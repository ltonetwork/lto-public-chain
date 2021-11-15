from LTO.Accounts.account_factory_ed25519 import AccountFactoryED25519 as AccountFactory
from LTO.public_node import PublicNode
from LTO.Transactions.transfer import Transfer
from LTO.Transactions.anchor import Anchor
from LTO.Transactions.sponsorship import Sponsorship
from LTO.Transactions.cancel_sponsorship import CancelSponsorship
from LTO.Transactions.lease import Lease
from LTO.Transactions.cancel_lease import CancelLease
from LTO.Transactions.mass_transfer import MassTransfer
from LTO.Transactions.revoke_association import RevokeAssociation
from LTO.Transactions.association import Association
from LTO import crypto
import base58
import random
import polling
import requests
import hashlib

CHAIN_ID = 'T'
URL2 = 'https://testnet.lto.network'
URL = 'http://116.203.167.231:6869'
NODE = PublicNode(URL)
ROOT_SEED = 'fragile because fox snap picnic mean art observe vicious program chicken purse text hidden chest'
ROOT_ACCOUNT = AccountFactory(CHAIN_ID).create_from_seed(ROOT_SEED)
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
    transaction.sign_with(senderAccount)
    try:
        tx = transaction.broadcast_to(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise



def anchor(user="", hash="", sponsor=""):
    global lastTransactionSuccess

    if not user:
        account = ROOT_ACCOUNT
    else:
        account = USERS[user]

    if not hash:
        hash = ''.join(random.choice('qwertyuioplkjhgfds') for _ in range(6))
    transaction = Anchor(encodeHash(hash))
    transaction.sign_with(account)

    if sponsor:
        sponsorAccount = USERS[sponsor]
        transaction.sponsor_with(sponsorAccount)

    try:
        tx = transaction.broadcast_to(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise



def isLastTransactionSuccessful():
    return lastTransactionSuccess


def convertBalance(balance):
    return int(float(balance) * 100000000)


def encodeHash(hash):
    return hashlib.sha256(hash.encode('utf-8')).hexdigest()


def isSponsoring(account1, account2):
    account1 = USERS[account1]
    account2 = USERS[account2]
    return account1.address in NODE.sponsorship_list(account2.address)['sponsor']


def isLeasing(account1, account2, amount=""):
    account1 = USERS[account1]
    account2 = USERS[account2]
    leasList = NODE.lease_list(account1.address)
    for lease in leasList:
        if lease['recipient'] == account2.address:
            if amount:
                if lease['amount'] == amount:
                    return True
            else:
                return True
    return False


def getLeaseId(account1, account2):
    leasList = NODE.lease_list(account1.address)
    for lease in leasList:
        if lease['recipient'] == account2.address:
            return lease['id']
    raise Exception("No Lease Id Found")


def sponsor(sponsored, sponsoring):
    global lastTransactionSuccess

    sponsored = USERS[sponsored]
    sponsoring = USERS[sponsoring]
    transaction = Sponsorship(sponsored.address)
    transaction.sign_with(sponsoring)

    try:
        tx = transaction.broadcast_to(NODE)
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
    transaction.sign_with(sponsoring)
    try:
        tx = transaction.broadcast_to(NODE)
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
    transaction.sign_with(account1)
    try:
        tx = transaction.broadcast_to(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise


def lease(account1, account2, amount=""):
    global lastTransactionSuccess

    if not amount:
        amount = 100000000
    amount = int(amount)
    account1 = USERS[account1]
    account2 = USERS[account2]

    transaction = Lease(recipient=account2.address, amount=amount)
    transaction.sign_with(account1)
    try:
        tx = transaction.broadcast_to(NODE)
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
    transaction.sign_with(sender)
    try:
        tx = transaction.broadcast_to(NODE)
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

def revokeAssociation(user1, user2, type, hash = ""):
    global lastTransactionSuccess

    user1 = USERS[user1]
    user2 = USERS[user2]

    transaction = RevokeAssociation(recipient=user2.address, association_type=type, anchor=hash)
    transaction.sign_with(user1)

    try:
        tx = transaction.broadcast_to(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise

def association(user1, user2, type, hash=""):
    global lastTransactionSuccess

    user1 = USERS[user1]
    user2 = USERS[user2]
    transaction = Association(user2.address, association_type=type, anchor=hash)
    transaction.sign_with(user1)

    try:
        tx = transaction.broadcast_to(NODE)
        pollTx(tx.id)
        lastTransactionSuccess = True
        return tx
    except:
        lastTransactionSuccess = False
        raise



