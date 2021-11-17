from lto.accounts.account_factory_ed25519 import AccountFactoryED25519 as AccountFactory
from lto.public_node import PublicNode
from lto.transactions.transfer import Transfer
from lto.transactions.anchor import Anchor
from lto.transactions.sponsorship import Sponsorship
from lto.transactions.cancel_sponsorship import CancelSponsorship
from lto.transactions.lease import Lease
from lto.transactions.cancel_lease import CancelLease
from lto.transactions.mass_transfer import MassTransfer
from lto.transactions.revoke_association import RevokeAssociation
from lto.transactions.association import Association
from lto import crypto
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
last_transaction_success = None
USERS = {}


def generate_account():
    return AccountFactory(CHAIN_ID).create()


def get_balance(user):
    return NODE.balance(USERS[user].address)


def funds_for_transaction(user, txFee):
    balance = get_balance(user)
    if txFee > balance:
        transfer_to(user, txFee - balance)


def poll_tx(id):
    return polling.poll(
        lambda: requests.get('%s%s' % (URL, ('/transactions/info/%s' % id)), headers='').json(),
        check_success=lambda response: 'id' in response,
        step=1,
        timeout=180
    )


def transfer_to(recipient="", amount=0, sender="", version=None):
    global last_transaction_success

    if not recipient:
        recipient_account = ROOT_ACCOUNT
    else:
        recipient_account = USERS[recipient]

    if not sender:
        sender_account = ROOT_ACCOUNT
    else:
        sender_account = USERS[sender]

    transaction = Transfer(recipient_account.address, amount)
    transaction.version = version or Transfer.DEFAULT_VERSION
    transaction.sign_with(sender_account)
    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def anchor(user="", hash="", sponsor="", version=None):
    global last_transaction_success

    if not user:
        account = ROOT_ACCOUNT
    else:
        account = USERS[user]

    if not hash:
        hash = ''.join(random.choice('qwertyuiopasdfghjklzxcvbnm') for _ in range(6))
    transaction = Anchor(encode_hahs(hash))
    transaction.version = version or Anchor.DEFAULT_VERSION
    transaction.sign_with(account)

    if sponsor:
        sponsor_account = USERS[sponsor]
        transaction.sponsor_with(sponsor_account)

    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def convert_balance(balance):
    return int(float(balance) * 100000000)


def encode_hahs(hash):
    return hashlib.sha256(hash.encode('utf-8')).hexdigest()


def is_sponsoring(account1, account2):
    account1 = USERS[account1]
    account2 = USERS[account2]
    return account1.address in NODE.sponsorship_list(account2.address)['sponsor']


def is_leasing(account1, account2, amount=""):
    account1 = USERS[account1]
    account2 = USERS[account2]
    lease_list = NODE.lease_list(account1.address)
    for lease in lease_list:
        if lease['recipient'] == account2.address:
            if amount:
                if lease['amount'] == amount:
                    return True
            else:
                return True
    return False


def get_lease_id(account1, account2):
    lease_list = NODE.lease_list(account1.address)
    for lease in lease_list:
        if lease['recipient'] == account2.address:
            return lease['id']
    raise Exception("No Lease Id Found")


def sponsor(sponsored, sponsoring, version=None):
    global last_transaction_success

    sponsored = USERS[sponsored]
    sponsoring = USERS[sponsoring]
    transaction = Sponsorship(sponsored.address)
    transaction.version = version or Sponsorship.DEFAULT_VERSION
    transaction.sign_with(sponsoring)

    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def cancel_sponsorship(sponsored, sponsoring, version=None):
    global last_transaction_success
    sponsored = USERS[sponsored]
    sponsoring = USERS[sponsoring]
    transaction = CancelSponsorship(sponsored.address)
    transaction.version = version or CancelSponsorship.DEFAULT_VERSION
    transaction.sign_with(sponsoring)
    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def cancel_lease(account1, account2, version=None):
    global last_transaction_success

    account1 = USERS[account1]
    account2 = USERS[account2]

    lease_id = get_lease_id(account1, account2)
    transaction = CancelLease(lease_id)
    transaction.version = version or CancelLease.DEFAULT_VERSION
    transaction.sign_with(account1)
    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def lease(account1, account2, amount="", version=None):
    global last_transaction_success

    if not amount:
        amount = 100000000
    amount = int(amount)
    account1 = USERS[account1]
    account2 = USERS[account2]

    transaction = Lease(recipient=account2.address, amount=amount)
    transaction.version = version or Lease.DEFAULT_VERSION
    transaction.sign_with(account1)
    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def processInput(transfers):
    transfer_list = []
    for transfer in transfers:
        transfer_list.append({'recipient': USERS[transfer[0]].address,
                              'amount': convert_balance(transfer[1])})
    return transfer_list


def mass_transfer(transfers, sender, version=None):
    global last_transaction_success
    sender = USERS[sender]
    transaction = MassTransfer(processInput(transfers))
    transaction.version = version or MassTransfer.DEFAULT_VERSION
    transaction.sign_with(sender)
    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def is_associated(user1, user2):
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


def revoke_association(user1, user2, type, hash="", version=None):
    global last_transaction_success

    user1 = USERS[user1]
    user2 = USERS[user2]

    transaction = RevokeAssociation(recipient=user2.address, association_type=type, anchor=hash)
    transaction.version = version or RevokeAssociation.DEFAULT_VERSION
    transaction.sign_with(user1)

    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise


def association(user1, user2, type, hash="", version=None):
    global last_transaction_success

    user1 = USERS[user1]
    user2 = USERS[user2]
    transaction = Association(user2.address, association_type=type, anchor=hash)
    transaction.version = version or Association.DEFAULT_VERSION
    transaction.sign_with(user1)

    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(tx.id)
        last_transaction_success = True
        return tx
    except:
        last_transaction_success = False
        raise
