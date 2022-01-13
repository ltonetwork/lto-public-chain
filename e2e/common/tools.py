from lto.accounts import AccountFactoryED25519 as AccountFactory, AccountFactoryECDSA
from lto.public_node import PublicNode
from lto.transactions import Transfer
import polling
import requests
import hashlib
from e2e.common import config

CHAIN_ID = config.chain_id
URL = config.node_url
NODE = PublicNode(URL)
ROOT_SEED = config.seed
ROOT_ACCOUNT = AccountFactory(CHAIN_ID).create_from_seed(ROOT_SEED)


def assert_equals(value1, value2):
    assert value1 == value2, f'{value1} is not {value2}'


def generate_account(key_type='ed25519'):
    if key_type == 'ed25519':
        return AccountFactory(CHAIN_ID).create()
    else:
        return AccountFactoryECDSA(CHAIN_ID, key_type).create()


def get_balance(address):
    return NODE.balance(address)


def get_data(address):
    return NODE.data_of(address)


def funds_for_transaction(context, user, tx_fee):
    account = context.users[user]
    transaction = Transfer(account.address, tx_fee)
    transaction.sign_with(ROOT_ACCOUNT)
    broadcast(context, transaction)


def minimum_balance(context, user, amount):
    account = context.users[user]
    balance = get_balance(account.address)
    if balance < amount:
        transaction = Transfer(account.address, amount - balance)
        transaction.sign_with(ROOT_ACCOUNT)
        broadcast(context, transaction)


def poll_tx(context, id):
    context.tx_ids.append(id)
    response = polling.poll(
        lambda: requests.get('%s%s' % (URL, ('/transactions/info/%s' % id)), headers='').json(),
        check_success=lambda response: 'id' in response,
        step=0.1,
        timeout=180
    )
    return response


def broadcast(context, transaction):
    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(context, tx.id)
        context.last_tx_success = True
        return tx
    except:
        context.last_tx_success = False
        raise


def convert_balance(balance):
    return int(float(balance) * 100000000)


def encode_hash(hash):
    return hashlib.sha256(hash.encode('utf-8')).hexdigest()
