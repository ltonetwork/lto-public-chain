from lto.accounts.account_factory_ed25519 import AccountFactoryED25519 as AccountFactory
from lto.public_node import PublicNode
import polling
import requests
import hashlib
from e2e.common import config

CHAIN_ID = config.chain_id
URL = config.node_url
NODE = PublicNode(URL)
ROOT_SEED = config.seed
ROOT_ACCOUNT = AccountFactory(CHAIN_ID).create_from_seed(ROOT_SEED)


# last_tx_success = None
# transactions = []
# users = {}


def assert_equals(value1, value2):
    assert value1 == value2, f'{value1} is not {value2}'


def assert_contains(value, set):
    assert value in set, f'{value} is not in {set}'


def generate_account():
    return AccountFactory(CHAIN_ID).create()


def get_balance(address):
    return NODE.balance(address)


def funds_for_transaction(context, user, tx_fee):
    account = context.users[user]
    balance = get_balance(account.address)
    if tx_fee > balance:
        from e2e.steps.transfer import transfer_to
        transfer_to(context, user, tx_fee - balance)


def poll_tx(id):
    response = polling.poll(
        lambda: requests.get('%s%s' % (URL, ('/transactions/info/%s' % id)), headers='').json(),
        check_success=lambda response: 'id' in response,
        step=0.1,
        timeout=180
        # timeout=5
    )
    return response


def convert_balance(balance):
    return int(float(balance) * 100000000)


def encode_hash(hash):
    return hashlib.sha256(hash.encode('utf-8')).hexdigest()
