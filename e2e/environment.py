from e2e.common import node
from behave.model_core import Status
from e2e.common.tools import get_balance


def before_all(context):
    context.started_node = False
    if not node.is_node_up():
        node.start_node()
        context.started_node = True
        assert node.is_node_up(30), "Unable to connect to node"


def after_all(context):
    if context.started_node:
        node.stop_node()


def before_feature(context, feature):
    context.users = {}
    context.tx_ids = []
    context.last_tx_success = None


def after_scenario(context, scenario):
    if scenario.status == Status.failed:
        print_users(context.users)
        print_txs(context.tx_ids)


def print_users(users):
    if users:
        print('      users:')
    for user, account in users.items():
        print(f'        \033[1m\33[90m{user: <8}\33[0m\33[90m {account.address}\33[0m\33[90m {get_balance(account.address)}\33[0m')


def print_txs(tx_ids):
    if tx_ids:
        print('      Transactions:')
    for txid in tx_ids:
        print(f'        \33[90m{txid}\33[0m')
