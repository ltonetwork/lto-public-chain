from behave import *
from e2e.common.tools import convert_balance
from e2e.common.tools import NODE
from e2e.common.tools import poll_tx
from lto.transactions.mass_transfer import MassTransfer


def process_input(context, transfers):
    transfer_list = []
    for transfer in transfers:
        transfer_list.append({'recipient': context.users[transfer[0]].address,
                              'amount': convert_balance(transfer[1])})
    return transfer_list


def mass_transfer(context, transfers, sender, version=None):
    sender = context.users[sender]
    transaction = MassTransfer(process_input(context, transfers))
    transaction.version = version or MassTransfer.DEFAULT_VERSION
    transaction.sign_with(sender)
    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(context, tx.id)
        context.last_tx_success = True
        return tx
    except:
        context.last_tx_success = False
        raise


@when(u'{sender} tries to do a mass-transfer of {amount1} lto to {receiver1} and {amount2} lto to {receiver2}')
def step_impl(context, sender, amount1, receiver1, amount2, receiver2):
    transfers = [[receiver1, amount1], [receiver2, amount2]]
    try:
        mass_transfer(context, transfers, sender)
    except:
        pass


@when(u'{sender} does a mass-transfer of {amount1} lto to {receiver1} and {amount2} lto to {receiver2}')
@when(u'{sender} does a mass-transfer (v{version:d}) of {amount1} lto to {receiver1} and {amount2} lto to {receiver2}')
def step_impl(context, sender, amount1, receiver1, amount2, receiver2, version=None):
    transfers = [[receiver1, amount1], [receiver2, amount2]]
    mass_transfer(context, transfers, sender, version)
