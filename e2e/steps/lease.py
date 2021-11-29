import lto
from behave import *
from e2e.common.tools import NODE
from e2e.common.tools import broadcast
from e2e.common.tools import convert_balance
from e2e.common.tools import funds_for_transaction
from lto.transactions.lease import Lease
from lto.transactions.cancel_lease import CancelLease


def is_leasing(context, account1, account2, amount=""):
    account1 = context.users[account1]
    account2 = context.users[account2]
    lease_list = NODE.lease_list(account1.address)
    for lease in lease_list:
        if lease['recipient'] == account2.address:
            if amount:
                if lease['amount'] == amount:
                    return True
            else:
                return True
    return False


def get_lease_id(context, account1, account2):
    lease_list = NODE.lease_list(account1.address)
    for lease in lease_list:
        if lease['recipient'] == account2.address:
            return lease['id']
    raise Exception("No Lease Id Found")


def cancel_lease(context, account1, account2, version=None):
    account1 = context.users[account1]
    account2 = context.users[account2]

    lease_id = get_lease_id(context, account1, account2)
    transaction = CancelLease(lease_id)
    transaction.version = version or CancelLease.DEFAULT_VERSION
    transaction.sign_with(account1)
    broadcast(context, transaction)


def lease(context, account1, account2, amount="", version=None):
    if not amount:
        amount = 100000000
    amount = int(amount)
    account1 = context.users[account1]
    account2 = context.users[account2]

    transaction = Lease(recipient=account2.address, amount=amount)
    transaction.version = version or Lease.DEFAULT_VERSION
    transaction.sign_with(account1)
    broadcast(context, transaction)


@given('{user1} is not leasing to {user2}')
def step_impl(context, user1, user2):
    try:
        assert not is_leasing(context, user1, user2), f'{user1} is leasing to {user2}'
    except:
        funds_for_transaction(context, user1, lto.CancelLease.DEFAULT_FEE)
        cancel_lease(context, user1, user2)


@when('{user1} tries to cancel the lease to {user2}')
def step_impl(context, user1, user2):
    try:
        cancel_lease(context, user1, user2)
    except:
        context.last_tx_success = False


@given('{user1} is leasing {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    amount = convert_balance(amount)
    try:
        assert is_leasing(context, user1, user2, amount), f'{user1} is not leasing to {user2}'
    except:
        funds_for_transaction(context, user1, lto.Lease.DEFAULT_FEE + amount)
        lease(context, user1, user2, amount)


@when('{user1} leases {amount} lto to {user2}')
@when('{user1} leases (v{version:d}) {amount} lto to {user2}')
def step_impl(context, user1, amount, user2, version=None):
    amount = convert_balance(amount)
    lease(context, user1, user2, amount, version)


@when('{user1} cancel the lease to {user2}')
@when('{user1} cancel the lease (v{version:d}) to {user2}')
def step_impl(context, user1, user2, version=None):
    cancel_lease(context, user1, user2, version)


@when('{user1} tries to lease {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    try:
        lease(context, user1, user2, amount)
    except:
        pass


@then('{user1} is leasing {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    amount = convert_balance(amount)
    assert is_leasing(context, user1, user2, amount), f'{user1} is not leasing to {user2}'


@then('{user1} is not leasing to {user2}')
def step_impl(context, user1, user2):
    assert not is_leasing(context, user1, user2), f'{user1} is leasing to {user2}'
