from behave import *
from e2e.common.tools import *
from lto.transactions.data import Data


def set_data(context, user=None, data=None, version=None):
    account = context.users[user] if user else ROOT_ACCOUNT

    transaction = Data(data or {})
    transaction.version = version or 3
    transaction.tx_fee = 110000000
    transaction.sign_with(account)

    broadcast(context, transaction)


@when(u'{user} sets data "{key}" to "{value}"')
@when(u'{user} sets data (v{version}) "{key}" to "{value}"')
def step_impl(context, user, key, value, version=None):
    set_data(context, user, {key: value}, version)

@when(u'{user} sets data "{key}" to {value}')
@when(u'{user} sets data (v{version}) "{key}" to {value}')
def step_impl(context, user, key, value, version=None):
    if value.lower() == 'true':
        value = True
    elif value.lower() == 'false':
        value = False
    else:
        value = int(value)
    set_data(context, user, {key: value}, version)

@when('{user} tries to set data "{key}" to "{value}"')
def step_impl(context, user, key, value):
    try:
        set_data(context, user, key, value)
    except:
        pass
