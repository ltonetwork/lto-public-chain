from behave import *
from e2e.common.tools import *
from lto.transactions import Data


def set_data(context, user=None, data=None, version=None):
    account = context.users[user] if user else ROOT_ACCOUNT

    transaction = Data(data or {})
    transaction.version = version or Data.DEFAULT_VERSION
    transaction.sign_with(account)

    broadcast(context, transaction)


@when(u'{user} sets data "{key}" to {value}')
@when(u'{user} sets data "{key}" to "{str}"')
@when(u'{user} sets data (v{version:d}) "{key}" to {value}')
@when(u'{user} sets data (v{version:d}) "{key}" to "{str}"')
def step_impl(context, user, key, str=None, value=None, version=None):
    set_data(context, user, {key: str or cast_boolean_or_int(value)}, version)


@when('{user} tries to set data "{key}" to {value}')
@when('{user} tries to set data "{key}" to "{str}"')
def step_impl(context, user, key, str=None, value=None):
    try:
        set_data(context, user, key, str or cast_boolean_or_int(value))
    except:
        pass


@then('{user} has data "{key}" with value {value}')
@then('{user} has data "{key}" with value "{str}"')
def step_impl(context, user, key, str=None, value=None):
    data = get_data(context.users[user])
    assert key in data, 'key "{}" is not set'.format(key)
    assert data[key] == str or cast_boolean_or_int(value)

