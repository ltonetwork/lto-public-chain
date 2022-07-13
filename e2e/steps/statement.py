from behave import *
from e2e.common.tools import broadcast, cast_boolean_or_int, NODE
from lto.binary import Binary
from lto.transactions import Statement
from e2e.steps.generic import wait


def statement(context, sender, type, recipient=None, subject=None, data=None, version=3):
    sender = context.users[sender]
    recipient = context.users[recipient].address if recipient else None
    
    transaction = Statement(type, recipient, subject=Binary(subject or '', 'utf-8'), data=data)
    transaction.version = version or Statement.DEFAULT_VERSION
    transaction.sign_with(sender)

    broadcast(context, transaction)


def data_value(data_entries, key):
    entry = next(filter(lambda e: e.key == key, data_entries), None)
    return entry.value if entry else None


@when('{sender} makes a statement of type {type:d}')
@when('{sender} makes a statement of type {type:d} with recipient {recipient} and subject {subject}')
@when('{sender} makes a statement (v{version:d}) of type {type:d}')
def step_impl(context, sender, type, recipient=None, subject=None, version=None):
    statement(context, sender, type, recipient, subject, version=version)


@when(u'{sender} tries to make a statement')
def step_impl(context, sender):
    try:
        statement(context, sender, 1)
    except:
        pass


@when(u'{sender} makes a statement with data "{key}" is {value}')
@when(u'{sender} makes a statement with data "{key}" is "{str}"')
def step_impl(context, sender, key, str=None, value=None):
    statement(context, sender, 1, data={key: str or cast_boolean_or_int(value)})


@then('There is an statement transaction with data "{key}" is {value} signed by {user}')
@then('There is an statement transaction with data "{key}" is "{str}" signed by {user}')
def step_impl(context, user, key, value=None, str=None):
    txs = NODE.transactions(context.users[user], 'statement')
    value = str or cast_boolean_or_int(value)

    found = next((v for v in [data_value(tx.data, key) for tx in txs] if v is not None), None)
    assert found is not None, 'no statement with key {}'.format(key)
    assert found == value, 'statement data {} is {} instead of {}'.format(key, found, value)
