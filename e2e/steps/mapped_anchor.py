from behave import *
from e2e.common.tools import NODE, encode_hash, broadcast
from lto.transactions import MappedAnchor


def mapped_anchor(context, user, key, value, version=None):
    account = context.users[user]

    transaction = MappedAnchor({encode_hash(key): encode_hash(value)})
    transaction.version = version or MappedAnchor.DEFAULT_VERSION
    transaction.sign_with(account)

    broadcast(context, transaction)


@when(u'{user} anchors key "{key}" and value "{value}"')
@when(u'{user} anchors (v{version:d}) key "{key}" and value "{value}"')
def step_impl(context, user, key, value, version=None):
    mapped_anchor(context, user, key, value, version=version)


@when('{user} tries to anchor key "{key}" and value "{value}"')
def step_impl(context, user, key, value):
    try:
        mapped_anchor(context, user, key, value)
    except:
        pass


@then('There is a mapped anchor transaction with key "{key}" and value "{value}" signed by {user}')
def step_impl(context, key, value, user):
    key_digest = encode_hash(key)
    value_digest = encode_hash(value)

    txs = NODE.transactions(context.users[user], 'mapped-anchor')

    print([tx.to_json() for tx in txs])

    found = next((v for v in [tx.anchors.get(key_digest) for tx in txs] if v is not None), None)
    assert found is not None, 'no anchor tx with key {}'.format(key)
    assert found == value_digest, 'anchor {} has value {} instead of {}'.format(key, found, value)
