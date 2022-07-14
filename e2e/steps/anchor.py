from behave import *
from e2e.common.tools import ROOT_ACCOUNT, NODE, encode_hash, broadcast
from lto.transactions import Anchor
from lto.crypto import encode
from os import urandom


def anchor(context, user, hash=None, sponsor=None, version=None):
    account = context.users[user]

    transaction = Anchor(encode_hash(hash) if hash else urandom(32))
    transaction.version = version or Anchor.DEFAULT_VERSION
    transaction.sign_with(account)

    if sponsor:
        sponsor_account = context.users[sponsor]
        transaction.sponsor_with(sponsor_account)

    broadcast(context, transaction)


@when(u'{user} anchors "{hash}"')
@when(u'{user} anchors (v{version:d}) "{hash}"')
@when(u'{user} anchors "{hash}" sponsored by {sponsor}')
def step_impl(context, user, version=None, hash='', sponsor=None):
    anchor(context, user, hash, sponsor=sponsor, version=version)


@when('{user} tries to anchor')
@when('{user} tries to anchor "{hash}"')
@when('{user} tries to anchor "{hash}" sponsored by {sponsor}')
def step_impl(context, user, hash='', sponsor=None):
    try:
        anchor(context, user, hash, sponsor)
    except:
        pass


@then('There is an anchor transaction with hash "{hash}" signed by {user}')
def step_impl(context, hash, user):
    digest = encode(encode_hash(hash), 'base58')
    txs = NODE.transactions(context.users[user], 'anchor')
    anchors = [anchor.base58() for tx in txs for anchor in tx.anchors]
    assert digest in anchors, 'Anchor tx with hash "{}" not found'.format(hash)


@when(u'{user} anchors {number:d} hashes')
def step_impl(context, user, number):
    account = context.users[user]
    hashes = [urandom(32) for i in range(0, number)]

    transaction = Anchor(*hashes)
    transaction.sign_with(account)
    broadcast(context, transaction)
