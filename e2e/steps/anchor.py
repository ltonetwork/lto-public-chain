from behave import *
from e2e.common.tools import *
from lto.transactions.anchor import Anchor
import random


def anchor(context, user="", hash="", sponsor="", version=None):
    if not user:
        account = ROOT_ACCOUNT
    else:
        account = context.users[user]

    if not hash:
        hash = ''.join(random.choice('qwertyuiopasdfghjklzxcvbnm') for _ in range(6))

    transaction = Anchor(encode_hash(hash))
    transaction.version = version or Anchor.DEFAULT_VERSION
    transaction.sign_with(account)

    if sponsor:
        sponsor_account = context.users[sponsor]
        transaction.sponsor_with(sponsor_account)

    try:
        tx = transaction.broadcast_to(NODE)
        poll_tx(context, tx.id)
        context.last_tx_success = True
        return tx
    except:
        context.last_tx_success = False
        raise


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
    pass
