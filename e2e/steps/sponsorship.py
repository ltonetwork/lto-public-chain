from behave import *
import lto
from e2e.common.tools import funds_for_transaction
from e2e.common.tools import NODE
from e2e.common.tools import broadcast
from lto.transactions.sponsorship import Sponsorship
from lto.transactions.cancel_sponsorship import CancelSponsorship


def is_sponsoring(context, account1, account2):
    account1 = context.users[account1]
    account2 = context.users[account2]
    return account1.address in NODE.sponsorship_list(account2.address)['sponsor']


def sponsor(context, sponsored, sponsoring, version=None):
    sponsored = context.users[sponsored]
    sponsoring = context.users[sponsoring]
    transaction = Sponsorship(sponsored.address)
    transaction.version = version or Sponsorship.DEFAULT_VERSION
    transaction.sign_with(sponsoring)
    broadcast(context, transaction)


def cancel_sponsorship(context, sponsored, sponsoring, version=None):
    sponsored = context.users[sponsored]
    sponsoring = context.users[sponsoring]
    transaction = CancelSponsorship(sponsored.address)
    transaction.version = version or CancelSponsorship.DEFAULT_VERSION
    transaction.sign_with(sponsoring)
    broadcast(context, transaction)


@given('{user1} is not sponsoring {user2}')
def step_impl(context, user1, user2):
    if is_sponsoring(context, user1, user2):
        funds_for_transaction(context, user1, lto.CancelSponsorship.DEFAULT_FEE)
        cancel_sponsorship(context, user2, user1)


@given('{user1} is sponsoring {user2}')
def step_impl(context, user1, user2):
    if not is_sponsoring(context, user1, user2):
        funds_for_transaction(context, user1, lto.Sponsorship.DEFAULT_FEE)
        sponsor(context, user2, user1)


@when('{user1} tries to sponsor {user2}')
def step_impl(context, user1, user2):
    try:
        sponsor(context, user2, user1)
    except:
        pass


@when('{user1} tries to cancel the sponsorship for {user2}')
def step_impl(context, user1, user2):
    try:
        cancel_sponsorship(context, user2, user1)
    except:
        pass


@when('{user1} sponsors {user2}')
@when('{user1} sponsors (v{version:d}) {user2}')
def step_impl(context, user1, user2, version=None):
    sponsor(context, user2, user1, version)


@when('{user1} cancels the sponsorship for {user2}')
@when('{user1} cancels the sponsorship (v{version:d}) for {user2}')
def step_impl(context, user1, user2, version=None):
    cancel_sponsorship(context, user2, user1, version)


@then('{user1} is sponsoring {user2}')
def step_impl(context, user1, user2):
    assert is_sponsoring(context, user1, user2), f'{user1} is not sponsoring {user2}'


@then('{user1} is not sponsoring {user2}')
def step_impl(context, user1, user2):
    assert not is_sponsoring(context, user1, user2), f'{user1} is sponsoring {user2}'
