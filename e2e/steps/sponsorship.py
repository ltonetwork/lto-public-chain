from behave import *
from e2e.common.tools import funds_for_transaction
from e2e.common.tools import NODE
from e2e.common.tools import broadcast
from lto.transactions import Sponsorship, CancelSponsorship


def is_sponsoring(context, user1, user2):
    account1 = context.users[user1]
    account2 = context.users[user2]
    sponsorships = NODE.sponsorship_list(account2.address)
    if account1.address in sponsorships['sponsor']:
        return sponsorships
    else:
        return None


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
        funds_for_transaction(context, user1, CancelSponsorship.DEFAULT_FEE)
        cancel_sponsorship(context, user2, user1)


@given('{user1} is sponsoring {user2}')
def step_impl(context, user1, user2):
    if not is_sponsoring(context, user1, user2):
        funds_for_transaction(context, user1, Sponsorship.DEFAULT_FEE)
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
    value = is_sponsoring(context, user1, user2)
    assert value, f'{user1} is not sponsoring {user2}'


@then('{user1} is not sponsoring {user2}')
def step_impl(context, user1, user2):
    value = is_sponsoring(context, user1, user2)
    assert not value, f'{value}'
