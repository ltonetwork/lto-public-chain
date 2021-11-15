import LTO
from behave import *
import tools


@given('{user1} is not leasing to {user2}')
def step_impl(context, user1, user2):
    try:
        assert tools.is_leasing(user1, user2) is False
    except:
        tools.funds_for_transaction(user1, LTO.CancelLease.DEFAULT_CANCEL_LEASE_FEE)
        tools.cancel_lease(user1, user2)


@when('{user1} tries to cancel the lease to {user2}')
def step_impl(context, user1, user2):
    try:
        tools.cancel_lease(user1, user2)
    except:
        tools.last_transaction_success = False


@given('{user1} is leasing {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    amount = tools.convert_balance(amount)
    try:
        assert tools.is_leasing(user1, user2, amount) is True
    except:
        tools.funds_for_transaction(user1, LTO.Lease.DEFAULT_LEASE_FEE + amount)
        tools.lease(user1, user2, amount)


@when('{user1} leases {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    amount = tools.convert_balance(amount)
    tools.lease(user1, user2, amount)


@when('{user1} cancel the lease to {user2}')
def step_impl(context, user1, user2):
    tools.cancel_lease(user1, user2)


@when('{user1} tries to lease {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    try:
        tools.lease(user1, user2, amount)
    except:
        pass



@then('{user1} is leasing {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    amount = tools.convert_balance(amount)
    assert tools.is_leasing(user1, user2, amount) is True


@then('{user1} is not leasing to {user2}')
def step_impl(context, user1, user2):
    assert tools.is_leasing(user1, user2) is False
