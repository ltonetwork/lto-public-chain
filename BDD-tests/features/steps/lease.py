import LTO
from behave import *
import tools


@given('{user1} is not leasing to {user2}')
def step_impl(context, user1, user2):
    try:
        assert tools.isLeasing(user1, user2) is False
    except:
        tools.fundsForTransaction(user1, LTO.CancelLease.DEFAULT_CANCEL_LEASE_FEE)
        tools.cancelLease(user1, user2)


@when('{user1} tries to cancel the lease to {user2}')
def step_impl(context, user1, user2):
    try:
        tools.cancelLease(user1, user2)
    except:
        tools.lastTransactionSuccess = False


@given('{user1} is leasing to {user2}')
def step_impl(context, user1, user2):
    try:
        assert tools.isLeasing(user1, user2) == True
    except:
        tools.fundsForTransaction(user1, LTO.Lease.DEFAULT_LEASE_FEE + 100000000)
        tools.lease(user1, user2)


@when('{user1} leases {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    amount = tools.convertBalance(amount)
    tools.lease(user1, user2, amount)


@when('{user1} cancel the lease to {user2}')
def step_impl(context, user1, user2):
    tools.cancelLease(user1, user2)


@when('{user1} tries to lease {amount} lto to {user2}')
def step_impl(context, user1, amount, user2):
    try:
        tools.lease(user1, user2, amount)
    except:
        pass



@then('{user1} is leasing to {user2}')
def step_impl(context, user1, user2):
    assert tools.isLeasing(user1, user2) is True


@then('{user1} is not leasing to {user2}')
def step_impl(context, user1, user2):
    assert tools.isLeasing(user1, user2) is False
