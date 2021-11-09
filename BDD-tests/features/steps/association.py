import LTO
from behave import *
import tools


@when(u'{user1} tries to make an association with {user2}')
def step_impl(context, user1, user2):
    try:
        tools.association(user1, user2)
    except:
        pass


@given('{user1} is not associated with {user2}')
def step_impl(context, user1, user2):
    try:
        assert tools.isAssociated(user1, user2) is False
    except:
        tools.fundsForTransaction(user1, LTO.RevokeAssociation.DEFAULT_LEASE_FEE)
        assTypeList = tools.isAssociated(user1, user2)
        for typeHash in assTypeList:
            tools.revokeAssociation(user1, user2, typeHash)
        assert tools.isAssociated(user1, user2) is False


@given('{user1} is associated with {user2}')
def step_impl(context, user1, user2):
    try:
        assert tools.isAssociated(user1, user2) is not False
    except:
        tools.fundsForTransaction(user1, LTO.Association.DEFAULT_LEASE_FEE)
        tools.association(user1, user2)
        assert tools.isAssociated(user1, user2) is not False


@when('{user1} make an association with {user2}')
def step_impl(context, user1, user2):
    tools.association(user1, user2)


@when('{user1} revoke the association with {user2}')
def step_impl(context, user1, user2):
    assTypeList = tools.isAssociated(user1, user2)
    for typeHash in assTypeList:
        tools.revokeAssociation(user1, user2, typeHash)


@then('{user1} is associated with {user2}')
def step_impl(context, user1, user2):
    assert tools.isAssociated(user1, user2) is not False


@then('{user1} is not associated with {user2}')
def step_impl(context, user1, user2):
    assert tools.isAssociated(user1, user2) is False
