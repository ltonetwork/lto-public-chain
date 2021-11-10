import LTO
from behave import *
import tools


@when(u'{sender} tries to make an association with {recipient}')
def step_impl(context, sender, recipient):
    try:
        tools.association(sender, recipient)
    except:
        pass


@given('{sender} is not associated with {recipient}')
def step_impl(context, sender, recipient):
    try:
        assert tools.isAssociated(sender, recipient) is False
    except:
        tools.fundsForTransaction(sender, LTO.RevokeAssociation.DEFAULT_LEASE_FEE)
        assTypeList = tools.isAssociated(sender, recipient)
        for typeHash in assTypeList:
            tools.revokeAssociation(sender, recipient, typeHash)
        assert tools.isAssociated(sender, recipient) is False


@given('{sender} is associated with {recipient}')
def step_impl(context, sender, recipient):
    try:
        assert tools.isAssociated(sender, recipient) is not False
    except:
        tools.fundsForTransaction(sender, LTO.Association.DEFAULT_LEASE_FEE)
        tools.association(sender, recipient)
        assert tools.isAssociated(sender, recipient) is not False


@when('{sender} make an association with {recipient}')
def step_impl(context, sender, recipient):
    tools.association(sender, recipient)


@when('{sender} revoke the association with {recipient}')
def step_impl(context, sender, recipient):
    assTypeList = tools.isAssociated(sender, recipient)
    for typeHash in assTypeList:
        tools.revokeAssociation(sender, recipient, typeHash)


@then('{sender} is associated with {recipient}')
def step_impl(context, sender, recipient):
    assert tools.isAssociated(sender, recipient) is not False


@then('{sender} is not associated with {recipient}')
def step_impl(context, sender, recipient):
    assert tools.isAssociated(sender, recipient) is False
