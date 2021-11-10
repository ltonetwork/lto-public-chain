import LTO
from behave import *
import tools


@when(u'{sender} tries to make an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    try:
        tools.association(sender, recipient, type)
    except:
        pass


@given('{sender} has an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    try:
        assert tools.isAssociated(sender, recipient) is not False
    except:
        tools.fundsForTransaction(sender, LTO.Association.DEFAULT_LEASE_FEE)
        tools.association(sender, recipient, type)
        assert tools.isAssociated(sender, recipient) is not False


@given('{sender} has an association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash):
    try:
        assert tools.isAssociated(sender, recipient) is not False
    except:
        tools.fundsForTransaction(sender, LTO.Association.DEFAULT_LEASE_FEE)
        tools.association(sender, recipient, type, hash)
        assert tools.isAssociated(sender, recipient) is not False

@when('{sender} make an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    tools.association(sender, recipient, type)


@when('{sender} revoke the association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    tools.revokeAssociation(sender, recipient, type)

@when('{sender} revoke the association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash):
    tools.revokeAssociation(sender, recipient, type, hash)

@then('{sender} is associated with {recipient}')
def step_impl(context, sender, recipient):
    assert tools.isAssociated(sender, recipient) is not False


@then('{sender} is not associated with {recipient}')
def step_impl(context, sender, recipient):
    assert tools.isAssociated(sender, recipient) is False
