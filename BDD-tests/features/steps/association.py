import lto
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
        assert tools.is_associated(sender, recipient) is not False
    except:
        tools.funds_for_transaction(sender, lto.Association.DEFAULT_LEASE_FEE)
        tools.association(sender, recipient, type)
        assert tools.is_associated(sender, recipient) is not False


@given('{sender} has an association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash):
    try:
        assert tools.is_associated(sender, recipient) is not False
    except:
        tools.funds_for_transaction(sender, lto.Association.DEFAULT_LEASE_FEE)
        tools.association(sender, recipient, type, hash)
        assert tools.is_associated(sender, recipient) is not False


@when('{sender} make an association with {recipient} of type {type:d}')
@when('{sender} make an association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    tools.association(sender, recipient, type, version=version)


@when('{sender} revoke the association with {recipient} of type {type:d}')
@when('{sender} revoke the association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    tools.revoke_association(sender, recipient, type, version=version)


@when('{sender} revoke the association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash):
    tools.revoke_association(sender, recipient, type, hash)


@then('{sender} is associated with {recipient}')
def step_impl(context, sender, recipient):
    assert tools.is_associated(sender, recipient) is not False


@then('{sender} is not associated with {recipient}')
def step_impl(context, sender, recipient):
    assert tools.is_associated(sender, recipient) is False
