import lto
from behave import *
from e2e.common.tools import *


@when(u'{sender} tries to make an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    try:
        association(sender, recipient, type)
    except:
        pass


@given('{sender} has an association with {recipient} of type {type:d}')
@given('{sender} has an association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash=""):
    if not is_associated(sender, recipient):
        funds_for_transaction(sender, lto.Association.DEFAULT_LEASE_FEE)
        association(sender, recipient, type, hash)
        assert_that(is_associated(sender, recipient))


@when('{sender} make an association with {recipient} of type {type:d}')
@when('{sender} make an association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    association(sender, recipient, type, version=version)


@when('{sender} revoke the association with {recipient} of type {type:d}')
@when('{sender} revoke the association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    revoke_association(sender, recipient, type, version=version)


@when('{sender} revoke the association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash):
    revoke_association(sender, recipient, type, hash)


@then('{sender} is associated with {recipient}')
def step_impl(context, sender, recipient):
    assert_that(is_associated(sender, recipient))


@then('{sender} is not associated with {recipient}')
def step_impl(context, sender, recipient):
    assert_that(not is_associated(sender, recipient))
