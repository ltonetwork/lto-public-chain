from behave import *
from e2e.common.tools import broadcast, NODE, funds_for_transaction, cast_boolean_or_int
from lto.binary import Binary
from lto.transactions import Association, RevokeAssociation
from e2e.steps.generic import wait


def association(context, sender, recipient, type, subject=None, version=4, data=None):
    sender = context.users[sender]
    recipient = context.users[recipient]
    
    transaction = Association(recipient.address, type, subject=Binary(subject or '', 'utf-8'), data=data)
    transaction.version = version or Association.DEFAULT_VERSION
    transaction.sign_with(sender)

    broadcast(context, transaction)


def is_associated(context, sender, recipient):
    sender = context.users[sender]
    recipient = context.users[recipient]

    result = NODE.request(endpoint='/associations/status/{}'.format(sender.address))
    list_outgoing = result['outgoing']

    return list(filter(lambda assoc: assoc['recipient'] == recipient.address, list_outgoing))


def has_association(context, sender, recipient, subject):
    return next(filter(lambda assoc: assoc.subject == subject, is_associated(context, sender, recipient)))


def revoke_association(context, user1, user2, type, subject=None, version=None):
    user1 = context.users[user1]
    user2 = context.users[user2]

    transaction = RevokeAssociation(user2.address, type, subject=Binary(subject or '', 'utf-8'))
    transaction.version = version or RevokeAssociation.DEFAULT_VERSION
    transaction.sign_with(user1)

    broadcast(context, transaction)


def data_value(assoc, key):
    entry = next(filter(lambda e: e['key'] == key, assoc['data']))
    return entry['value'] if entry else None


@given('{sender} has an association with {recipient} of type {type:d}')
@given('{sender} has an association with {recipient} of type {type:d} and subject {subject}')
def step_impl(context, sender, recipient, type, subject=None):
    if not is_associated(context, sender, recipient):
        funds_for_transaction(context, sender, Association.BASE_FEE)
        association(context, sender, recipient, type, subject)
        assert is_associated(context, sender, recipient), 'Failed to issue association'


@given('{sender} does not have an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    for assoc in is_associated(context, sender, recipient):
        funds_for_transaction(context, sender, RevokeAssociation.DEFAULT_FEE)
        revoke_association(context, sender, recipient, type, assoc.hash)

    assert not is_associated(context, sender, recipient), 'Failed to revoke association'


@when('{sender} issues an association with {recipient} of type {type:d}')
@when('{sender} issues an association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    association(context, sender, recipient, type, version=version)


@when('{sender} revokes the association with {recipient} of type {type:d}')
@when('{sender} revokes the association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    revoke_association(context, sender, recipient, type, version=version)


@when('{sender} revokes the association with {recipient} of type {type:d} and subject {subject}')
def step_impl(context, sender, recipient, type, subject):
    revoke_association(context, sender, recipient, type, subject)


@when(u'{sender} tries to issue an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    try:
        association(context, sender, recipient, type)
    except:
        pass


@when(u'{sender} tries to revoke an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    try:
        revoke_association(context, sender, recipient, type)
    except:
        pass


@then('{sender} is associated with {recipient}')
def step_impl(context, sender, recipient):
    assocs = is_associated(context, sender, recipient)
    assert assocs, '{} is not associated with {}'.format(context.users[sender].address, context.users[recipient].address)


@then('{sender} is not associated with {recipient}')
def step_impl(context, sender, recipient):
    value = is_associated(context, sender, recipient)
    assert not value, f'{value}'


@given(u'{sender} has an association with {recipient} that has data "{key}" with value {value}')
@given(u'{sender} has an association with {recipient} that has data "{key}" with value "{str}"')
def step_impl(context, sender, recipient, key, str=None, value=None):
    funds_for_transaction(context, sender, Association.BASE_FEE + Association.VAR_FEE)
    association(context, sender, recipient, 1, data={key: str or cast_boolean_or_int(value)}, version=4)

@when(u'{sender} issues an association with {recipient} that sets data "{key}" to {value}')
@when(u'{sender} issues an association with {recipient} that sets data "{key}" to "{str}"')
def step_impl(context, sender, recipient, key, str=None, value=None):
    association(context, sender, recipient, 1, data={key: str or cast_boolean_or_int(value)}, version=4)


@then('{sender} has an association with {recipient} that has data "{key}" with value {value}')
@then('{sender} has an association with {recipient} that has data "{key}" with value "{str}"')
def step_impl(context, sender, recipient, key, str=None, value=None):
    assocs = is_associated(context, sender, recipient)
    assert assocs, '{} is not associated with {}'.format(context.users[sender].address, context.users[recipient].address)

    value = str or cast_boolean_or_int(value)
    assert any(data_value(assoc, key) == value for assoc in assocs), "No association with given data"
