import lto
from behave import *
from e2e.common.tools import broadcast
from e2e.common.tools import NODE
from e2e.common.tools import funds_for_transaction
from lto.transactions.association import Association
from lto.transactions.revoke_association import RevokeAssociation


def association(context, user1, user2, type, hash="", version=None):
    user1 = context.users[user1]
    user2 = context.users[user2]
    transaction = Association(user2.address, association_type=type, anchor=hash)
    transaction.version = version or Association.DEFAULT_VERSION
    transaction.sign_with(user1)

    broadcast(context, transaction)


def is_associated(context, user1, user2):
    user1 = context.users[user1]
    user2 = context.users[user2]

    listOutgoing = NODE.wrapper(api='/associations/status/{}'.format(user1.address))['outgoing']
    assType = []
    for association in listOutgoing:
        if 'revokeTransactionId' not in association and association['party'] == user2.address:
            assType.append([association['associationType'], association['hash']])
    if not assType:
        return False
    else:
        return assType


def revoke_association(context, user1, user2, type, hash="", version=None):
    user1 = context.users[user1]
    user2 = context.users[user2]

    transaction = RevokeAssociation(recipient=user2.address, association_type=type, anchor=hash)
    transaction.version = version or RevokeAssociation.DEFAULT_VERSION
    transaction.sign_with(user1)

    broadcast(context, transaction)


@given('{sender} has an association with {recipient} of type {type:d}')
@given('{sender} has an association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash=""):
    if not is_associated(context, sender, recipient):
        funds_for_transaction(context, sender, lto.Association.DEFAULT_FEE)
        association(context, sender, recipient, type, hash)
        assert is_associated(context, sender, recipient), 'Failed to issue association'


@given('{sender} does not have an association with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type):
    if is_associated(context, sender, recipient):
        funds_for_transaction(sender, lto.RevokeAssociation.DEFAULT_FEE)
        revoke_association(sender, recipient, type, hash)
        assert revoke_association(context, sender, recipient), 'Failed to revoke association'


@when('{sender} issues an association with {recipient} of type {type:d}')
@when('{sender} issues an association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    association(context, sender, recipient, type, version=version)


@when('{sender} revokes the association with {recipient} of type {type:d}')
@when('{sender} revokes the association (v{version:d}) with {recipient} of type {type:d}')
def step_impl(context, sender, recipient, type, version=None):
    revoke_association(context, sender, recipient, type, version=version)


@when('{sender} revokes the association with {recipient} of type {type:d} and anchor {hash}')
def step_impl(context, sender, recipient, type, hash):
    revoke_association(context, sender, recipient, type, hash)


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
    assert is_associated(context, sender, recipient)


@then('{sender} is not associated with {recipient}')
def step_impl(context, sender, recipient):
    assert not is_associated(context, sender, recipient)
