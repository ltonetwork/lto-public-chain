from behave import *
from e2e.common.tools import ROOT_ACCOUNT, convert_balance, get_balance, broadcast, assert_equals
from lto.transactions import Transfer


def transfer_to(context, recipient="", amount=0, sender="", version=None):
    if not recipient:
        recipient_account = ROOT_ACCOUNT
    else:
        recipient_account = context.users[recipient]

    if not sender:
        sender_account = ROOT_ACCOUNT
    else:
        sender_account = context.users[sender]

    transaction = Transfer(recipient_account.address, amount)
    transaction.version = version or Transfer.DEFAULT_VERSION
    transaction.sign_with(sender_account)
    broadcast(context, transaction)


@given('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = convert_balance(balance)
    user_balance = get_balance(context.users[user].address)

    if user_balance < balance:
        transfer_to(context, recipient=user, amount=balance - user_balance)
    elif user_balance > balance:
        if user_balance - balance <= Transfer.BASE_FEE:
            transfer_to(context, recipient=user, amount=Transfer.BASE_FEE)
        user_balance = get_balance(context.users[user].address)
        transfer_to(context, amount=user_balance - (balance + Transfer.BASE_FEE), sender=user)

    assert_equals(get_balance(context.users[user].address), balance)


@then('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = convert_balance(balance)
    assert_equals(get_balance(context.users[user].address), balance)


@when('{sender} transfers {amount} lto to {recipient}')
@when('{sender} transfers (v{version:d}) {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient, version=None):
    amount = convert_balance(amount)
    transfer_to(context, recipient=recipient, amount=amount, sender=sender, version=version)


@when('{sender} tries to transfer {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient):
    amount = convert_balance(amount)
    try:
        transfer_to(context, recipient=recipient, amount=amount, sender=sender)
    except:
        pass
