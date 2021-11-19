import lto
from behave import *
from e2e.utils import tools

TRANSFER_FEE = lto.Transfer.DEFAULT_TX_FEE


@given('{user} has a new account')
def step_impl(context, user):
    tools.USERS.update({user: tools.generate_account()})


@given('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = tools.convert_balance(balance)
    user_balance = tools.get_balance(user)
    try:
        assert user_balance == balance
    except:
        if user_balance < balance:
            transfer = tools.transfer_to(recipient=user, amount=balance - user_balance)

        else:
            if user_balance - balance <= TRANSFER_FEE:
                transfer = tools.transfer_to(recipient=user, amount=TRANSFER_FEE)
            user_balance = tools.get_balance(user)
            transfer = tools.transfer_to(amount=user_balance - (balance + TRANSFER_FEE), sender=user)
    assert tools.get_balance(user) == balance


@then('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = tools.convert_balance(balance)
    assert tools.get_balance(user) == balance


@then('The transaction fails')
def step_impl(context):
    assert tools.last_transaction_success == False


@then('The transaction is successful')
def step_impl(context):
    assert tools.last_transaction_success == True
