import LTO
from behave import *
import tools

TRANSFER_FEE = LTO.Transfer.DEFAULT_TX_FEE

@given('{user} has a new account')
def step_impl(context, user):
    tools.USERS.update({user: tools.generate_account()})

@given('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = tools.convert_balance(balance)
    userBalance = tools.get_balance(user)
    try:
        assert userBalance == balance
    except:
        if userBalance < balance:
            transfer = tools.transfer_to(recipient=user, amount=balance - userBalance)

        else:
            if userBalance - balance <= TRANSFER_FEE:
                transfer = tools.transfer_to(recipient=user, amount=TRANSFER_FEE)
            userBalance = tools.get_balance(user)
            transfer = tools.transfer_to(amount=userBalance - (balance + TRANSFER_FEE), sender=user)
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
