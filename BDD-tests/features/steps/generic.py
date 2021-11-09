import LTO
from behave import *
import tools

TRANSFER_FEE = LTO.Transfer.DEFAULT_TX_FEE

@given('{user} has a new account')
def step_impl(context, user):
    tools.USERS.update({user: tools.generateAccount()})

@given('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = tools.convertBalance(balance)
    userBalance = tools.getBalance(user)
    try:
        assert userBalance == balance
    except:
        if userBalance < balance:
            transfer = tools.transferTo(recipient=user, amount=balance - userBalance)

        else:
            if userBalance - balance <= TRANSFER_FEE:
                transfer = tools.transferTo(recipient=user, amount=TRANSFER_FEE)
            userBalance = tools.getBalance(user)
            transfer = tools.transferTo(amount=userBalance - (balance + TRANSFER_FEE), sender=user)
    assert tools.getBalance(user) == balance


@then('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = tools.convertBalance(balance)
    assert tools.getBalance(user) == balance


@then('The transaction fails')
def step_impl(context):
    assert tools.lastTransactionSuccess == False


@then('The transaction is successful')
def step_impl(context):
    assert tools.lastTransactionSuccess == True
