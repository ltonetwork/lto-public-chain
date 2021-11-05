from behave import *
import tools
import pytest

TRANSFER_FEE = 100000000


@given('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = tools.convertBalance(balance)
    aliceBalance = tools.getBalance(user)
    try:
        assert aliceBalance == balance
    except:
        if aliceBalance < balance:
            transfer = tools.transferTo(recipient=user, amount=balance - aliceBalance)
            assert transfer.id == tools.pollTx(transfer.id)["id"]

        else:
            transfer = tools.transferTo(amount=aliceBalance - (balance + TRANSFER_FEE), sender=user)
            assert transfer.id == tools.pollTx(transfer.id)["id"]
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


"""@then('"{user}" "{transaction}" transaction fails')
def step_impl(context, user, transaction):
    with pytest.raises(Exception):
        getattr(tools, islas)(seed=user)"""
