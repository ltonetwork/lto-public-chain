from behave import *
import tools
import pytest

TRANSFER_FEE = 100000000

@given('"{user}" has "{balance}" lto')
def step_impl(context, user, balance):
    balance = int(float(balance) * 100000000)
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


@when('"{user}" make an anchor transaction')
def step_impl(context, user):
    transaction = tools.anchor(user)
    assert transaction.id == tools.pollTx(transaction.id)["id"]


@then('"{user}" has "{balance}" lto')
def step_impl(context, user, balance):
    balance = int(float(balance) * 100000000)
    assert tools.getBalance(user) == balance



@then('"{user}" Anchor transaction fails')
def step_impl(context, user):
    with pytest.raises(Exception):
        tools.anchor(seed=user)
