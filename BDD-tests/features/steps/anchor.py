from behave import *
import tools
import pytest

TRANSFER_FEE = 100000000

@given('"{user}" has 100 lto')
def step_impl(context, user):
    aliceBalance = tools.getBalance(user)
    try:
        assert aliceBalance == 10000000000
    except:
        if aliceBalance < 10000000000:
            transfer = tools.transferTo(recipient=user, amount=10000000000 - aliceBalance)
            assert transfer.id == tools.pollTx(transfer.id)["id"]

        else:
            transfer = tools.transferTo(amount=aliceBalance - 10000000000, sender=user)
            assert transfer.id == tools.pollTx(transfer.id)["id"]
    assert tools.getBalance(user) == 10000000000


@when('"{user}" make an anchor transaction')
def step_impl(context, user):
    transaction = tools.anchor(user)
    assert transaction.id == tools.pollTx(transaction.id)["id"]


@then('"{user}" has 99.65 lto')
def step_impl(context, user):
    assert tools.getBalance(user) == 9965000000

@given('"{user}" has 0 lto')
def step_impl(context, user):
    aliceBalance = tools.getBalance(user)
    try:
        assert aliceBalance == 0
    except:
        transfer = tools.transferTo(amount=aliceBalance-TRANSFER_FEE, sender=user)
        print("Transfer ", transfer)
        assert transfer.id == tools.pollTx(transfer.id)["id"]
    assert tools.getBalance(user) == 0

@then('"{user}" Anchor transaction fails')
def step_impl(context, user):
    with pytest.raises(Exception):
        tools.anchor(seed=user)
