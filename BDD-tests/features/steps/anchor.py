from behave import *
import tools
import pytest

TRANSFER_FEE = 100000000


@given('"{user}" has "{balance}" lto')
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


@given('"{user1}" "{value}" sponsoring "{user2}"')
def step_impl(context, user1, value, user2):
    if value == 'is':
        try:
            assert tools.isSponsoring(user1, user2) == True
        except:
            sponsorship = tools.sponsor(user2, user1)
            assert sponsorship.id == tools.pollTx(sponsorship.id)["id"]
            assert tools.isSponsoring(user1, user2) == True
    else:
        try:
            assert tools.isSponsoring(user1, user2) == False
        except:
            cancelSponsorship = tools.cancelSponsorship(user2, user1)
            assert cancelSponsorship.id == tools.pollTx(cancelSponsorship.id)["id"]
            assert tools.isSponsoring(user1, user2) == False


@when('"{user1}" sponsor "{user2}"')
def step_impl(context, user1, user2):
    sponsorship = tools.sponsor(user2, user1)
    assert sponsorship.id == tools.pollTx(sponsorship.id)["id"]


@when('"{user}" make an anchor transaction')
def step_impl(context, user):
    transaction = tools.anchor(user)
    assert transaction.id == tools.pollTx(transaction.id)["id"]


@when('"{sender}" transfer "{balance}" lto to "{recipient}"')
def step_impl(context, sender, balance, recipient):
    balance = tools.convertBalance(balance)
    transaction = tools.transferTo(recipient=recipient, amount=balance, sender=sender)
    assert transaction.id == tools.pollTx(transaction.id)["id"]


@when('"{user1}" cancel the sponsorship for "{user2}"')
def step_impl(context, user1, user2):
    cancelSponsorship = tools.cancelSponsorship(user2, user1)
    assert cancelSponsorship.id == tools.pollTx(cancelSponsorship.id)["id"]


@then('"{user}" has "{balance}" lto')
def step_impl(context, user, balance):
    balance = tools.convertBalance(balance)
    assert tools.getBalance(user) == balance


@then('"{user}" "{transaction}" transaction fails')
def step_impl(context, user, transaction):
    with pytest.raises(Exception):
        getattr(tools, transaction)(seed=user)


@then('"{user1}" "{value}" sponsoring "{user2}"')
def step_impl(context, user1, value, user2):
    if value == 'is':
        assert tools.isSponsoring(user1, user2) is True
    else:
        assert tools.isSponsoring(user1, user2) is False
