from behave import *
import tools
import pytest



@when('{sender} transfers {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient):
    amount = tools.convertBalance(amount)
    transaction = tools.transferTo(recipient=recipient, amount=amount, sender=sender)
    assert transaction.id == tools.pollTx(transaction.id)["id"]


@when('{sender} tries to transfer {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient):
    try:
        tools.transferTo(recipient=recipient, amount=amount, sender=sender)
    except:
        pass