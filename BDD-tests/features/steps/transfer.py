from behave import *
import tools


@when('{sender} transfers {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient):
    amount = tools.convertBalance(amount)
    tools.transferTo(recipient=recipient, amount=amount, sender=sender)


@when('{sender} tries to transfer {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient):
    amount = tools.convertBalance(amount)
    try:
        tools.transferTo(recipient=recipient, amount=amount, sender=sender)
    except:
        pass
