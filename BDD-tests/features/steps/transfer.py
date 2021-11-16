from behave import *
import tools

@when('{sender} transfers {amount} lto to {recipient}')
@when('{sender} transfers (v{version:d}) {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient, version=None):
    amount = tools.convert_balance(amount)
    tools.transfer_to(recipient=recipient, amount=amount, sender=sender, version=version)


@when('{sender} tries to transfer {amount} lto to {recipient}')
def step_impl(context, sender, amount, recipient):
    amount = tools.convert_balance(amount)
    try:
        tools.transfer_to(recipient=recipient, amount=amount, sender=sender)
    except:
        pass
