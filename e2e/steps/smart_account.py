from behave import *
from e2e.common.tools import NODE, funds_for_transaction, broadcast
from lto.transactions import SetScript


def set_script(context, user, script):
    transaction = NODE.compile(script)
    transaction.sign_with(context.users[user])
    broadcast(context, transaction)

def clear_script(context, user):
    transaction = SetScript()
    transaction.sign_with(context.users[user])
    broadcast(context, transaction)

def has_script(context, user):
    response = NODE.request('/addresses/scriptInfo/{}'.format(context.users[user].address))
    return 'script' in response

@given("{user} has a smart account with script")
def step_impl(context, user):
    funds_for_transaction(context, user, SetScript.DEFAULT_FEE)
    set_script(context, user, context.text)
    assert has_script(context, user), "No script set for account {}".format(context.users[user].address)

@when("{user} creates a smart account with script")
def step_impl(context, user):
    set_script(context, user, context.text)

@when("{user} removes the account script")
def step_impl(context, user):
    clear_script(context, user)

@when("{user} tries to remove the account script")
def step_impl(context, user):
    try:
        clear_script(context, user)
    except:
        pass

@then("{user} has a smart account")
def step_impl(context, user):
    assert has_script(context, user), "No script set for account {}".format(context.users[user].address)

@then("{user} doesn't have a smart account")
def step_impl(context, user):
    assert not has_script(context, user), "Account {} has a script".format(context.users[user].address)
