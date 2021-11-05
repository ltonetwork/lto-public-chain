from behave import *
import tools
import pytest


@when('{user} anchors {hash}')
def step_impl(context, user, hash):
    tools.anchor(user, hash)

@when('{user} tries to anchor')
def step_impl(context, user):
    try:
        tools.anchor(user)
    except:
        pass

@then('There is an anchor transaction with hash {hash} signed by {Alice}')
def step_impl(context, hash, user):
    pass