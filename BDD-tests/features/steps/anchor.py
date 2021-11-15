from behave import *
import tools
import pytest


@when(u'{user} anchors "{hash}"')
@when(u'{user} anchors (v{version:d}) "{hash}"')
def step_impl(context, user, version=None, hash=''):
    tools.anchor(user, hash, version=version)

@when('{user} tries to anchor')
def step_impl(context, user):
    try:
        tools.anchor(user)
    except:
        pass


@then('There is an anchor transaction with hash {hash} signed by {user}')
def step_impl(context, hash, user):
    pass



