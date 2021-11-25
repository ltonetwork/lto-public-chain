from behave import *
from e2e.common import tools


@when(u'{user} anchors "{hash}"')
@when(u'{user} anchors (v{version:d}) "{hash}"')
@when(u'{user} anchors "{hash}" sponsored by {sponsor}')
def step_impl(context, user, version=None, hash='', sponsor=None):
    tools.anchor(user, hash, sponsor=sponsor, version=version)


@when('{user} tries to anchor')
@when('{user} tries to anchor "{hash}"')
@when('{user} tries to anchor "{hash}" sponsored by {sponsor}')
def step_impl(context, user, hash='', sponsor=None):
    try:
        tools.anchor(user, hash, sponsor)
    except:
        pass


def step_impl(context, user, hash, sponsor):
    tools.anchor(user, hash, sponsor)
    

@then('There is an anchor transaction with hash "{hash}" signed by {user}')
def step_impl(context, hash, user):
    pass
