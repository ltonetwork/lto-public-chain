from behave import *
from e2e.common import tools


@when(u'{user} anchors {hash} sponsored by {sponsor}')
def step_impl(context, user, hash, sponsor):
    tools.anchor(user, hash, sponsor)
