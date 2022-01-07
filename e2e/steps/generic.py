import lto
from behave import *
from e2e.common.tools import *
from time import sleep


@given('{user} has a new account')
@given('{user} has a new {key_type} account')
def step_impl(context, user, key_type='ed25519'):
    context.users.update({user: generate_account(key_type)})


@then('the transaction fails')
def step_impl(context):
    assert not context.last_tx_success, "transaction was successful"


@then('the transaction is successful')
def step_impl(context):
    assert context.last_tx_success, "transaction failed"


@given('wait')
@given('wait {seconds} seconds')
@when('wait')
@when('wait {seconds} seconds')
@then('wait')
@then('wait {seconds} seconds')
def wait(context, seconds=5):
    sleep(seconds)
