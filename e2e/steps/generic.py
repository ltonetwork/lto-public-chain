from behave import *
from e2e.common.tools import *
from time import sleep


@given('{user} has a new account')
@given('{user} has a new {key_type} account')
def step_impl(context, user, key_type='ed25519'):
    context.users.update({user: generate_account(key_type)})


@given('{user} has an account with {balance} lto')
@given('{user} has an {key_type} account with {balance} lto')
def step_impl(context, user, key_type='ed25519', balance=0):
    context.execute_steps(u'''
        Given {user} has a new {key_type} account
        Given {user} has {balance} lto
    '''.format(user=user, key_type=key_type, balance=balance))


@then('the transaction fails')
def step_impl(context):
    assert not context.last_tx_success, "transaction was successful"


@then('the transaction is successful')
def step_impl(context):
    assert context.last_tx_success, "transaction failed"
