import lto
from behave import *
from e2e.common.tools import *

TRANSFER_FEE = lto.Transfer.DEFAULT_TX_FEE


@given('{user} has a new account')
def step_impl(context, user):
    USERS.update({user: generate_account()})


@given('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = convert_balance(balance)
    user_balance = get_balance(user)
    
    if user_balance < balance:
        transfer = transfer_to(recipient=user, amount=balance - user_balance)
    elif user_balance > balance:
        if user_balance - balance <= TRANSFER_FEE:
            transfer = transfer_to(recipient=user, amount=TRANSFER_FEE)
        user_balance = get_balance(user)
        transfer = transfer_to(amount=user_balance - (balance + TRANSFER_FEE), sender=user)

    assert_equals(get_balance(user), balance)

@then('{user} has {balance} lto')
def step_impl(context, user, balance):
    balance = convert_balance(balance)
    assert_equals(get_balance(user), balance)


@then('the transaction fails')
def step_impl(context):
    assert_that(not last_transaction_success)


@then('the transaction is successful')
def step_impl(context):
    assert_that(last_transaction_success)

@given('wait')
@given('wait {seconds} seconds')
@when('wait')
@when('wait {seconds} seconds')
@then('wait')
@then('wait {seconds} seconds')
def wait(context, seconds=3):
    sleep(seconds)

