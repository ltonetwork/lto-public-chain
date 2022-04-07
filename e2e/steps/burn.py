from behave import *
from e2e.common.tools import *
from lto.transactions import Burn


def burn(context, user, amount):
    account = context.users[user]

    transaction = Burn(amount)
    transaction.sign_with(account)

    broadcast(context, transaction)


@when("{user} burns {amount} LTO")
def step_impl(context, user, amount):
    burn(context, user, convert_balance(amount))
