from behave import *
from e2e.common.tools import *
from lto.transactions import Register


def register(context, user=None, key_type="ed25519", public_key=None, version=None):
    account = context.users[user] if user else ROOT_ACCOUNT
    register_account = {"key_type": key_type, "public_key": public_key} if public_key else generate_account(key_type)

    transaction = Register(register_account)
    transaction.version = version or Register.DEFAULT_VERSION
    transaction.sign_with(account)

    broadcast(context, transaction)


@when(u'{user} registers an account')
@when(u'{user} registers (v{version:d}) an account')
@when(u'{user} registers a {key_type} account')
@when(u'{user} registers (v{version:d}) a {key_type} account')
@when(u'{user} registers a {key_type} account with public key "{public_key}"')
@when(u'{user} registers (v{version:d}) a {key_type} account with public key "{public_key}"')
def step_impl(context, user, version=None, key_type='ed25519', public_key=None):
    register(context, user, key_type, public_key, version)


@when(u'{user} tries to register an account')
@when(u'{user} tries to register a {key_type} account with public key "{public_key}"')
def step_impl(context, user, key_type='ed25519', public_key=None):
    try:
        register(context, user, key_type, public_key)
    except:
        pass
