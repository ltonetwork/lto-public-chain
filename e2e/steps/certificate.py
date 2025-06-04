from behave import *
from e2e.common.tools import *
from lto.transactions import Certificate


def submit_certificate(context, user, file):
    account = context.users[user]
    with open(f"e2e/data/{file}", "rb") as f:
        cert_bytes = f.read()

    tx = Certificate(cert_bytes)
    tx.sign_with(account)
    broadcast(context, tx)


def has_certificate(context, user):
    address = context.users[user].address
    try:
        NODE.certificate(address)
        return True
    except Exception as e:
        if '404' in str(e):
            return False
        raise e


@given('{user} has a certificate')
def step_submit_cert(context, user):
    if not has_certificate(context, user):
        funds_for_transaction(context, sender, Certificate.BASE_FEE)
        submit_certificate(context, user, "valid-cert.pem")
        assert has_certificate(context, user), f"Failed to submit certificate for {user}"


@when('{user} submits certificate "{file}"')
def step_submit_cert(context, user, file):
    submit_certificate(context, user, file)


@when('{user} tries to submit certificate "{file}"')
def step_try_submit_cert(context, user, file):
    try:
        submit_certificate(context, user, file)
    except:
        pass


@when('{user} submits an empty certificate')
def step_impl(context, user):
    from lto.transactions import Certificate

    account = context.users[user]
    tx = Certificate(None)
    tx.sign_with(account)
    broadcast(context, tx)


@then('the certificate for {user} matches')
def step_check_certificate(context, user):
    address = context.users[user].address
    try:
        cert = NODE.certificate(address)
    except:
        assert False, f"No certificate found for {user}"

    for row in context.table:
        field, value = row.cells[0], row.cells[1]
        assert cert.get(field) == value, f'Expected {field} to be "{value}", but got "{cert.get(field)}"'


@then('there is no certificate for {user}')
def step_no_certificate(context, user):
    address = context.users[user].address
    try:
        NODE.certificate(address)
        assert False, f"Expected no certificate for {user}, but one was found"
    except Exception as e:
        assert '404' in str(e), f"Expected 404, got: {e}"

