'''from behave import *
import tools
import pytest

TRANSFER_FEE = 100000000

@given('Bob has 0 lto')
def step_impl(context):
    def step_impl(context):
        aliceBalance = tools.getBalance('Bob')
        try:
            assert aliceBalance == 0
        except:
            transfer = tools.transferTo(recipient="", amount=aliceBalance - TRANSFER_FEE, sender='Bob')
            assert transfer.id == tools.pollTx(transfer.id)["id"]
        assert tools.getBalance('Bob') == 0
'''