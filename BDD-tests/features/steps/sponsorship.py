from behave import *
import tools
import pytest



@given('"{user1}" "{value}" sponsoring "{user2}"')
def step_impl(context, user1, value, user2):
    if value == 'is':
        try:
            assert tools.isSponsoring(user1, user2) == True
        except:
            sponsorship = tools.sponsor(user2, user1)
            assert sponsorship.id == tools.pollTx(sponsorship.id)["id"]
            assert tools.isSponsoring(user1, user2) == True
    else:
        try:
            assert tools.isSponsoring(user1, user2) == False
        except:
            cancelSponsorship = tools.cancelSponsorship(user2, user1)
            assert cancelSponsorship.id == tools.pollTx(cancelSponsorship.id)["id"]
            assert tools.isSponsoring(user1, user2) == False


@when('"{user1}" sponsor "{user2}"')
def step_impl(context, user1, user2):
    sponsorship = tools.sponsor(user2, user1)
    assert sponsorship.id == tools.pollTx(sponsorship.id)["id"]

@when('"{user1}" cancel the sponsorship for "{user2}"')
def step_impl(context, user1, user2):
    cancelSponsorship = tools.cancelSponsorship(user2, user1)
    assert cancelSponsorship.id == tools.pollTx(cancelSponsorship.id)["id"]




@then('"{user1}" is sponsoring "{user2}"')
def step_impl(context, user1, value, user2):
    assert tools.isSponsoring(user1, user2) is True

@then('"{user1}" is not sponsoring "{user2}"')
def step_impl(context, user1, value, user2):
    assert tools.isSponsoring(user1, user2) is False



