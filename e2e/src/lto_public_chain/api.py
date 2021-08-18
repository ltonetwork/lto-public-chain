import string
import polling
import random

import config
import http_requests
import PyCLTO

pl = PyCLTO.PyCLTO()
pl.NODE = config.node_url
pl.setChain("testnet")

def create_wallet():
    return http_requests.post("/addresses", "")

def create_account(base58_seed):
    return pl.Address(seed=base58_seed)

def transfer(sender, recipient, amount, attachment=''):
    sender_account = create_account(sender.seed)
    return sender_account.sendLTO(recipient, amount, attachment)

def invoke_association(sender, party, anchor, association_type=1):
    sender_account = create_account(sender.seed)
    return sender_account.invokeAssociation(party, association_type, anchor)

def revoke_association(sender, party, anchor, association_type=1):
    sender_account = create_account(sender.seed)
    return sender_account.revokeAssociation(party, association_type, anchor)

def list_associations(address):
    return http_requests.get("/associations/status/{}".format(address))

def set_script(sender, script):
    sender_account = create_account(sender.seed)
    return sender_account.setScript(script)

def get_tx(id):
    return polling.poll(
        lambda: pl.tx(id),
        check_success=lambda response: 'id' in response,
        step=1,
        poll_forever=True
    )

def get_height():
    return pl.height()
