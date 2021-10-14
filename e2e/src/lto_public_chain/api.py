import subprocess
import time

import requests
import polling
import time

import config
import http_requests
from LTO import PyCLTO
from LTO.Accounts.AccountFactoryED25519 import AccountED25519
from LTO.Transactions.Transfer import Transfer
from LTO.Transactions.Association import Association
from LTO.Transactions.RevokeAssociation import RevokeAssociation
from LTO.Transactions.Lease import Lease
from LTO.Transactions.CancelLease import CancelLease
from LTO.PublicNode import PublicNode
from LTO.Transactions.SetScript import SetScript
from LTO.Transactions.MassTransfer import MassTransfer
from LTO.Transactions.Sponsorship import Sponsorship
from LTO.Transactions.CancelSponsorship import CancelSponsorship
from LTO.Transactions.Anchor import Anchor


CHAIN_ID = 'T'


pl = PyCLTO(CHAIN_ID)
node = PublicNode(pl.NODE.url)

def create_wallet():
    return http_requests.post("/addresses", "")


def create_account(base58_seed):
    return AccountED25519(CHAIN_ID).createFromSeed(base58_seed)


def transfer(sender, recipient, amount, attachment=''):
    transaction = Transfer(recipient=recipient, amount=amount, attachment=attachment)
    transaction.signWith(sender)
    return transaction.broadcastTo(node)


def invoke_association(sender, party, anchor, association_type=1):
    transaction = Association(party.address, association_type, anchor)
    transaction.signWith(sender)
    return transaction.broadcastTo(node)


def revoke_association(sender, party, anchor, association_type=1):
    transaction = RevokeAssociation(party.address, association_type, anchor)
    transaction.signWith(sender)
    return transaction.broadcastTo(node)

def list_associations(address):
    return http_requests.get("/associations/status/{}".format(address))



def lease(lessor, lessee, amount):
    transaction = Lease(lessee, amount)
    transaction.signWith(lessor)
    return transaction.broadcastTo(node)


def cancel_lease(lessor, lease_id):
    transaction = CancelLease(lease_id)
    transaction.signWith(lessor)
    return transaction.broadcastTo(node)

def list_active_leases(address):
     return node.leaseList(address)



def set_script(sender, script):
    transaction = SetScript(script)
    transaction.signWith(sender)
    return transaction.broadcastTo(node)



def mass_transfer(sender, transfers, attachment='e2etests'):
    transaction = MassTransfer(transfers, attachment)
    transaction.signWith(sender)
    return transaction.broadcastTo(node)


def sponsor(sponsor, recipient):
    transaction = Sponsorship(recipient)
    transaction.signWith(sponsor)
    return transaction.broadcastTo(node)


def cancel_sponsor(sponsor, recipient):
    transaction = CancelSponsorship(recipient)
    transaction.signWith(sponsor)
    return transaction.broadcastTo(node)


def anchor(sender, hash):
    transaction = Anchor(hash)
    transaction.signWith(sender)
    return transaction.broadcastTo(node)

def get_tx_polled(id):
    return polling.poll(
        lambda: (requests.get('%s%s' % ('https://testnet.lto.network', ('/transactions/info/%s' % id)), headers='')).json(),
        check_success=lambda response: 'id' in response,
        step=1,
        poll_forever=True
    )

def get_height():
    return node.height()

def get_address_balance(address):
    return http_requests.get("/addresses/balance/details/{}".format(address))

def shutdown_node():
    http_requests.post("/node/stop")

def start_node():
    subprocess.call("bin/run_public_node", shell=True)

def is_node_down():
    try:
        polling.poll(
            lambda: http_requests.get("/"),
            check_success=lambda response: response.status_code != 200,
            step=1,
            timeout=180
        )
        return False
    except requests.exceptions.ConnectionError:
        return True

def is_node_up():
    return polling.poll(
        lambda: http_requests.get("/").status_code == 200,
        step=1,
        ignore_exceptions=(requests.exceptions.ConnectionError),
        timeout=180
    )
