import subprocess
import time

import requests
import polling

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
#pl.NODE = config.node_url
#pl.setChain("testnet")

def create_wallet():
    return http_requests.post("/addresses", "")


def create_account(base58_seed):
    account = AccountED25519(CHAIN_ID).createFromSeed(base58_seed)
    return account.address
    #return pl.Address(seed=base58_seed)

def transfer(sender, recipient, amount, attachment=''):
    sender_account = create_account(sender.seed)
    return sender_account.sendLTO(recipient, amount, attachment)

def transfer_v3(sender, recipient, amount, attachment=''):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sender.seed)
    return Transfer(recipient=recipient, amount=amount, attachment=attachment).signWith(sender_account)

def invoke_association(sender, party, anchor, association_type=1):
    sender_account = create_account(sender.seed)
    return sender_account.invokeAssociation(party, association_type, anchor)

def invoke_association_v3(sender, party, anchor, association_type=1):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sender.seed)
    return Association(party, association_type, anchor).signWith(sender_account)

def revoke_association(sender, party, anchor, association_type=1):
    sender_account = create_account(sender.seed)
    return sender_account.revokeAssociation(party, association_type, anchor)

def revoke_association_v3(sender, party, anchor, association_type=1):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sender.seed)
    return RevokeAssociation(party, association_type, anchor).signWith(sender_account)

def list_associations(address):
    return http_requests.get("/associations/status/{}".format(address))

def lease(lessor, lessee, amount):
    sender_account = create_account(lessor.seed)
    return sender_account.lease(lessee, amount)

def lease_v3(lessor, lessee, amount):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(lessor.seed)
    return Lease(lessee, amount).signWith(sender_account)

def cancel_lease(lessor, lease_id):
    sender_account = create_account(lessor.seed)
    return sender_account.leaseCancel(lease_id)

def cancel_lease_v3(lessor, lease_id):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(lessor.seed)
    return CancelLease(lease_id).signWith(sender_account)

def list_active_leases(address):
     return PublicNode(pl.NODE.url).leaseList(address)

def set_script(sender, script):
    sender_account = create_account(sender.seed)
    return sender_account.setScript(script)

def set_script_v3(sender, script):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sender.seed)
    return SetScript(script).signWith(sender_account)

def mass_transfer(sender, transfers, attachment='e2etests'):
    sender_account = create_account(sender.seed)
    return sender_account.massTransferLTO(transfers, attachment)

def mass_transfer_v3(sender, transfers, attachment='e2etests'):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sender.seed)
    return MassTransfer(transfers, attachment).signWith(sender_account)

def sponsor(sponsor, party):
    sender_account = create_account(sponsor.seed)
    return sender_account.sponsor(party)

def sponsor_v3(sponsor, recipient):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sponsor.seed)
    return Sponsorship(recipient).signWith(sender_account)

def cancel_sponsor(sponsor, party):
    sender_account = create_account(sponsor.seed)
    return sender_account.cancelSponsor(party)

def cancel_sponsor_v3(sponsor, recipient):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sponsor.seed)
    return CancelSponsorship(recipient).signWith(sender_account)

def anchor(sender, hash):
    sender_account = create_account(sender.seed)
    return sender_account.anchor(hash)

def anchor_v3(sender, hash):
    sender_account = AccountED25519(CHAIN_ID).createFromSeed(sender.seed)
    return Anchor(hash).signWith(sender_account)

def get_tx_polled(id):
    return polling.poll(
        lambda: PublicNode(pl.NODE.url).tx(id),
        check_success=lambda response: 'id' in response,
        step=1,
        poll_forever=True
    )

def get_height():
    return PublicNode(pl.NODE.url).height()

def get_address_balance(address):
    return PublicNode(pl.NODE.url).balance(address)

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
