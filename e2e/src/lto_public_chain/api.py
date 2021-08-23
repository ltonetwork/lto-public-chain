import subprocess
import time

import requests
import polling

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

def lease(lessor, lessee, amount):
    sender_account = create_account(lessor.seed)
    return sender_account.lease(lessee, amount)

def cancel_lease(lessor, lease_id):
    sender_account = create_account(lessor.seed)
    return sender_account.leaseCancel(lease_id)

def list_active_leases(address):
    return http_requests.get("/leasing/active/{}".format(address))

def set_script(sender, script):
    sender_account = create_account(sender.seed)
    return sender_account.setScript(script)

def mass_transfer(sender, transfers, attachment='e2etests'):
    sender_account = create_account(sender.seed)
    return sender_account.massTransferLTO(transfers, attachment)

def sponsor(sponsor, party):
    sender_account = create_account(sponsor.seed)
    return sender_account.sponsor(party)

def cancel_sponsor(sponsor, party):
    sender_account = create_account(sponsor.seed)
    return sender_account.cancelSponsor(party)

def get_tx_polled(id):
    return polling.poll(
        lambda: pl.tx(id),
        check_success=lambda response: 'id' in response,
        step=1,
        poll_forever=True
    )

def get_height():
    return pl.height()

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
