import subprocess
import time

import requests
import polling

import config
import http_requests
from LTO import PyCLTO, AccountED25519, CancelLease, PublicNode

CHAIN_ID = 'T'
pl = PyCLTO(CHAIN_ID)
node = PublicNode(pl.NODE.url)

def create_account(base58_seed):
    account = AccountED25519(CHAIN_ID).createFromSeed(base58_seed)
    return account.address

def cancel_lease(lessor, lease_id):
    transaction = CancelLease(lease_id)
    transaction.signWith(lessor)
    return transaction.broadcastTo(node)

def get_tx_polled(id):
    return polling.poll(
        lambda: http_requests.get_id(id),
        check_success=lambda response: 'id' in response,
        step=1,
        timeout=60
    )


def is_lease_missing(address, lease_id):
    try:
        polling.poll(
            lambda: http_requests.get_from_url("https://testnet.lto.network/leasing/active/{}".format(address)),
            check_success=lambda response: (not any(r['id'] == lease_id for r in response.json())),
            ignore_exceptions=(requests.exceptions.ConnectionError),
            step=1,
            timeout=60
        )
        return True
    except polling.TimeoutException:
        return False
