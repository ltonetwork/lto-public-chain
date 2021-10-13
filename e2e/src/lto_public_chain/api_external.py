import subprocess
import time

import requests
import polling

import config
import http_requests
from LTO import PyCLTO

pl = PyCLTO('T')
#pl.NODE = "https://testnet.lto.network"
#pl.setChain("testnet")

def create_account(base58_seed):
    return pl.Address(seed=base58_seed)

def cancel_lease(address, lease_id):
    sender_account = create_account(address.seed)
    return sender_account.leaseCancel(lease_id)

def get_tx_polled(id):
    return polling.poll(
        lambda: pl.tx(id),
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
