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
