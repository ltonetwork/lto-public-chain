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

def set_script(sender, script):
    sender_account = create_account(sender.seed)
    return sender_account.setScript(script)

def get_tx(id):
    return pl.tx(id)

def get_height():
    return pl.height()
