import os

api_key = os.environ.get('LTO_API_KEY', "open")
node_url = "http://localhost:6869"
chain_id = 'Z'
seed = os.environ.get('LTO_WALLET_SEED', "root")
