import os
import os.path
import urllib.request
import sys
import requests
from pyhocon import ConfigFactory, HOCONConverter
import pywaves as pw
import base58
import string
import random
import math
from tqdm import tqdm
from shutil import copyfile
from pyblake2 import blake2b
from hashlib import sha256

network_names = ['MAINNET', 'TESTNET', 'CUSTOM']

NETWORK = os.environ.get('LTO_NETWORK')


def generate_password(size=12, chars=string.ascii_letters + string.digits):
    return ''.join(random.choice(chars) for i in range(size))


def nested_set(dic, keys, value):
    for key in keys[:-1]:
        dic = dic.setdefault(key, {})
    dic[keys[-1]] = value


def create_configs_dir():
    if not os.path.isdir("/lto/configs"):
        os.mkdir("/lto/configs")
    if not os.path.isdir("/lto/data"):
        os.mkdir("/lto/data")


def parse_env_variables():
    dictionary = dict()
    for env_key in os.environ:
        if "__" in env_key:
            parts = env_key.split('__')
            keys = [x.lower().replace('_', '-') for x in parts]
            nested_set(dictionary, keys, os.environ[env_key])
    return dictionary


def get_wallet_data():
    seed = os.environ.get('LTO_WALLET_SEED')
    seed_base58 = os.environ.get('LTO_WALLET_SEED_BASE58')
    if seed_base58 is not None:
        try:
            base58.b58decode(seed_base58.encode())
        except:
            seed_base58 = base58.b58encode(seed.encode())
    else:
        if seed is None:
            seed = pw.Address().seed
            print('Seed phrase:', seed)
        seed_base58 = base58.b58encode(seed.encode())
    password = os.environ.get('LTO_WALLET_PASSWORD', generate_password())
    if os.environ.get('LTO_WALLET_PASSWORD') is None:
        print('Wallet password:', password)
    return seed_base58, password

def secureHash(message):
    h = blake2b(digest_size=32)
    h.update(message.encode())
    return base58.b58encode(sha256(h.digest()).digest())

if __name__ == "__main__":
    if NETWORK is None or NETWORK not in network_names:
        NETWORK = 'MAINNET'

    create_configs_dir()

    if NETWORK == 'MAINNET':
        copyfile('/lto-node/lto-mainnet.conf', '/lto/configs/lto-config.conf')
    elif NETWORK == 'TESTNET':
        copyfile('/lto-node/lto-testnet.conf', '/lto/configs/lto-config.conf')
    elif NETWORK == 'CUSTOM':
        copyfile('/lto-node/lto-custom.conf', '/lto/configs/lto-config.conf')

    api_key = os.environ.get('LTO_API_KEY', 'lt1secretapikey!')
    api_key_hash = secureHash(api_key)

    env_dict = parse_env_variables()
    lto_data = get_wallet_data()

    confFilePath = '/lto/configs/local.conf'

    if os.path.isfile(confFilePath):
        conf = ConfigFactory.parse_file(confFilePath)
        if conf.get('lto.wallet.seed') != lto_data[0] or conf.get('lto.wallet.password') != lto_data[1]:
            print('The wallet seed or password has changed. You will need create a new container to change the configuration.')
            sys.exit(0)

    nested_set(env_dict, ['lto', 'directory'], '/lto')
    nested_set(env_dict, ['lto', 'data-directory'], '/lto/data')
    nested_set(env_dict, ['lto', 'wallet', 'seed'], lto_data[0])
    nested_set(env_dict, ['lto', 'wallet', 'password'], lto_data[1])
    nested_set(env_dict, ['lto', 'rest-api', 'api-key-hash'], api_key_hash)

    ENABLE_REST_API = os.environ.get('ENABLE_REST_API', os.environ.get('LTO_ENABLE_REST_API', 'false'))
    if ENABLE_REST_API.lower() in ['yes', 'true', 't', '1', 'on']:
        nested_set(env_dict, ['lto', 'rest-api', 'enable'], 'yes')
        nested_set(env_dict, ['lto', 'rest-api', 'bind-address'], '0.0.0.0')

    LTO_NODE_NAME = os.getenv('LTO_NODE_NAME')
    if LTO_NODE_NAME is not None:
        nested_set(env_dict, ['lto', 'network', 'node-name'], LTO_NODE_NAME)

    LTO_DECLARED_ADDRESS = os.getenv('LTO_DECLARED_ADDRESS')
    if LTO_DECLARED_ADDRESS is not None:
        nested_set(env_dict, ['lto', 'network', 'declared-address'], LTO_DECLARED_ADDRESS)

    LTO_FEATURES = os.getenv('LTO_FEATURES')
    if LTO_FEATURES is not None:
        nested_set(env_dict, ['lto', 'features', 'supported'], LTO_FEATURES.split(','))

    config = ConfigFactory.from_dict(env_dict)
    local_conf = HOCONConverter.convert(config, 'hocon')
    with open(confFilePath, 'w') as file:
        file.write(local_conf)
