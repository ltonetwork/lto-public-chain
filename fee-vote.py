# Vote to increase, maintain, or decrease the fee, based on the current price of LTO in USD.
# This script should be run a cron job once an hour.

import requests
import os
import sys

NETWORK = os.environ.get('LTO_NETWORK')
if NETWORK is None or NETWORK not in ['MAINNET', 'TESTNET', 'CUSTOM']:
    NETWORK = 'MAINNET'

ENABLE_REST_API = os.environ.get('LTO_ENABLE_REST_API', 'maybe')


def coingecko_price() -> float:
    response = requests.get("https://api.coingecko.com/api/v3/simple/price?ids=lto-network&vs_currencies=usd")
    response.raise_for_status()

    data = response.json()

    # CoinGecko returns an empty object in case the price is unknown.
    if 'lto-network' not in data or 'usd' not in data['lto-network']:
        raise Exception("Failed to get price from CoinGecko")

    return data['lto-network']['usd']


def coinmarketcap_price(api_key: str) -> float:
    response = requests.get(
        "https://pro-api.coinmarketcap.com/v2/tools/price-conversion?amount=1&symbol=lto&convert=usd",
        headers = {"X-CMC_PRO_API_KEY": api_key}
    )
    response.raise_for_status()

    data = response.json()
    return data['data'][0]['quote']['USD']['price']


def fetch_price(node: str) -> int:
    response = requests.get("%s/fees/status" % node, timeout=1)
    response.raise_for_status()

    # Make decision based on next price, not current one. Don't increase the price if it's already increasing.
    data = response.json()
    return data['next']['price']


def lto_fee_price():
    has_rest_api = ENABLE_REST_API.lower() in ['yes', 'true', 't', '1', 'on', 'maybe']

    if has_rest_api:
        try:
            return fetch_price("http://localhost:6869")
        except requests.exceptions.Timeout:
            pass

    node = "https://nodes.lto.network" if NETWORK == "MAINNET" else "https://testnet.lto.network"
    return fetch_price(node)


def determine_vote(target: int, current: int):
    if target < current / 1.1:
        return 'decrease'
    elif target > current * 1.1:
        return 'increase'
    else:
        return 'remain'


def lto_fee_vote(file: str, vote: str):
    with open(file, 'w') as f:
        f.write(vote)


def main(file: str):
    price = coingecko_price()

    cmc_api_key = os.environ.get('COINMARKETCAP_API_KEY')
    cmc_price = coinmarketcap_price(cmc_api_key) if cmc_api_key else None

    if cmc_price and abs(price - cmc_price) > 0.1 * price:
        vote = 'remain'
        print("More than 10%% difference between CoinGecko (%f) and CMC price (%f), vote: remain" % (price, cmc_price))
    else:
        target = int(os.environ.get('LTO_FEE_TARGET', 2000)) / price
        current = lto_fee_price()
        vote = determine_vote(target, current)
        print("target: %d, current: %d, vote: %s" % (target, current, vote))

    lto_fee_vote(file, vote)


if __name__ == '__main__':
    try:
        file = sys.argv[1]
        main(file)
        sys.exit(0)
    except Exception as err:
        print(err, file=sys.stderr)
        sys.exit(1)
