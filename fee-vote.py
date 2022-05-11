#!/usr/bin/env python3

# Vote to increase, maintain, or decrease the fee, based on the current price of LTO in USD.
# This script should be run a cron job once an hour.

import requests
import os
import sys

def coingecko_price() -> float:
    response = requests.get(
        "https://api.coingecko.com/api/v3/simple/price?ids=lto-network&vs_currencies=usd",
        timeout=5
    )
    response.raise_for_status()

    data = response.json()

    # CoinGecko returns an empty object in case the price is unknown.
    if 'lto-network' not in data or 'usd' not in data['lto-network']:
        raise Exception("Failed to get price from CoinGecko")

    return data['lto-network']['usd']


def coinmarketcap_price(api_key: str) -> float:
    response = requests.get(
        "https://pro-api.coinmarketcap.com/v2/tools/price-conversion?amount=1&symbol=lto&convert=usd",
        headers={"X-CMC_PRO_API_KEY": api_key},
        timeout=5
    )
    response.raise_for_status()

    data = response.json()
    return data['data'][0]['quote']['USD']['price']


def fetch_price(node: str) -> int:
    response = requests.get("%s/fees/status" % node, timeout=5)
    response.raise_for_status()

    # Make decision based on next price, not current one. Don't increase the price if it's already increasing.
    data = response.json()
    return data['next']['price']


def determine_vote(target: int, current: int):
    if target < current / 1.1:
        return 'decrease'
    elif target > current * 1.1:
        return 'increase'
    else:
        return 'maintain'


def write_vote(file: str, vote: str):
    with open(file, 'w') as f:
        f.write(vote)


def main(node: str, file: str):
    price = coingecko_price()

    cmc_api_key = os.environ.get('COINMARKETCAP_API_KEY')
    cmc_price = coinmarketcap_price(cmc_api_key) if cmc_api_key else None

    if cmc_price and abs(price - cmc_price) > 0.1 * price:
        vote = 'remain'
        print("More than 10%% difference between CoinGecko (%f) and CMC price (%f), vote: remain" % (price, cmc_price))
    else:
        target = int(os.environ.get('LTO_FEE_TARGET', 20000)) / price
        max_target = int(os.environ.get('LTO_FEE_MAX_PRICE', 100000))
        current = fetch_price(node)
        vote = determine_vote(min(target, max_target), current)
        print("target: %d, current: %d, vote: %s" % (target, current, vote))

    write_vote(file, vote)


if __name__ == '__main__':
    if (len(sys.argv) < 3):
        print("USAGE: fee-vote NODE_URL PATH")
        sys.exit(1)


    try:
        main(sys.argv[1], sys.argv[2])
        sys.exit(0)
    except Exception as err:
        print(err, file=sys.stderr)
        sys.exit(1)
