import subprocess
import polling
import requests
import os

from e2e.common import config


def header():
    return {"X-API-Key": '{}'.format(config.api_key)}


def stop_node():
    return requests.post(config.node_url + "/node/stop", header())


def start_node():
    dir_path = os.path.dirname(os.path.realpath(__file__))
    subprocess.run(dir_path + "/../bin/run_public_node", shell=True, check=True)


def is_node_up():
    try:
        polling.poll(
            lambda: requests.get(config.node_url + "/"), header() == 200,
            step=1,
            timeout=180
        )
        return True
    except:
        return False
