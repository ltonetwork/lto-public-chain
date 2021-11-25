import subprocess
import polling
import requests
import os

from e2e.common import config


def header():
    return {"X-API-Key": '{}'.format(config.api_key)}


def stop_node():
    dir_path = os.path.dirname(os.path.realpath(__file__))
    subprocess.run(dir_path + "/../bin/stop_public_node", shell=True, check=True)


def start_node():
    dir_path = os.path.dirname(os.path.realpath(__file__))
    subprocess.run(dir_path + "/../bin/run_public_node", shell=True, check=True)


def is_node_up():
    try:
        polling.poll(
            lambda: requests.get(config.node_url + "/", headers=header()).status_code == 200,
            step=1,
            timeout=10
        )
        return True
    except:
        return False
