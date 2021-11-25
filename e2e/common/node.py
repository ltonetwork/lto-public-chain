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


def is_node_up(timeout=1):
  try:
    polling.poll(_ping_node, step=1, timeout=timeout)
    return True
  except:
    return False

def _ping_node():
  try:
    return requests.get(config.node_url + "/", headers=header(), timeout=2).status_code == 200
  except:
    return False

