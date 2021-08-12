import requests
import config

def url(endpoint):
    return config.node_url + endpoint

def api_key_header():
    return {"Authorization": 'Bearer {}'.format(config.api_key)}

def get(endpoint):
    return requests.get(url(endpoint), headers=api_key_header())

def post(endpoint, payload):
    return requests.post(url(endpoint), headers=api_key_header(), json=payload)

def delete(endpoint):
    return requests.post(url(endpoint), headers=api_key_header())
