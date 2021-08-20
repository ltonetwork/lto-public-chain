import requests
import config

def url(endpoint):
    return config.node_url + endpoint

def api_key_header():
    return {"X-API-Key": '{}'.format(config.api_key)}

def get(endpoint):
    return requests.get(url(endpoint), headers=api_key_header())

def post(endpoint, payload=None):
    return requests.post(url(endpoint), headers=api_key_header(), json=payload)

def delete(endpoint):
    return requests.delete(url(endpoint), headers=api_key_header())

def get_from_url(url):
    return requests.get(url, headers=api_key_header())

def post_from_url(url, payload=None):
    return requests.post(url, headers=api_key_header(), json=payload)
