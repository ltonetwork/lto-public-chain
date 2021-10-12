import requests
import json

from LTO.Account import Account

class PublicNode(object):
    def __init__(self, url):
        self.url = url


    def wrapper(self, api, postData='', host='', headers=''):
        if not host:
            host = self.url

        if postData:
            r = requests.post('%s%s' % (host, api), data=postData,
                                headers={'content-type': 'application/json'})
        else:
            r = requests.get('%s%s' % (host, api), headers=headers)


        if r.status_code != 200:
            jsonResp = json.loads(r.text)
            raise Exception('{}'.format(jsonResp['message']))

        r.raise_for_status()

        return r.json()

    def broadcast(self, transaction):
        from LTO import PyCLTO

        data = json.dumps(transaction.toJson())

        response = self.wrapper(api='/transactions/broadcast', postData=data)
        return PyCLTO().fromData(response)

    def getScript(self, scriptSource):
        return self.wrapper('/utils/script/compile', scriptSource)['script'][7:]


    def height(self):
        return self.wrapper('/blocks/height')['height']

    def lastblock(self):
        return self.wrapper('/blocks/last')

    def block(self, n):
        return self.wrapper('/blocks/at/%d' % n)

    def tx(self, id):
        from LTO import PyCLTO
        response = self.wrapper('/transactions/info/%s' % id)
        return PyCLTO().fromData(response)

    def leaseList(self, address):
        return self.wrapper(api='/leasing/active/{}'.format(address))

    def sponsorshipList(self, address):
        return self.wrapper(api='/sponsorship/status/{}'.format(address))

    def balance(self, address):

        if type(address) == Account:
            address = address.address

        try:
            return self.wrapper('/addresses/balance/%s' % address)['balance']
        except:
            return -1

    def transactions(self, limit=100, after='', address = ''):
        return self.wrapper('/transactions/address/%s/limit/%d%s' % (
            address, limit, "" if after == "" else "?after={}".format(after)))

