from __future__ import absolute_import, division, print_function, unicode_literals

from LTO.AccountFactory import AccountFactory
from LTO.PublicNode import PublicNode

from LTO.Transactions.Anchor import Anchor
from LTO.Transactions.Lease import Lease
from LTO.Transactions.Association import Association
from LTO.Transactions.CancelLease import CancelLease
from LTO.Transactions.CancelSponsorship import CancelSponsorship
from LTO.Transactions.MassTransfer import MassTransfer
from LTO.Transactions.RevokeAssociation import RevokeAssociation
from LTO.Transactions.SetScript import SetScript
from LTO.Transactions.Sponsorship import Sponsorship
from LTO.Transactions.Transfer import Transfer
from LTO.Accounts.AccountFactoryECDSA import AccountECDSA
from LTO.Accounts.AccountFactoryED25519 import AccountED25519


class PyCLTO:

    def __init__(self, chainId='T'):

        if chainId == 'T':
            self.NODE = PublicNode('https://testnet.lto.network')
        elif chainId == 'L':
            self.NODE = PublicNode('https://nodes.lto.network')
        else:
            self.NODE = ''

        self.chainId = chainId

        self.accountFactories = {
            'ed25519': AccountED25519(chainId),
            'secp256r1': AccountECDSA(chainId, curve='secp256r1'),
            'secp256k1': AccountECDSA(chainId, curve='secp256k1')
        }

    def Account(self, keytype='ed25519', address='', publicKey='', privateKey='', seed='', nonce=0):
        factory = self.accountFactories[keytype]

        if seed:
            account = factory.createFromSeed(seed, nonce)
        elif privateKey:
            account = factory.createFromPrivateKey(privateKey)
        elif publicKey:
            account = factory.createFromPublicKey(publicKey)
        else:
            account = factory.create()

        # We don't have a case for someone who just passes the address
        if not factory.assertAccount(account, address, publicKey, privateKey, seed):
            raise Exception("Accounts info are inconsistent")
        return account

    def getChainId(self):
        return self.chainId

    def fromData(self, data):

        if data['type'] == 4:
            return Transfer(recipient=data['recipient'], amount=data['amount']).fromData(data)
        elif data['type'] == 8:
            return Lease(amount=1, recipient='').fromData(data)
        elif data['type'] == 11:
            return MassTransfer(transfers='').fromData(data)
        elif data['type'] == 15:
            return Anchor(anchor='').fromData(data)
        elif data['type'] == 16:
            return Association(recipient='', associationType='', anchor='').fromData(data)
        elif data['type'] == 17:
            return RevokeAssociation(recipient='', associationType='').fromData(data)
        elif data['type'] == 18:
            return Sponsorship(data['recipient']).fromData(data)
        elif data['type'] == 19:
            return CancelSponsorship(data['recipient']).fromData(data)
        elif data['type'] == 13:
            return SetScript(data['script']).fromData(data)
        elif data['type'] == 9:
            return CancelLease(leaseId='').fromData(data)
        else:
            raise Exception('Incorrect transaction Type')
