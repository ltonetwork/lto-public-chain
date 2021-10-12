import base58
from LTO import crypto
from time import time
from LTO.Transaction import Transaction
import struct


class Association(Transaction):
    DEFAULT_LEASE_FEE = 100000000
    TYPE = 16
    DEFAULT_VERSION = 3

    def __init__(self, recipient, associationType, anchor='', expires=0):
        super().__init__()
        self.recipient = recipient
        self.associationType = associationType
        self.anchor = anchor
        self.txFee = self.DEFAULT_LEASE_FEE
        self.version = self.DEFAULT_VERSION

        self.expires = expires
        current = int(time() * 1000)
        if self.expires != 0 and self.expires <= current:
            raise Exception('Wring exipration date')

    def __toBinaryV1(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\1' +
                crypto.str2bytes(crypto.getNetwork(self.sender)) +
                base58.b58decode(self.senderPublicKey) +
                base58.b58decode(self.recipient) +
                struct.pack(">i", self.associationType) +
                b'\1' +
                struct.pack(">H", len(crypto.str2bytes(self.anchor))) +
                crypto.str2bytes(self.anchor) +
                struct.pack(">Q", self.timestamp) +
                struct.pack(">Q", self.txFee))

    def __toBinaryV3(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\3' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                base58.b58decode(self.recipient) +
                struct.pack(">i", self.associationType) +
                crypto.str2bytes(crypto.getNetwork(self.sender)) +
                struct.pack(">Q", self.expires) +
                struct.pack(">H", len(crypto.str2bytes(self.anchor))) +
                crypto.str2bytes(self.anchor))

    def toBinary(self):
        if self.version == 1:
            return self.__toBinaryV1()
        elif self.version == 3:
            return self.__toBinaryV3()
        else:
            raise Exception('Incorrect Version')



    def toJson(self):
        if self.version == 3:
            return ({
                    "type": self.TYPE,
                    "version": self.version,
                    "sender": self.sender,
                    "senderKeyType": "ed25519",
                    "senderPublicKey": self.senderPublicKey,
                    "recipient": self.recipient,
                    "associationType": self.associationType,
                    "hash": base58.b58encode(crypto.str2bytes(self.anchor)),
                    "timestamp": self.timestamp,
                    "expires": self.expires,
                    "fee": self.txFee,
                    "proofs": self.proofs
                } | self._sponsorJson())
        elif self.version == 1:
            return ({
                "type": self.TYPE,
                "version": self.version,
                "recipient": self.recipient,
                "associationType": self.associationType,
                "hash": base58.b58encode(crypto.str2bytes(self.anchor)),
                "sender": self.sender,
                "senderPublicKey": self.senderPublicKey,
                "timestamp": self.timestamp,
                "fee": self.txFee,
                "proofs": self.proofs
            })
        else:
            raise Exception('Incorrect Version')

    @staticmethod
    def fromData(data):
        tx = Association(recipient='', associationType='', anchor='')
        tx.type = data['type']
        tx.version = data['version']
        tx.id = data['id'] if 'id' in data else ''
        tx.sender = data['sender'] if 'sender' in data else ''
        tx.senderKeyType = data['senderKeyType'] if 'senderKeyType' in data else 'ed25519'
        tx.senderPublicKey = data['senderPublicKey']
        tx.recipient = data['recipient']
        tx.associationType = data['associationType']
        tx.hash = data['hash']
        tx.timestamp = data['timestamp']
        tx.expires = data['expires'] if 'expires' in data else ''
        tx.fee = data['fee']
        tx.proofs = data['proofs'] if 'proofs' in data else []
        tx.height = data['height'] if 'height' in data else ''

        return tx

