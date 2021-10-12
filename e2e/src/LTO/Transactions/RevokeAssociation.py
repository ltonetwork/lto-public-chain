import base58
from LTO import crypto
from LTO.Transaction import Transaction
import struct

class RevokeAssociation(Transaction):
    TYPE = 17
    DEFAULT_LEASE_FEE = 100000000
    DEFAULT_VERSION = 3

    def __init__(self, recipient, associationType, anchor = ''):
        super().__init__()
        self.recipient = recipient
        self.anchor = anchor
        self.associationType = associationType

        self.txFee = self.DEFAULT_LEASE_FEE
        self.version = self.DEFAULT_VERSION

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
                b'\1' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                base58.b58decode(self.recipient) +
                struct.pack(">i", self.associationType) +
                struct.pack(">H", len(crypto.str2bytes(self.anchor))) +
                crypto.str2bytes(self.anchor)
                )

    def toBinary(self):
        if self.version == 1:
            return self.__toBinaryV1()
        elif self.version == 3:
            return self.__toBinaryV3()
        else:
            raise Exception('Incorrect Version')

    def toJson(self):
        return ({
            "type": self.TYPE,
            "version": self.version,
            "sender": self.senderPublicKey,
            "senderKeyType": "ed25519",
            "senderPublicKey": self.senderPublicKey,
            "recipient": self.recipient,
            "associationType": self.associationType,
            "hash": base58.b58encode(crypto.str2bytes(self.anchor)),
            "timestamp": self.timestamp,
            "fee": self.txFee,
            "proofs": self.proofs
        } | self._sponsorJson())

    @staticmethod
    def fromData(data):
        tx = RevokeAssociation(recipient='', associationType='')
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
        tx.fee = data['fee']
        tx.proofs = data['proofs'] if 'proofs' in data else []
        tx.height = data['height'] if 'height' in data else ''
        return tx