import base58
from LTO import crypto
import struct
from LTO.Transaction import Transaction

class Lease(Transaction):
    DEFAULT_LEASE_FEE = 100000000
    TYPE = 8
    DEFAULT_VERSION = 3

    def __init__(self, recipient, amount):
        super().__init__()
        self.amount = amount
        self.recipient = recipient
        # The json response doesn't contain the recipient
        # crypto.validateAddress(recipient)
        self.txFee = self.DEFAULT_LEASE_FEE
        self.version = self.DEFAULT_VERSION
        if self.amount <= 0:
            raise Exception ('Amount must be > 0')


    def __toBinaryV2(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\2' +
                b'\0' +
                base58.b58decode(self.senderPublicKey) +
                base58.b58decode(self.recipient) +
                struct.pack(">Q", self.amount) +
                struct.pack(">Q", self.txFee) +
                struct.pack(">Q", self.timestamp))

    def __toBinaryV3(self):
        return (
                self.TYPE.to_bytes(1, 'big') +
                b'\3' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                base58.b58decode(self.recipient) +
                struct.pack(">Q", self.amount)
        )

    def toBinary(self):
        if self.version == 2:
            return self.__toBinaryV2()
        elif self.version == 3:
            return self.__toBinaryV3()
        else:
            raise Exception('Incorrect Version')

    def toJson(self):
        return ({
            "type": self.TYPE,
            "version": self.version,
            "sender": self.sender,
            "senderKeyType": "ed25519",
            "senderPublicKey": self.senderPublicKey,
            "recipient": self.recipient,
            "amount": self.amount,
            "fee": self.txFee,
            "timestamp": self.timestamp,
            "proofs": self.proofs
        } | self._sponsorJson())

    @staticmethod
    def fromData(data):
        tx = Lease('',1)
        tx.id = data['id'] if 'id' in data else ''
        tx.type = data['type']
        tx.version = data['version']
        tx.sender = data['sender'] if 'sender' in data else ''
        tx.senderKeyType = data['senderKeyType'] if 'senderKeyType' in data else 'ed25519'
        tx.senderPublicKey = data['senderPublicKey']
        tx.fee = data['fee']
        tx.timestamp = data['timestamp']
        tx.recipient = data['recipient']
        tx.proofs = data['proofs'] if 'proofs' in data else []
        tx.height = data['height'] if 'height' in data else ''
        return tx



