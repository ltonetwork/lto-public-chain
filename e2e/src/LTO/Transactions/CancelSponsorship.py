import base58
from LTO import crypto
import struct
from LTO.Transaction import Transaction


class CancelSponsorship(Transaction):
    DEFAULT_SPONSORSHIP_FEE = 500000000
    TYPE = 19
    DEFAULT_VERSION = 3

    def __init__(self, recipient):
        super().__init__()
        self.recipient = recipient
        crypto.validateAddress(recipient)
        self.txFee = self.DEFAULT_SPONSORSHIP_FEE
        self.version = self.DEFAULT_VERSION

    def __toBinaryV1(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\1' +  # version
                crypto.str2bytes(crypto.getNetwork(self.sender)) +
                base58.b58decode(self.senderPublicKey) +
                base58.b58decode(self.recipient) +
                struct.pack(">Q", self.timestamp) +
                struct.pack(">Q", self.txFee))

    def __toBinaryV3(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\3' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +  # key type 'ed25519'
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                base58.b58decode(self.recipient)
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
            "senderKeyType": "ed25519",
            "recipient": self.recipient,
            "sender": self.sender,
            "senderPublicKey": self.senderPublicKey,
            "timestamp": self.timestamp,
            "fee": self.txFee,
            "proofs": self.proofs
        } | self._sponsorJson())

    @staticmethod
    def fromData(data):
        tx = CancelSponsorship(data['recipient'])
        tx.type = data['type']
        tx.version = data['version']
        tx.id = data['id'] if 'id' in data else ''
        tx.sender = data['sender'] if 'sender' in data else ''
        tx.senderKeyType = data['senderKeyType'] if 'senderKeyType' in data else 'ed25519'
        tx.senderPublicKey = data['senderPublicKey']
        tx.timestamp = data['timestamp']
        tx.fee = data['fee']
        tx.proofs = data['proofs']
        tx.recipient = data['recipient']
        tx.height = data['height'] if 'height' in data else ''
        return tx

