import base58
from LTO import crypto
from LTO.Transaction import Transaction
import struct

class Anchor(Transaction):
    TYPE = 15
    DEFAULT_ANCHOR_FEE = 35000000
    DEFAULT_VERSION = 3

    def __init__(self, anchor):
        super().__init__()

        self.anchor = anchor
        self.txFee = self.DEFAULT_ANCHOR_FEE
        self.version = self.DEFAULT_VERSION


    def __toBinaryV1(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">H", 1) +
                struct.pack(">H", len(crypto.str2bytes(self.anchor))) +
                crypto.str2bytes(self.anchor) +
                struct.pack(">Q", self.timestamp) +
                struct.pack(">Q", self.txFee))

    def __toBinaryV3(self):
        return (
                self.TYPE.to_bytes(1, 'big') +
                b'\3' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                struct.pack(">H", 1) +
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
        return({
            "type": self.TYPE,
            "version": self.version,
            "sender": self.sender,
            "senderKeyType": "ed25519",
            "senderPublicKey": self.senderPublicKey,
            "fee": self.txFee,
            "timestamp": self.timestamp,
            "anchors": [base58.b58encode(crypto.str2bytes(self.anchor))],
            "proofs": self.proofs
            } | self._sponsorJson())

    @staticmethod
    def fromData(data):
        tx = Anchor(anchor='')
        tx.id = data['id'] if 'id' in data else ''
        tx.type = data['type']
        tx.version = data['version']
        tx.sender = data['sender'] if 'sender' in data else ''
        tx.senderKeyType = data['senderKeyType'] if 'senderKeyType' in data else 'ed25519'
        tx.senderPublicKey = data['senderPublicKey']
        tx.fee = data['fee']
        tx.timestamp = data['timestamp']
        tx.anchors = data['anchors']
        tx.proofs = data['proofs'] if 'proofs' in data else []
        tx.height = data['height'] if 'height' in data else ''
        return tx
