import base58
import struct
from LTO.Transaction import Transaction
from LTO import crypto


class CancelLease(Transaction):
    TYPE = 9
    DEFAULT_CANCEL_LEASE_FEE = 500000000
    DEFAULT_VERSION = 3


    def __init__(self, leaseId):
        super().__init__()
        self.leaseId = leaseId
        self.txFee = self.DEFAULT_CANCEL_LEASE_FEE
        self.version = self.DEFAULT_VERSION

    def __toBinaryV2(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\02' +
                crypto.str2bytes(self.chainId) +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                struct.pack(">Q", self.timestamp) +
                base58.b58decode(self.leaseId))

    def __toBinaryV3(self):
        return (
                self.TYPE.to_bytes(1, 'big') +
                b'\3' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                base58.b58decode(self.leaseId)
                )

    def toBinary(self):
        if self.version == 2:
            return self.__toBinaryV2()
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
            "proofs": self.proofs,
            "leaseId": self.leaseId
        } | self._sponsorJson())

    @staticmethod
    def fromData(data):
        tx = CancelLease(leaseId='')
        tx.id = data['id'] if 'id' in data else ''
        tx.type = data['type']
        tx.version = data['version']
        tx.sender = data['sender'] if 'sender' in data else ''
        tx.senderKeyType = data['senderKeyType'] if 'senderKeyType' in data else 'ed25519'
        tx.senderPublicKey = data['senderPublicKey']
        tx.fee = data['fee']
        tx.timestamp = data['timestamp']
        tx.proofs = data['proofs'] if 'proofs' in data else []
        tx.leaseId = data['leaseId'] if 'leaseId' in data else ''
        tx.height = data['height'] if 'height' in data else ''
        return tx



