import base58
from LTO import crypto
import struct

from LTO.Transaction import Transaction


class MassTransfer(Transaction):
    DEFAULT_BASE_FEE = 100000000
    TYPE = 11
    DEFAULT_VERSION = 3

    def __init__(self, transfers, attachment=''):
        super().__init__()
        self.transfers = transfers
        self.attachment = attachment
        self.transfersData = ''
        self.baseFee = self.DEFAULT_BASE_FEE
        self.txFee = self.baseFee + int(len(self.transfers) * self.baseFee / 10)
        self.version = self.DEFAULT_VERSION

        if len(self.transfers) > 100:
            raise Exception('Too many recipients')

        self.transfersData = b''
        for i in range(0, len(self.transfers)):
            self.transfersData += base58.b58decode(self.transfers[i]['recipient']) \
                             + struct.pack(">Q", self.transfers[i]['amount'])

    def __toBinaryV1(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">H", len(self.transfers)) +
                self.transfersData +
                struct.pack(">Q", self.timestamp) +
                struct.pack(">Q", self.txFee) +
                struct.pack(">H", len(self.attachment)) +
                crypto.str2bytes(self.attachment))

    def __toBinaryV3(self):
        return (
                self.TYPE.to_bytes(1, 'big') +
                b'\3' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                struct.pack(">H", len(self.transfers)) +

                self.transfersData +

                struct.pack(">H", len(self.attachment)) +
                crypto.str2bytes(self.attachment))

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
            "sender": self.sender,
            "senderKeyType": "ed25519",
            "senderPublicKey": self.senderPublicKey,
            "fee": self.txFee,
            "timestamp": self.timestamp,
            "proofs": self.proofs,
            "attachment": base58.b58encode(crypto.str2bytes(self.attachment)),
            "transfers": self.transfers
        } | self._sponsorJson())

    @staticmethod
    def fromData(data):
        tx = MassTransfer(transfers='')
        tx.type = data['type']
        tx.version = data['version']
        tx.id = data['id'] if 'id' in data else ''
        tx.sender = data['sender'] if 'sender' in data else ''
        tx.senderKeyType = data['senderKeyType'] if 'senderKeyType' in data else 'ed25519'
        tx.senderPublicKey = data['senderPublicKey']
        tx.fee = data['fee']
        tx.timestamp = data['timestamp']
        tx.proofs = data['proofs'] if 'proofs' in data else []
        tx.attachment = data['attachment'] if 'attachment' in data else ''
        tx.transfers = data['transfers']
        tx.height = data['height'] if 'height' in data else ''
        return tx

