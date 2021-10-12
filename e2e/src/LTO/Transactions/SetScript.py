import base64
from LTO.Transaction import Transaction
import struct
from LTO import crypto
import base58

class SetScript(Transaction):
    TYPE = 13
    DEFAULT_SCRIPT_FEE = 500000000
    DEFAULT_VERSION = 3

    def __init__(self, script):
        super().__init__()

        self.script = script
        self.compiledScript = base64.b64decode(self.script)

        self.txFee = self.DEFAULT_SCRIPT_FEE
        self.version = self.DEFAULT_VERSION

    def __toBinaryV1(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\1' +
                crypto.str2bytes(crypto.getNetwork(self.sender)) +
                base58.b58decode(self.senderPublicKey) +
                b'\1' +
                struct.pack(">H", len(self.compiledScript)) +
                self.compiledScript +
                struct.pack(">Q", self.txFee) +
                struct.pack(">Q", self.timestamp))

    def __toBinaryV3(self):
        return (self.TYPE.to_bytes(1, 'big') +
                b'\3' +
                crypto.str2bytes(self.chainId) +
                struct.pack(">Q", self.timestamp) +
                b'\1' +
                base58.b58decode(self.senderPublicKey) +
                struct.pack(">Q", self.txFee) +
                struct.pack(">H", len(self.compiledScript)) +
                self.compiledScript
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
            "sender": self.sender,
            "senderKeyType": "ed25519",
            "senderPublicKey": self.senderPublicKey,
            "script": 'base64:' + str(self.script),
            "timestamp": self.timestamp,
            "fee": self.txFee,
            "proofs": self.proofs
        } | self._sponsorJson())

    @staticmethod
    def fromData(data):
        tx = SetScript(data['script'])
        tx.id = data['id'] if 'id' in data else ''
        tx.type = data['type']
        tx.version = data['version']
        tx.sender = data['sender'] if 'sender' in data else ''
        tx.senderKeyType = data['senderKeyType'] if 'senderKeyType' in data else 'ed25519'
        tx.senderPublicKey = data['senderPublicKey']
        tx.fee = data['fee']
        tx.timestamp = data['timestamp']
        tx.proofs = data['proofs']
        tx.script = data['script']
        tx.height = data['height'] if 'height' in data else ''
        return tx