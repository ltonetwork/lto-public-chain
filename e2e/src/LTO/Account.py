from nacl.signing import SigningKey, VerifyKey
from LTO import crypto
import base58


class Account(object):

    SODIUM_CRYPTO_SIGN_BYTES = 64
    SODIUM_CRYPTO_SIGN_PUBLICKEYBYTES = 32

    def __init__(self, address, publicKey, privateKey = '', seed='', nonce=0):
        self.address = address
        self.publicKey = publicKey
        self.privateKey = privateKey
        self.seed = seed
        self.nonce = nonce

    def sign(self, message):
        if (self.privateKey == ''):
            raise Exception("Private key not set")
        if isinstance(self.privateKey, SigningKey):
            return base58.b58encode(self.privateKey.sign(message).signature)
        else:
            signature = self.privateKey.sign(message)
            return base58.b58encode(signature)

    def getPublicKey(self):
        if isinstance(self.publicKey, VerifyKey):
            return base58.b58encode(bytes(self.publicKey))
        else:
            return base58.b58encode(self.publicKey.to_string())

    def verifySignature(self, message: str, signature: str, encoding: str = 'base58'):
        if not self.publicKey:
            raise Exception('Unable to verify message; no public sign key')
        rawSignature = crypto.decode(signature, encoding)
        return self.publicKey.verify(message, rawSignature)

    def getNetwork(self):
        return str(base58.b58decode(self.address))[6]


