from LTO.AccountFactory import AccountFactory
from LTO import crypto
import struct
from nacl.signing import SigningKey, VerifyKey
import base58
from LTO.Account import Account

class AccountED25519(AccountFactory):

    def __init__(self, chainId):
        super().__init__(chainId)


    def createSignKeys(self, seed, nonce=0):
        seedHash = crypto.hashChain(struct.pack(">L", nonce) + crypto.str2bytes(seed))
        accountSeedHash = crypto.sha256(seedHash)
        privateKey = SigningKey(accountSeedHash)
        publicKey = privateKey.verify_key
        return privateKey, publicKey

    def createAddress(self, publicKey):
        unhashedAddress = chr(1) + str(self.chainId) + crypto.hashChain(publicKey.__bytes__())[0:20]
        addressHash = crypto.hashChain(crypto.str2bytes(unhashedAddress))[0:4]
        return base58.b58encode(crypto.str2bytes(unhashedAddress + addressHash))


    def createFromPrivateKey(self, privateKey):
        if not isinstance(privateKey, SigningKey):
            publicKey = VerifyKey(base58.b58decode(privateKey)[-32:])
            privateKey = base58.b58decode(privateKey)[:-32]
            address = self.createAddress(publicKey)
        else:
            publicKey = privateKey.verify_key
            address = self.createAddress(publicKey)
        return Account(address, publicKey, privateKey)


    def createFromPublicKey(self, publicKey):
        if not isinstance(publicKey, VerifyKey):
            decodedPublicKey = VerifyKey(base58.b58decode(publicKey))
            address = self.createAddress(decodedPublicKey)
        else:
            address = self.createAddress(publicKey)
        return Account(address, publicKey)