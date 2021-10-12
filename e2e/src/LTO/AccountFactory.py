import os
import struct


from LTO.Account import Account
from LTO import crypto
from LTO.WordList import wordList

import base58

from abc import ABC, abstractmethod

class AccountFactory(ABC):

    # this is the constructor
    def __init__(self, chainId):
        self.chainId = chainId

    @abstractmethod
    def createSignKeys(self, seed):
        pass

    @abstractmethod
    def createAddress(self, publicKey):
        pass

    def create(self):
        return self.createFromSeed(self.generateSeedPhrase())

    def createFromSeed(self, seed, nonce=0):
        privateKey, publicKey = self.createSignKeys(seed, nonce)
        address = self.createAddress(publicKey)
        return Account(address, publicKey, privateKey, seed)

    @abstractmethod
    def createFromPrivateKey(self, privateKey):
        pass

    @abstractmethod
    def createFromPublicKey(self, publicKey):
        pass

    def createWithValues(self, address, publicKey, privateKey, seed=''):
        return Account(address, publicKey, privateKey, seed)

    def assertAccount(self, account, address, publicKey, privateKey, seed):
        if address and account.address != address:
            return False
        if publicKey and account.publicKey != publicKey:
            return False
        if privateKey and account.privateKey != privateKey:
            return False
        return True

    # create the class from the seed
    def generateSeedPhrase(self):
        wordCount = len(wordList)
        words = []
        for i in range(5):
            r = crypto.bytes2str(os.urandom(4))
            x = (ord(r[3])) + (ord(r[2]) << 8) + (ord(r[1]) << 16) + (ord(r[0]) << 24)
            w1 = x % wordCount
            w2 = ((int(x / wordCount) >> 0) + w1) % wordCount
            w3 = ((int((int(x / wordCount) >> 0) / wordCount) >> 0) + w2) % wordCount
            words.append(wordList[w1])
            words.append(wordList[w2])
            words.append(wordList[w3])
        return ' '.join(words)
