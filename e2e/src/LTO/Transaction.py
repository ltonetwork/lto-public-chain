from abc import ABC, abstractmethod
from time import time
from LTO.PublicNode import PublicNode
from LTO.Account import Account



class Transaction(ABC):

    def __init__(self):

        self.txFee = 0
        self.timestamp = 0

        self.proofs = []
        self.sender = ''
        self.senderPublicKey = ''
        self.chainId = ''
        self.sponsor = ''
        self.sponsorPublicKey = ''
        self.sponsorKeyType = 'ed25519'


    @abstractmethod
    def toBinary(self):
        pass

    def isSigned(self):
        return len(self.proofs) != 0

    def signWith(self, account: Account):
        if self.timestamp == 0:
            self.timestamp = int(time() * 1000)

        if self.sender == '':
            self.sender = account.address
            self.senderPublicKey = account.getPublicKey()

        self.chainId = account.getNetwork()
        self.proofs.append(account.sign(self.toBinary()))

    def sponsorWith(self, sponsorAccount: Account):
        if not self.isSigned():
            raise Exception('Transaction must be signed first')

        self.sponsor = sponsorAccount.address
        self.sponsorPublicKey = sponsorAccount.publicKey
        self.proofs.append(sponsorAccount.sign(self.toBinary()))

    def broadcastTo(self, node: PublicNode):
        return node.broadcast(self)

    @abstractmethod
    def toJson(self):
        pass

    def _sponsorJson(self):
        if self.sponsor:
            return {"sponsor": self.sponsor,
                    "sponsorPublicKey": self.sponsorPublicKey,
                    "sponsorKeyType": self.sponsorKeyType}
        else:
            return{}

    def __getattr__(self, item):
        return getattr(self, item)
