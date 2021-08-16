
class pyLTOCoin(object):
    def __init__(self, pylto):
        self.pylto = pylto
        self.assetId=''
        self.issuer = self.name = self.description = 'LTO'
        self.reissuable = False
        self.quantity=500000000e8
        self.decimals=8

    def __str__(self):
        return 'assetId = %s\n' \
               'issuer = %s\n' \
               'name = %s\n' \
               'description = %s\n' \
               'quantity = %d\n' \
               'decimals = %d'% (self.assetId, self.issuer, self.name, self.description, self.quantity, self.decimals)

    __repr__ = __str__


