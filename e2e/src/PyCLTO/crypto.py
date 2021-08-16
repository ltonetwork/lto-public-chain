import hashlib
import pyblake2
import base58


if bytes == str:  # python2
    str2bytes = lambda s: s
    bytes2str = lambda b: b
    str2list = lambda s: [ord(c) for c in s]
else:  # python3
    str2bytes = lambda s: s.encode('latin-1')
    bytes2str = lambda b: ''.join(map(chr, b))
    str2list = lambda s: [c for c in s]


def sha256(s):
    return hashlib.sha256(str2bytes(s)).digest()

def hashChain(s):
    a=pyblake2.blake2b(s, digest_size=32).digest()
    b=hashlib.sha256(a).digest()
    return ''.join(map(chr, b))

def sign(privateKey, message):
    return base58.b58encode(privateKey.sign(message).signature)

def id(message):
    return base58.b58encode(hashlib.sha256(message).digest())

def verify_signature(pub_key, message, signature):
    """ all of the arguments are expected in a string format """
    #return curve.verifySignature(base58.b58decode(pub_key), message.encode(), base58.b58decode(signature)) == 0
