import string
import random

def random_string(length=16):
  return ''.join(random.choices(string.ascii_uppercase + string.digits, k=16))
