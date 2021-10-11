def pow_mod(x, y, z):
    "Calculate (x ** y) % z efficiently."
    number = 1
    while y:
        if y & 1:
            number = number * x % z
        y >>= 1
        x = x * x % z
    return number

p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
compressed_key = '0314fc03b8df87cd7b872996810db8458d61da8448e531569c8517b469a119d267'
y_parity = int(compressed_key[:2]) - 2
print(compressed_key)
print(y_parity)
x = int(compressed_key[2:], 16)
print(x)
a = (pow_mod(x, 3, p) + 7) % p
y = pow_mod(a, (p+1)//4, p)
if y % 2 != y_parity:
    y = -y % p
uncompressed_key = '04{:x}{:x}'.format(x, y)
print(uncompressed_key)
