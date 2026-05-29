import random

#тест миллера-рабина
def isprime(n, k=40):
    if n < 2:
        return False
    if n in (2, 3):
        return True
    if n % 2 == 0:
        return False
    
    r = 0
    d = n - 1
    while d % 2 == 0:
        d //= 2
        r += 1


    def check_composite(a):
        x = pow(a, d, n)
        if x == 1 or x == n - 1:
            return False
        for _ in range(r - 1):
            x = pow(x, 2, n)
            if x == n - 1:
                return False
        return True
    

    for _ in range(k):
        a = random.randint(2, n - 2)
        if check_composite(a):
            return False
    
    return True

#генерируем большое число нечетное
def generate_large_prime(bits=256):
    while True:
        n = random.getrandbits(bits) | 1  # гарантируем нечётность
        if isprime(n):
            return n

#генерация сильно простого числа
def gordon_strong_prime(bits=250):
    s = generate_large_prime(bits // 2)
    t = generate_large_prime(bits // 2)
    
    i = random.randint(2**15, 2**16)
    while True:
        r = 2 * i * t + 1
        if isprime(r):
            break
        i += 1
    
    p0 = 2 * pow(s, r - 2, r) * s - 1
    
    j = random.randint(2**15, 2**16)
    while True:
        p = 2 * j * r * s + p0
        if isprime(p):
            return p
        j += 1


if __name__ == "__main__":
    p = gordon_strong_prime(250)
    print(f"Сгенерировано простое число: {p}")
    print(f"Длина в битах: {p.bit_length()}")
