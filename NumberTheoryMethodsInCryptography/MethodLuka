import random

def factorize(n):
    factors = set()
    d = 2
    while d * d <= n:
        while n % d == 0:
            factors.add(d)
            n //= d
        d += 1
    if n > 1:
        factors.add(n)
    return factors


def error_f_lucas(n, t):
    factors = factorize(n - 1)

    phi = n - 1
    for p in factors:
        phi = phi // p * (p - 1)
    
    p_good = phi / (n - 1)
    error = (1 - p_good) ** t
    
    return error

def Lucas(n, t):
    if n < 2:
        return None
    if n == 2 or n == 3:
        return True
    if n % 2 == 0:
        return False
    
    factors = factorize(n - 1)
    
    for _ in range(t):
        a = random.randint(2, n - 2)
        
        if pow(a, n - 1, n) != 1:
            return False
        
        for p in factors:
            if pow(a, (n - 1) // p, n) == 1:
                break
        else:
            return True
    
    return False

def test_lucas():
    print("test_lucas")
    test_data = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
                 1, 4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25,
                 561, 8911, 10585, 15841, 29341, 41041]
    t = 3
    
    for n in test_data:
        res = Lucas(int(n), t)
        if res == True:
            err = error_f_lucas(int(n), t)
            print(f"Число {n} простое. Вероятность ошибки = {err:.6f}\n")
        elif res == False:
            print(f"Число {n} составное\n")
        else:
            print(f"Число {n} не подходит (меньше 2)\n")

def main():
    test_lucas()

if __name__ == "__main__":
    main()
