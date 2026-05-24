import random

def error_f_mil_rab(t):
    return (1/4) ** t

def error_f_sol_st(t):
    return (1/2) ** t

def Jacobi(a, n):
    if a == 0:
        return 0
    if a == 1:
        return 1
    
    k = 0
    a1 = a
    while a1 % 2 == 0:
        a1 = a1 // 2
        k = k + 1
    
    s = 1
    if k % 2 == 1:
        r = n % 8
        if r != 1 and r != 7:
            s = -s
    
    if n % 4 == 3 and a1 % 4 == 3:
        s = -s

    if a1 == 1:
        return s
    else:
        return s * Jacobi(n % a1, a1)


def SolovayStrassen(n , t):
    if n <= 3:
        print("не подходит под условия")
        return False
    if n % 2 == 0:
        return False
    
    for _ in range(t):
        a = random.randint(2, n - 2)

        r = pow(a, (n - 1) // 2, n)
        if r == n - 1:
            r = -1
        
        s = Jacobi(a, n)

        if r != s:
            return False
    
    return True


def MillerRabin(n, t):
    if n < 2:
        return None
    if n == 2 or n == 3:
        return True
    if n % 2 == 0:
        return False

    r = n - 1
    s = 0
    while r % 2 == 0:
        r //= 2
        s += 1
    
    for _ in range(t):
        a = random.randint(2, n - 2)
        y = pow(a, r, n)
        
        if y == 1 or y == n - 1:
            continue
        
        for _ in range(s - 1):
            y = pow(y, 2, n)
            if y == n - 1:
                break
        else:
            return False
    
    return True



def test_mil_rab():
    print("test_mil_rab")
    test_data=[2, 3, 5, 7, 11, 1, 9, 10, 12, 14, 15, 17, 19, 21,
               561, 8911, 10585, 15841, 29341, 41041, 
               59283834566841846300814233552512955601, 
               9904788899632347103]
    t = 2
    #t = 5

    for i in test_data:
        res = MillerRabin(int(i), t)
        if res == True:
            err = error_f_mil_rab(t)
            print(f"Число {i} простое. Вероятность ошибки = {err:.6f}\n")
        elif res == False:
            print(f"Число {i} составное \n")
        
def test_sol_str():
    print("test_sol_str")

    test_data = [2, 3, 5, 7, 11, 1, 9, 10, 12, 14, 15, 17, 19, 21,
                 561, 8911, 10585, 15841, 29341, 41041,
                 59283834566841846300814233552512955601,
                 9904788899632347103]
    t = 2
    #t = 5
    
    for i in test_data:
        res = SolovayStrassen(int(i), t)
        if res == True:
            err = error_f_sol_st(t)
            print(f"Число {i} простое. Вероятность ошибки = {err:.6f}\n")
        elif res == False:
            print(f"Число {i} составное \n")

def compare_tests():
    print("сравнение")
    
    test_data = [5, 7, 11, 9, 10, 12, 14, 15, 17, 19, 21,
                 561, 8911, 10585, 15841, 29341, 41041,
                 59283834566841846300814233552512955601,
                 9904788899632347103]
    t = 2
    #t = 10
    
    for i in test_data:
        # Тест Миллера-Рабина
        res_mr = MillerRabin(int(i), t)
        if res_mr == True:
            err_mr = error_f_mil_rab(t)
            print(f"{i} -> простое (ошибка = {err_mr:.6f}) - MilRab")
        elif res_mr == False:
            print(f"{i} -> составное - MilRab")
        else:
            print(f"{i} -> не подходит - MilRab")
        
        # Тест Соловея-Штрассена
        res_ss = SolovayStrassen(int(i), t)
        if res_ss == True:
            err_ss = error_f_sol_st(t)
            print(f"{i} -> простое (ошибка = {err_ss:.6f}) - SolStr")
        elif res_ss == False:
            print(f"{i} -> составное - SolStr")
        else:
            print(f"{i} -> не подходит - SolStr")
        
        print()  # пустая строка между числами


def main():
    # test_mil_rab()
    # test_sol_str()
    compare_tests()

if __name__ == "__main__":
    main()
