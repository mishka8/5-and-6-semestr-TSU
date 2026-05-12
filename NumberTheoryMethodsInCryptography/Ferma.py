import random

#тест ферма - вероятностный
#малая теорема ферма


#функция Эйлера - вычисляет количество взаимно простых чисел
def fi(n):
    f = n;
    if n%2 == 0:
        while n%2 == 0:
            n = n // 2;
        f = f // 2;
    i = 3
    while i*i <= n:
        if n%i == 0:
            while n%i == 0:
                n = n // i;
            f = f // i;
            f = f * (i-1);
        i = i + 2;
    if n > 1:
        f = f // n;
        f = f * (n-1);
    return f;

def error_f(n,t):
    i = (fi(n)/n)**t
    print(f'Вероятность ошибки = {i}')

def test_ferma(n, t):
    for i in range(1, t):
        a = random.randint(2, n-2)
        r = pow(a,n-1,n) #a^(n-1) mod n
        
        if r != 1:
            return 1


def test_ferma_wr(n,t):
    if n <= 3:
        print("Число не подходит под условия\n")
    else:

        res = test_ferma(n, t)
        if res == 1:
            print("составное -", n, "\n")
        else:
            print("простое -", n, "\n")
            error_f(n, t)

def test():
    test_data=[2, 3, 5, 7, 11, 1, 9, 10, 12, 14, 1, 561, 8911, 10585, 15841, 29341, 41041]
    #561, 8911, 10585, 15841, 29341, 41041 - чиса Кармайкла
    t = 12
    #t = 2
    #после измениения параметра надёжности выше 3 последние 2 числа Кармайкла определяются
    for i in test_data:
        test_ferma_wr(i, t)


def main():
    print("Ferma Test")
    #n = int(input("Введите число для проверки "))
    #t = int(input("Введите параметр надёжности "))
    #n = 12312313212311
    #t = 12
    #print(n, t)
    #test_ferma_wr(n,t)
    #test_ferma_wr(348251240609926627320927902551,5)
    test()

if __name__ == "__main__":
    main()
