#include <limits>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <random>
#include <stdexcept>
#include <chrono>

using namespace std;
using namespace chrono;

typedef unsigned short BASE;
typedef unsigned long long int DBASE;

//по сути используем за BASE 16 бит а за DBASE 64 и сделано это для безопасности умножения 
//потому что если перемножать числа размером 32 бита нам лучше взять размерность 128 бита чтобы сделать код безопасным 

#define BASE_SIZE (sizeof(BASE) * 8)

#define SAFE_MULTIPLY(a, b) (static_cast<DBASE>(a) * static_cast<DBASE>(b))
#define SAFE_ADD(a, b) (static_cast<DBASE>(a) + static_cast<DBASE>(b))
#define SAFE_SUB(a, b) (static_cast<DBASE>(a) - static_cast<DBASE>(b))
#define BASENUM ((DBASE)1 << BASE_SIZE)

constexpr DBASE TB = std::numeric_limits<BASE>::max();

using namespace std;

class BigNumber
{
    bool sign;
    vector<BASE> coefs;

public:
    // Конструктор и деструктор
    BigNumber();
    BigNumber(long long int num, int mode = 1);
    BigNumber(string& str, int mode = 1);
    // Конструктор копирования
    BigNumber(const BigNumber& obj)
        : sign(obj.sign), coefs(obj.coefs) {
    }

    // Конструктор перемещения
    BigNumber(BigNumber&& obj)
        : sign(obj.sign), coefs(std::move(obj.coefs)) {
    }

    ~BigNumber() = default;

    // операции сравнения
    bool operator==(BigNumber& BigNumber);
    bool operator!=(BigNumber& BigNumber);
    bool operator>(const BigNumber& BigNumber) const;
    bool operator>=(const BigNumber& BigNumber) const;
    bool operator<(const BigNumber& BigNumber) const;
    bool operator<=(BigNumber& BigNumber);

    // другое
    int getCoefLength() const;
    vector<BASE> getCoefs() const;
    bool getSign() const;
    void reverseSign();
    void deleteLeadZeros();
    void square();


    // Теоретико числовые методы
    //быстрое возведение в квадрат 
    BigNumber fastSquare();

    //дихотомический алгоритм возведения в степень
    BigNumber fastPow(DBASE power);
    vector<BASE> getReverseBinary(DBASE num);

    // Алгоритм Барретта для приведения по модулю
    BigNumber BarrettReduction(BigNumber mod, BigNumber constZ);
    BigNumber computeBarrettConstZ(BigNumber mod);
    void TestBarrettReduction();
    void TestBarrettReductionBasic();


    BigNumber getSum(BigNumber& other);
    BigNumber getDifference(BigNumber& other);
    BigNumber getDiv(BigNumber& other);
    BigNumber getBase()
    {
        BigNumber base;
        base.coefs.push_back(1);
        base.sign = 0;
        return base;
    }
    // другие операции
    BigNumber operator=(const BigNumber& BigNumber);

    BigNumber operator+(BigNumber& other);
    BigNumber operator+(const BASE& num);
    BigNumber operator+=(const BASE& num);
    BigNumber operator+=(BigNumber& other);

    BigNumber operator-(BigNumber& other);
    BigNumber operator-(const BASE& num);
    BigNumber operator-=(const BASE& num);
    BigNumber operator-=(BigNumber& other);

    BigNumber operator*(const BASE& num);
    BigNumber operator*(const BigNumber& other) const;
    BigNumber operator*=(const BigNumber& other);

    BigNumber operator/(const BASE& num);
    BigNumber operator/(BigNumber& other);
    BigNumber operator%(BigNumber& other);

    bool operator==(const BigNumber& other) const;
    BigNumber operator/=(const BASE& num);
    BigNumber operator%(const BASE& num);
    // ввод вывод
    string HexString() const;
    void OutputInHex() const;
    string DecString() const;
    BigNumber OutputInDec(string& str);

    BigNumber shift(int x)
    {
        BigNumber result;

        if (x > 0)
        {
            result.coefs.reserve(coefs.size() + x);
            result.coefs.resize(x, 0);
            result.coefs.insert(result.coefs.end(),
                coefs.begin(), coefs.end());
        }
        else if (x < 0)
        {
            int shift_size = static_cast<int>(coefs.size()) + x;
            if (shift_size <= 0)
            {
                result.coefs.reserve(1);
                result.coefs.push_back(0);
            }
            else
            {
                result.coefs.reserve(shift_size);
                result.coefs.insert(result.coefs.end(),
                    coefs.begin() - x,
                    coefs.end());
            }
        }
        else
        {
            result.coefs.reserve(coefs.size());
            result.coefs = coefs;
        }

        result.deleteLeadZeros();
        result.sign = this->sign;
        return result;
    }

    friend ostream& operator<<(ostream&, const BigNumber&);
    friend istream& operator>>(istream&, BigNumber&);
};


// конструкторы и деструкторы
BigNumber::BigNumber()
{
    coefs.push_back(0);
    sign = 0;
}

BigNumber::BigNumber(long long int num, int mode)
{
    if (mode == 1)
    {
        sign = 0;
        coefs.resize(2 * num, 0);
    }
    else
    {
        if (num < 0)
        {
            exit(-1);
        }
        else if (num == 0)
        {
            coefs.push_back(0);
            sign = 0;
        }
        else
        {
            sign = 0;
            coefs.resize(num);
            // Вихрь Мерсенна
            random_device rd;
            // используем аппаратные шумы
            mt19937_64 gen(rd());

            int i = 0;
            while (i < num - 1)
            {
                coefs[i] = (BASE)gen();
                i++;
            }
            do
            {
                coefs[num - 1] = (BASE)gen();
            } while (coefs[num - 1] == 0);
        }
    }
}

BigNumber::BigNumber(string& str, int mode)
{
    if (mode == 0) {
        int j = 0;
        BASE tmp;
        int k = 0;
        if (str.size() > 1)
        {
            if (str[0] == '-')
            {
                sign = 1;
                str.erase(0, 1);
            }
            else
            {
                sign = 0;
            }
        }
        coefs.push_back(0);
        for (int i = str.length() - 1; i >= 0; i--)
        {
            if (k >= BASE_SIZE)
            {
                coefs.push_back(0);
                k = 0;
                j++;
            }
            if (str[i] == '_')
            {
                i--;
            }
            if (str[i] >= '0' && str[i] <= '9')
            {
                tmp = str[i] - '0';
            }
            else if (tolower(str[i]) >= 'a' && tolower(str[i]) <= 'f')
            {
                tmp = str[i] - 'a' + 10;
            }
            else
            {
                cerr << "symbol is not correct ";
                exit(-1);
            }
            coefs[j] |= tmp << k;
            k += 4;
        }
        this->deleteLeadZeros();
    }
    else (*this) = OutputInDec(str);
}

bool BigNumber::operator==(BigNumber& BigNumber)
{
    if (this->getCoefLength() == BigNumber.getCoefLength() &&
        this->sign == BigNumber.sign)
    {
        for (int i = 0; i < this->getCoefLength(); i++)
        {
            if (this->coefs[i] != BigNumber.coefs[i])
            {
                return false;
            }
        }
    }
    else
    {
        return false;
    }
    return true;
}

bool BigNumber::operator!=(BigNumber& BigNumber)
{
    return this->operator==(BigNumber) ? 0 : 1;
}

bool BigNumber::operator>(const BigNumber& other) const
{
    if (this->sign < other.sign)
    {
        return 1;
    }
    if (this->sign > other.sign)
    {
        return 0;
    }

    if (this->getCoefLength() > other.getCoefLength())
    {
        return !this->sign;
    }
    else if (this->getCoefLength() < other.getCoefLength())
    {
        return this->sign;
    }
    else
    {
        for (int i = this->getCoefLength() - 1; i >= 0; i--)
        {
            if (this->coefs[i] > other.coefs[i])
            {
                return !this->sign;
            }
            else if (this->coefs[i] < other.coefs[i])
            {
                return this->sign;
            }
        }
        return false;
    }
}

bool BigNumber::operator>=(const BigNumber& other) const
{
    if (this->sign < other.sign)
    {
        return 1;
    }
    else if (this->sign > other.sign)
    {
        return 0;
    }

    if (this->getCoefLength() > other.getCoefLength())
    {
        return !this->sign;  // Если положительное то true иначе false
    }
    else if (this->getCoefLength() < other.getCoefLength())
    {
        return this->sign;  // Если отрицательное то true иначе false
    }
    else
    {
        for (int i = this->getCoefLength() - 1; i >= 0; i--)
        {
            if (this->coefs[i] > other.coefs[i])
            {
                return !this->sign;  // Если положительное то true иначе false
            }
            else if (this->coefs[i] < other.coefs[i])
            {
                return this->sign;  // Если отрицательное то true иначе false
            }
        }
        return true;
    }
}

bool BigNumber::operator<(const BigNumber& other) const
{
    if (this->sign < other.sign)
    {
        return 0;
    }
    else if (this->sign > other.sign)
    {
        return 1;
    }
    if (this->getCoefLength() > other.getCoefLength())
    {
        return this->sign;
    }
    else if (this->getCoefLength() < other.getCoefLength())
    {
        return !this->sign;
    }
    else
    {
        for (int i = this->getCoefLength() - 1; i >= 0; i--)
        {
            if (this->coefs[i] < other.coefs[i])
            {
                return !this->sign;
            }
            else if (this->coefs[i] > other.coefs[i])
            {
                return this->sign;
            }
        }
        return false;
    }
}

bool BigNumber::operator<=(BigNumber& other)
{
    if (this->sign < other.sign)
    {
        return 0;
    }
    else if (this->sign > other.sign)
    {
        return 1;
    }

    if (this->getCoefLength() > other.getCoefLength())
    {
        return this->sign;  // Если положительное то true иначе false
    }
    else if (this->getCoefLength() < other.getCoefLength())
    {
        return !this->sign;  // Если отрицательное то true иначе false
    }
    else
    {
        for (int i = this->getCoefLength() - 1; i >= 0; i--)
        {
            if (this->coefs[i] < other.coefs[i])
            {
                return !this->sign;  // Если положительное то true иначе false
            }
            else if (this->coefs[i] > other.coefs[i])
            {
                return this->sign;  // Если отрицательное то true иначе false
            }
        }
        return true;
    }
}

int BigNumber::getCoefLength() const
{
    return coefs.size();
}

vector<BASE> BigNumber::getCoefs() const
{
    return coefs;
}

bool BigNumber::getSign() const
{
    return sign;
}

void BigNumber::reverseSign()
{
    this->sign = !sign;
}

void BigNumber::deleteLeadZeros()
{
    while (!coefs.empty() && static_cast<DBASE>(coefs.back()) == 0)
    {
        coefs.pop_back();
    }
    if (coefs.empty())
    {
        coefs.push_back(0);
        sign = false;
    }
}

void BigNumber::square()
{
    this->sign = 0;
    *this = *this * *this;
}

BigNumber BigNumber::getSum(BigNumber& other)
{
    int lenMax = max(this->coefs.size(), other.coefs.size());
    int lenMin = min(this->coefs.size(), other.coefs.size());

    BASE carry = 0;
    DBASE getSum;

    BigNumber result;
    result.sign = this->sign;
    result.coefs.resize(lenMax);
    for (size_t i = 0; i < lenMin; ++i)
    {
        DBASE getSum =
            SAFE_ADD(SAFE_ADD(this->coefs[i], other.coefs[i]), carry);
        if (getSum <
            static_cast<DBASE>(this->coefs[i]))
        {
            throw std::overflow_error("Addition overflow");
        }
        result.coefs[i] = static_cast<BASE>(getSum);
        carry = getSum >> BASE_SIZE;
    }
    for (size_t i = lenMin; i < this->coefs.size(); ++i)
    {
        getSum = (DBASE)this->coefs[i] + carry;
        result.coefs[i] = (BASE)getSum;
        carry = getSum >> BASE_SIZE;
    }
    for (size_t i = lenMin; i < other.coefs.size(); ++i)
    {
        getSum = (DBASE)other.coefs[i] + carry;
        result.coefs[i] = (BASE)getSum;
        carry = getSum >> BASE_SIZE;
    }
    if (carry > 0)
    {
        result.coefs.push_back(carry);
    }
    result.deleteLeadZeros();
    return result;
}

BigNumber BigNumber::getDifference(BigNumber& other)
{
    if (this->operator==(other))
    {
        return BigNumber();
    }
    bool whoLarger;
    if (this->sign == 1 && other.sign == 1)
    {
        whoLarger = other > *this;
    }
    else {
        whoLarger = *this > other;
    }

    BigNumber largBigNumber = whoLarger ? *this : other;
    BigNumber smallBigNumber = !whoLarger ? *this : other;
    size_t minLen = min(this->getCoefLength(), other.getCoefLength());
    size_t maxLen = max(this->getCoefLength(), other.getCoefLength());

    BASE adder = 0;
    BigNumber result;

    result.sign = whoLarger ? this->sign : !this->sign;

    DBASE tmp;
    result.coefs.resize(maxLen);
    int j = 0;
    BASE k = 0;
    while (j < minLen)
    {
        tmp = ((DBASE) static_cast<DBASE>(1) << BASE_SIZE);
        tmp |= largBigNumber.coefs[j];
        tmp = tmp - smallBigNumber.coefs[j] - k;
        result.coefs[j] = (BASE)tmp;
        k = !(tmp >> BASE_SIZE);
        j++;
    }
    while (j < maxLen)
    {
        tmp = ((DBASE) static_cast<DBASE>(1) << BASE_SIZE) |
            largBigNumber.coefs[j];
        tmp -= k;
        result.coefs[j] = (BASE)tmp;
        k = !(tmp >> BASE_SIZE);
        j++;
    }
    if (result.coefs.size() == 1 && result.coefs.back() == 0)
    {
        result.sign = false;
    }

    result.deleteLeadZeros();
    return result;
}

inline BigNumber BigNumber::getDiv(BigNumber& other)
{
    BigNumber result;
    return BigNumber();
}

BigNumber BigNumber::operator=(const BigNumber& BigNumber)
{
    this->coefs = BigNumber.coefs;
    this->sign = BigNumber.sign;
    return *this;
}

BigNumber BigNumber::operator+(BigNumber& other)
{
    BigNumber maxim;
    BigNumber num;
    if (this->operator>(other))
    {
        maxim = *this;
        num = other;
    }
    else
    {
        maxim = other;
        num = *this;
    }
    if (this->sign == other.sign)
    {
        return this->getSum(other);
    }
    else
    {
        num.reverseSign();
        return maxim.getDifference(num);
    }
}

BigNumber BigNumber::operator+(const BASE& num)
{
    BigNumber result = *this;
    DBASE carry = num;
    for (size_t i = 0; i < result.coefs.size() && carry > 0; ++i)
    {
        carry += static_cast<DBASE>(result.coefs[i]);
        result.coefs[i] = static_cast<BASE>(carry);
        carry >>= BASE_SIZE;
    }
    if (carry > 0)
    {
        result.coefs.push_back(static_cast<BASE>(carry));
    }
    result.deleteLeadZeros();
    return result;
}

BigNumber BigNumber::operator+=(const BASE& num)
{
    *this = *this + num;
    return *this;
}

BigNumber BigNumber::operator+=(BigNumber& other)
{
    *this = *this + other;
    return *this;
}

BigNumber BigNumber::operator-(BigNumber& other)
{
    BigNumber temp = other;
    if (this->sign == other.sign)
    {
        return this->getDifference(other);
    }
    else
    {
        temp.reverseSign();
        return this->getSum(temp);
    }
}

BigNumber BigNumber::operator-(const BASE& num)
{
    BigNumber result = *this;

    if (result < BigNumber(num))
    {
        throw std::invalid_argument("Negative result not supported");
    }

    BASE borrow = num;
    int i = 0;

    while (borrow > 0 && i < result.coefs.size())
    {
        if (result.coefs[i] >= borrow)
        {
            result.coefs[i] -= borrow;
            borrow = 0;
        }
        else
        {
            result.coefs[i] = (BASE)((static_cast<DBASE>(1) << BASE_SIZE) +
                result.coefs[i] - borrow);
            borrow = static_cast<BASE>(1);
        }
        ++i;
    }

    result.deleteLeadZeros();
    return result;
}

BigNumber BigNumber::operator-=(const BASE& num)
{
    *this = *this - num;
    return *this;
}

BigNumber BigNumber::operator-=(BigNumber& other)
{
    *this = *this - other;
    return *this;
}

BigNumber BigNumber::operator*(const BASE& num)
{
    if (num == 0) return BigNumber();
    if (num == 1) return *this;

    BigNumber result;
    result.sign = this->sign;
    result.coefs.resize(this->coefs.size() + 1, 0);

    DBASE carry = 0;
    const DBASE base = static_cast<DBASE>(1) << BASE_SIZE;

    for (size_t i = 0; i < this->coefs.size(); ++i)
    {
        carry = SAFE_ADD(carry, SAFE_MULTIPLY(this->coefs[i], num));
        result.coefs[i] = static_cast<BASE>(carry % base);
        carry /= base;
    }

    if (carry > 0)
    {
        if (carry > TB)
        {
            throw std::overflow_error("Multiplication overflow");
        }
        result.coefs[this->coefs.size()] = static_cast<BASE>(carry);
    }
    else
    {
        result.coefs.pop_back();
    }

    result.deleteLeadZeros();
    return result;
}

BigNumber BigNumber::operator*(const BigNumber& other) const
{
    BigNumber result;
    result.coefs.resize(
        this->coefs.size() + other.coefs.size(), 0);
    result.sign = this->sign ^ other.sign;

    const DBASE base = static_cast<DBASE>(1) << BASE_SIZE;

    for (size_t i = 0; i < this->coefs.size(); ++i)
    {
        DBASE carry = 0;
        for (size_t j = 0; j < other.coefs.size(); ++j)
        {
            DBASE product = static_cast<DBASE>(this->coefs[i]) *
                static_cast<DBASE>(other.coefs[j]) +
                result.coefs[i + j] + carry;

            result.coefs[i + j] = static_cast<BASE>(product % base);
            carry = product / base;
        }
        if (carry > 0)
        {
            result.coefs[i + other.coefs.size()] +=
                static_cast<BASE>(carry);
        }
    }
    while (result.coefs.size() > 1 && result.coefs.back() == 0)
    {
        result.coefs.pop_back();
    }
    if (result.coefs.size() == 1 && result.coefs[0] == 0)
    {
        result.sign = 0;
    }

    return result;
}

BigNumber BigNumber::operator*=(const BigNumber& other)
{
    *this = *this * other;
    return *this;
}

BigNumber BigNumber::operator/(const BASE& num)
{
    if (num == 0)
    {
        throw std::runtime_error("LLLLO");
    }
    BigNumber result;
    result.coefs.resize(this->coefs.size());
    result.sign = this->sign;

    DBASE remainder = 0;
    for (int i = this->coefs.size() - 1; i >= 0; --i)
    {
        remainder =
            (remainder << BASE_SIZE) + static_cast<DBASE>(this->coefs[i]);
        result.coefs[i] = remainder / static_cast<DBASE>(num);
        remainder %= num;
    }

    result.deleteLeadZeros();

    if (result.coefs.size() == 1 && result.coefs[0] == 0)
    {
        result.sign = 0;
    }

    return result;
}

BigNumber BigNumber::operator/=(const BASE& num)
{
    *this = *this / num;
    return *this;
}

BigNumber BigNumber::operator%(const BASE& num)
{
    if (num == 0)
    {
        printf("Остаток на ноль не допустим\n");
        return BigNumber();
    }

    int n = getCoefLength();
    BASE r = 0;
    int j = 0;

    while (j < n)
    {
        DBASE tmp = ((static_cast<DBASE>((r)) << BASE_SIZE) +
            static_cast<DBASE>(coefs[n - 1 - j]));
        r = tmp % num;
        r = (BASE)r;
        j++;
    }
    if (this->sign == 1)
    {
        r = num - r;
    }
    BigNumber result;
    result.sign = 0;
    result.coefs[0] = r;
    result.deleteLeadZeros();

    return result;
}

void BigNumber::OutputInHex() const
{
    bool canWrite = true;
    int k = BASE_SIZE - 4;
    BASE tmp;
    string s = {};
    while (k >= 0)
    {
        tmp = coefs[getCoefLength() - 1] >> k & (0xf);
        if (tmp >= 0 && tmp <= 9)
        {
            if (tmp != 0 && canWrite == false)
            {
                canWrite = true;
            }
            if (canWrite)
            {
                s += (char)tmp + '0';
            }
        }
        else if (tmp >= 10 && tmp <= 15)
        {
            if (canWrite == false)
            {
                canWrite = true;
            }
            s += (char)tmp - 10 + 'a';
        }
        k -= 4;
    }
    k = BASE_SIZE - 4;
    if (getCoefLength() - 2 >= 0)
    {
        s += '_';
    }
    for (int j = getCoefLength() - 2; j >= 0;)
    {
        tmp = coefs[j] >> k & (0xf);
        if (tmp >= 0 && tmp <= 9)
        {
            s += (char)tmp + '0';
        }
        else if (tmp >= 10 && tmp <= 15)
        {
            s += (char)tmp - 10 + 'a';
        }
        k -= 4;
        if (k < 0)
        {
            k = BASE_SIZE - 4;
            j--;
            if (j != -1)
            {
                s += '_';
            }
        }
    }
    if (s.size() == 0)
    {
        s += '0';
    }
    if (sign)
    {
        cout << "stringHex = " << '-' << s << endl;
    }
    else
    {
        cout << "stringHex = " << s << endl;
    }
}

string BigNumber::DecString() const
{
    BigNumber newNum = *this;
    bool minus = false;
    BigNumber zero;
    string s;
    if (newNum.sign == 1)
    {
        newNum.sign = 0;
        minus = true;
    }

    while (newNum != zero)
    {
        BigNumber t = newNum % 10;
        s.push_back(t.coefs[0] + '0');
        newNum = newNum / 10;
    }
    if (s.empty())
    {
        s = "0";
    }
    reverse(s.begin(), s.end());
    if (minus)
    {
        s = "-" + s;
    }
    return s;
}

BigNumber BigNumber::OutputInDec(string& str)
{
    string h = str;
    BigNumber newNum;
    if (h[0] == '-')
    {
        newNum.sign = 1;
        h.erase(0, 1);
    }
    else
    {
        newNum.sign = 0;
    }

    int k = h.length();

    for (int j = 0; j < k; ++j)
    {
        if (isalnum(h[j])) {
            BASE t = h[j] - '0';
            newNum = newNum * 10 + t;
        }
        else
        {
            cerr << "Ошибка номер 13";
            return BigNumber();
        }
    }
    newNum.deleteLeadZeros();
    return newNum;
}

string BigNumber::HexString() const
{
    bool canWrite = false;
    int k = BASE_SIZE - 4;
    BASE tmp;
    string s = {};
    while (k >= 0)
    {
        tmp = coefs[getCoefLength() - 1] >> k & (0xf);
        if (tmp >= 0 && tmp <= 9)
        {
            if (tmp != 0 && canWrite == false)
            {
                canWrite = true;
            }
            if (canWrite)
            {
                s += (char)tmp + '0';
            }
        }
        else if (tmp >= 10 && tmp <= 15)
        {
            if (canWrite == false)
            {
                canWrite = true;
            }
            s += (char)tmp - 10 + 'a';
        }
        k -= 4;
    }
    k = BASE_SIZE - 4;

    if (getCoefLength() - 2 >= 0)
    {
        s += '_';
    }
    for (int j = getCoefLength() - 2; j >= 0;)
    {
        tmp = coefs[j] >> k & (0xf);

        if (tmp >= 0 && tmp <= 9)
        {
            s += (char)tmp + '0';
        }
        else if (tmp >= 10 && tmp <= 15)
        {
            s += (char)tmp - 10 + 'a';
        }
        k -= 4;
        if (k < 0)
        {
            k = BASE_SIZE - 4;
            j--;
            if (j != -1)
            {
                s += '_';
            }
        }
    }
    if (s.size() == 0)
    {
        s += '0';
    }
    return s;
}

BigNumber BigNumber::operator/(BigNumber& other)
{
    if (other == BigNumber())
    {
        throw std::runtime_error("Division by zero");
    }
    if (*this < other)
    {
        return BigNumber();
    }
    if (*this == other)
    {
        BigNumber res;
        res.coefs.back() = 1;
        return res;
    }
    if (other.coefs.size() == 1)
    {
        return *this / other.coefs[0];
    }

    const int n = other.coefs.size();
    const int m = coefs.size() - n;

    // Нормализация
    BASE d = static_cast<BASE>((static_cast<DBASE>(1) << BASE_SIZE) /
        (static_cast<DBASE>(other.coefs.back()) + 1));
    BigNumber u = *this * d;  // делимое
    BigNumber v = other * d;  // делитель
    BigNumber q;

    q.coefs.resize(m + 1, 0);
    q.sign = this->sign ^ other.sign;
    if (q.coefs.size() == 1 && q.coefs[0] == 0) q.sign = 0;

    if (coefs.size() == u.coefs.size())
    {
        u.coefs.push_back(0);
    }
    for (int j = m; j >= 0; --j)
    {
        if (j + n >= u.coefs.size())
        {
            while (j + n >= u.coefs.size())
            {
                u.coefs.push_back(0);
            }

        }
        DBASE qhat = (static_cast<DBASE>(u.coefs[j + n]) << BASE_SIZE) +
            u.coefs[j + n - 1];
        qhat /= v.coefs[n - 1];

        DBASE rhat = (static_cast<DBASE>(u.coefs[j + n]) << BASE_SIZE) +
            u.coefs[j + n - 1];
        rhat %= v.coefs[n - 1];
        if ((qhat == BASENUM) ||
            (qhat * v.coefs[n - 2] >
                ((rhat << BASE_SIZE) + u.coefs[j + n - 2])))
        {
            qhat--;
            rhat += v.coefs[n - 1];
        }
        if (rhat < BASENUM)
        {
            if ((qhat == BASENUM) ||
                (qhat * v.coefs[n - 2] >
                    ((rhat << BASE_SIZE) + u.coefs[j + n - 2])))
            {
                qhat--;
                rhat += v.coefs[n - 1];
            }
        }
        BigNumber temp = v * static_cast<BASE>(qhat);
        temp = temp.shift(j);

        if (u < temp)
        {
            qhat--;
            temp = v * static_cast<BASE>(qhat);
            temp = temp.shift(j);
        }
        u = u - temp;
        q.coefs[j] = static_cast<BASE>(qhat);
    }

    q.deleteLeadZeros();
    return q;
}

BigNumber BigNumber::operator%(BigNumber& other)
{
    if (other == BigNumber())
    {
        throw std::runtime_error("Modulo by zero");
    }
    if (*this < other)
    {
        return *this;
    }
    if (*this == other)
    {
        return BigNumber();
    }
    if (other.coefs.size() == 1)
    {
        return *this % other.coefs[0];
    }

    const int n = other.coefs.size();
    const int m = coefs.size() - n;

    BASE d =
        static_cast<BASE>((static_cast<DBASE>(1) << BASE_SIZE) /
            (static_cast<DBASE>(other.coefs.back()) + 1));

    BigNumber u = *this * d;
    u.sign = this->sign;
    if (u.coefs.size() == 1 && u.coefs[0] == 0) u.sign = 0;

    BigNumber v = other * d;
    if (coefs.size() == u.coefs.size())
    {
        u.coefs.push_back(0);
    }

    for (int j = m; j >= 0; --j)
    {
        if (j + n >= u.coefs.size())
        {
            while (j + n >= u.coefs.size())
            {
                u.coefs.push_back(0);
            }
        }
        DBASE qhat = (static_cast<DBASE>(u.coefs[j + n]) << BASE_SIZE) +
            u.coefs[j + n - 1];
        qhat /= v.coefs[n - 1];

        DBASE rhat = (static_cast<DBASE>(u.coefs[j + n]) << BASE_SIZE) +
            u.coefs[j + n - 1];
        rhat %= v.coefs[n - 1];

        if ((qhat == BASENUM) ||
            (qhat * v.coefs[n - 2] >
                ((rhat << BASE_SIZE) + u.coefs[j + n - 2])))
        {
            qhat--;
            rhat += v.coefs[n - 1];
        }
        if (rhat < BASENUM)
        {
            if ((qhat == BASENUM) ||
                (qhat * v.coefs[n - 2] >
                    ((rhat << BASE_SIZE) + u.coefs[j + n - 2])))
            {
                qhat--;
                rhat += v.coefs[n - 1];
            }
        }

        BigNumber temp = v * static_cast<BASE>(qhat);
        temp = temp.shift(j);

        if (u < temp)
        {
            qhat--;
            temp = v * static_cast<BASE>(qhat);
            temp = temp.shift(j);
        }

        u = u - temp;
    }

    u = u / d;
    u.deleteLeadZeros();

    return u;
}

void TEST()
{
    int max_L = 1000;
    int Nam = 1000;
    BigNumber A, D, Q, R;
    bool f1 = 0;
    bool f2 = 0;
    bool f3 = 0;
    srand(time(NULL));
    do
    {
        std::cout << "n = " << Nam << endl;

        int len_A = rand() % max_L + 1;
        int len_D = rand() % max_L + 1;
        BigNumber M(len_A);
        BigNumber N(len_D);
        BigNumber B = M / N;
        BigNumber V = M % N;
        A = M;
        D = N;
        Q = B;
        R = V;

        BigNumber U = Q * D + R;
        BigNumber U2 = A - R;
        BigNumber U3 = Q * D;

        f1 = 0;
        f2 = 0;
        f3 = 0;
        if (A == ((Q * D) + R))
        {
            f1 = 1;
        }
        if (A - R == (Q * D))
        {
            f2 = 1;
        }
        if (R < D)
        {
            f3 = 1;
        }
        if (f1 != 1 || f2 != 1 || f3 != 1)
        {
            cout << len_A << " " << len_D << endl;
            cout << "A" << endl;
            A.deleteLeadZeros();
            D.deleteLeadZeros();
            Q.deleteLeadZeros();
            R.deleteLeadZeros();
            U3.deleteLeadZeros();
            U.deleteLeadZeros();
            U2.deleteLeadZeros();
            cout << A.DecString() << endl;
            cout << "D" << endl;
            cout << D.DecString() << endl;
            cout << "Q" << endl;
            cout << Q.DecString() << endl;
            cout << "R" << endl;
            cout << R.DecString() << endl;
            cout << "A" << endl;
            cout << A.DecString() << endl;
            cout << "D" << endl;
            cout << D.DecString() << endl;
            cout << "U3" << endl;
            cout << U3.DecString() << endl;
            cout << "1" << endl;
            cout << A.DecString() << endl << " == " << endl << U.DecString() << endl;
            cout << "2" << endl;
            cout << U2.DecString() << endl
                << " == " << endl
                << U3.DecString() << endl;
            cout << R.DecString() << " < " << D.DecString() << endl;
            cout << "Q" << endl;
            cout << Q.DecString() << endl;

            cout << "R" << endl;
            cout << R.DecString() << endl;
        }
    } while (f1 && f2 && f3 && --Nam);
}


bool BigNumber::operator==(const BigNumber& other) const
{
    if (coefs.size() != other.coefs.size())
    {
        return false;
    }
    for (int i = 0; i < coefs.size(); i++)
    {
        if (coefs[i] != other.coefs[i])
        {
            return false;
        }
    }
    return true;
}

//вывод
ostream& operator<<(ostream& out, const BigNumber& obj)
{
    BigNumber temp = obj;

    out << temp.DecString();
    return out;
}

//ввод
istream& operator>>(istream& in, BigNumber& obj)
{
    string tmp;
    in >> tmp;

    bool isNegative = false;
    if (tmp[0] == '-')
    {
        isNegative = true;
        tmp = tmp.substr(1);
    }

    if (tmp.empty())
    {
        cerr << "string is empty\n";
        exit(-4);
    }

    for (char c : tmp)
    {
        if (!isdigit(c))
        {
            cerr << "input error onle digits!!!\n";
            exit(-4);
        }
    }

    bool isZero = true;
    for (char c : tmp)
    {
        if (c != '0')
        {
            isZero = false;
            break;
        }
    }

    if (isZero) {
        obj = BigNumber();
        return in;
    }

    obj = BigNumber();

    for (char c : tmp) {
        int digit = c - '0';
        obj = obj * 10 + digit;
    }

    if (isNegative &&
        !(obj.coefs.size() == 1 && obj.coefs[0] == 0)) {
        obj.sign = 1;
    }

    obj.deleteLeadZeros();
    return in;
}

BigNumber BigNumber::fastSquare()
{
    long long int num_size = this->getCoefLength();
    long long int res_size = 2 * num_size + 1;
    BigNumber res;

    if (num_size == 0)
    {
        cout << "ERROR size == 0" << endl;
        exit(1);
    }

    if ((*this) == 0 || (*this) == 1) return (*this);

    res.coefs.resize(res_size, 0);
    res.sign = false;

    for (int i = 0; i < num_size; i++)
    {
        //шаг 2.1
        DBASE uv = static_cast<DBASE>(res.coefs[2 * i]) +
            static_cast<DBASE>(this->coefs[i]) *
            static_cast<DBASE>(this->coefs[i]);

        res.coefs[2 * i] = (BASE)(uv & (BASENUM - 1));

        DBASE cu = uv >> BASE_SIZE;


        //шаг 2.2 
        for (int j = i + 1; j < num_size; j++)
        {
            DBASE ai_aj = static_cast<DBASE>(this->coefs[i]) *
                static_cast<DBASE>(this->coefs[j]);

            uv = static_cast<DBASE>(res.coefs[i + j]) +
                static_cast<DBASE>(static_cast<BASE>(ai_aj) * 2) +
                static_cast<DBASE>(static_cast<BASE>(cu));

            res.coefs[i + j] = (BASE)(uv & (BASENUM - 1));

            cu = static_cast<DBASE>((static_cast<DBASE>(ai_aj) >> BASE_SIZE) *
                static_cast<DBASE>(2)) + static_cast<DBASE>(static_cast<DBASE>(cu) >> BASE_SIZE) +
                static_cast<DBASE>(static_cast<DBASE>(uv) >> BASE_SIZE);
        }

        //шаг 2.3
        res.coefs[i + num_size] += static_cast<BASE>(cu);
        res.coefs[i + num_size + 1] += static_cast<BASE>(cu >> BASE_SIZE);
    }

    res.deleteLeadZeros();

    //шаг 3
    return res;
}

vector<BASE> BigNumber::getReverseBinary(DBASE num) 
{
    vector<BASE> bits;

    while (num != 0) {
        bits.push_back(num % 2);
        num /= 2;
    }

    return bits;
}

BigNumber BigNumber::fastPow(DBASE power) {
    string hex = "1";
    BigNumber one(hex);
    BigNumber result = one;

    vector<BASE> binaryPower = getReverseBinary(power);

    if (power < 0 || this->getCoefLength() == 0) {
        cout << "Error!: fastPow()" << endl;
        exit(1);
    }


    if (power == 0) return one;

    if ((*this) == 0 || (*this) == 1) {
        result = (*this);
        result.sign = false;
        return result;
    }

    if (this->sign && power % 2 == 0) this->sign = false;

    BigNumber q = *this;

    if (binaryPower[0]) result = q;

    // Справа-налево
    for (int i = 1; i < binaryPower.size(); i++) {
        q = q.fastSquare();

        if (binaryPower[i]) result = result * q;
    }

    return result;
}
//алгоритм барретта
BigNumber computeBarrettConstZ(BigNumber mod) {
    if (mod.getCoefLength() == 0) {
        cout << "Error!: in computeBarrettConstZ()\n";
        cout << "m is empty!\n";
        exit(1);
    }

    if (mod == BigNumber()) {
        cout << "Error!: in computeBarrettConstZ()\n";
        cout << "m == 0\n";
        exit(1);
    }

    if (mod.getSign()) mod.reverseSign();

    BigNumber base;
    base = base.getBase();
    DBASE k = mod.getCoefLength();

    return base.fastPow(2 * k) / mod;
}


BigNumber BigNumber::BarrettReduction(BigNumber mod, BigNumber constZ) {
    DBASE k = mod.getCoefLength();    // к-во цифр в модуле
    DBASE n = this->getCoefLength();  // к-во цифр в самом числе

    // Условие для алгоритма
    if (n > 2 * k) {
        cout << "Error!: BarrettReduction()\n";
        cout << "Necessary condition: n <= 2k\n";
        exit(1);
    }

    if (mod.getSign() || this->getSign()) {
        cout << "Error!: BarretReduction()\n";
        cout << "Necessary condition: numbers must be positive\n";
        exit(1);
    }

    // Модуль равен 0
    if (mod == BigNumber()) {
        cout << "Error!: BarrettReduction()\n";
        cout << "m == 0\n";
        exit(1);
    }

    if ((*this) == BigNumber()) {
        return BigNumber();
    }

    // Если модуль больше самого числа 
    if (mod > (*this)) return (*this);

    // Если число и модуль равны
    if (mod == (*this)) return BigNumber();


    BigNumber base = getBase();
    BigNumber baseInKplus1 = base.fastPow(k + 1);
    BigNumber baseInKminus1 = base.fastPow(k - 1);

    BigNumber tmp1 = (*this) / baseInKminus1;
    BigNumber qhat = (tmp1 * constZ) / baseInKplus1;

    // cout << "qhat " << qhat.DecString() << endl;

    BigNumber r1 = *this % baseInKplus1;
    BigNumber r2 = (qhat * mod) % baseInKplus1;

    BigNumber r = r1 - r2;

    if (r1 < r2) r += baseInKplus1;

    while (r >= mod) r -= mod;

    return r;
}


void TestBarrettReduction() {
    DBASE n = 1000;
    DBASE m = 750;

    BigNumber num(n, 2);
    BigNumber mod(m, 2);

    BigNumber constZ = BigNumber();

    constZ = computeBarrettConstZ(mod);

    auto start = chrono::high_resolution_clock::now();

    BigNumber result = num.BarrettReduction(mod, constZ);

    cout << result << endl;

    auto end = chrono::high_resolution_clock::now();

    auto duration = chrono::duration_cast<chrono::milliseconds>(end - start);

    cout << "Time: " << duration.count() << "ms" << endl;

    if (result == num % mod) {
        cout << "\n\nOK!\n";
    }
    else { cout << "\n\nNOT OK!\n"; }

}

void TestBarrettReductionBasic()
{
    cout << "test1" << endl;
    string num_str = "123456789";
    string mod_str = "1";

    BigNumber num(num_str, 1);
    BigNumber mod(mod_str, 1);

    BigNumber const1 = computeBarrettConstZ(mod);
    BigNumber result = num.BarrettReduction(mod, const1);
    BigNumber expected = num % mod;

    cout << "num: " << num << endl;
    cout << "mod: " << mod << endl;
    cout << "res_bar: " << result << endl;
    cout << "res: " << expected << endl << endl;

    cout << "test2" << endl;
    string num_str2 = "42";
    string mod_str2 = "100";

    BigNumber num2(num_str2, 1);
    BigNumber mod2(mod_str2, 1);

    BigNumber const2 = computeBarrettConstZ(mod2);
    BigNumber result2 = num2.BarrettReduction(mod2, const2);
    BigNumber expected2 = num2 % mod2;

    cout << "num: " << num2<< endl;
    cout << "mod: " << mod2 << endl;
    cout << "res_bar: " << result2 << endl;
    cout << "res: " << expected2 << endl << endl;


    cout << "test3" << endl;
    string num_mod_str = "500";

    BigNumber num3(num_mod_str, 1);
    BigNumber mod3(num_mod_str, 1);

    BigNumber const3 = computeBarrettConstZ(mod3);
    BigNumber result3 = num3.BarrettReduction(mod3, const3);
    BigNumber expected3 = num3 % mod3;

    cout << "num: " << num3 << endl;
    cout << "mod: " << mod3 << endl;
    cout << "res_bar: " << result3 << endl;
    cout << "res: " << expected3 << endl << endl;


    cout << "test4" << endl;
    string num_str4 = "0";
    string mod_str4 = "123";

    BigNumber num4(num_str4, 1);
    BigNumber mod4(mod_str4, 1);

    BigNumber const4 = computeBarrettConstZ(mod4);
    BigNumber result4 = num4.BarrettReduction(mod4, const4);
    BigNumber expected4 = num4 % mod4;

    cout << "num: " << num4 << endl;
    cout << "mod: " << mod4 << endl;
    cout << "res_bar: " << result4 << endl;
    cout << "res: " << expected4 << endl << endl;

}


int main()
{
    string str1 = "12345";
    DBASE num1 = 0;
    BigNumber test1(str1, 1);
    BigNumber test1_res = test1.fastPow(num1);
    cout << "12345 ^ 0 = " << test1_res << endl;
    cout << endl;


    string str2 = "0";
    DBASE num2 = 5;
    BigNumber test2(str2, 1);
    BigNumber test2_res = test2.fastPow(num1);
    BigNumber test2_result_5 = test2.fastPow(num2);

    cout << "0 ^ 0 = " << test2_res << endl;
    cout << "0 ^ 5 = " << test2_res << endl;
    cout << endl;


    string str3 = "1";
    DBASE num3 = 1;
    BigNumber test3(str3, 1);
    BigNumber test3_res = test3.fastPow(num1);
    BigNumber test3_res2 = test3.fastPow(num3);
    
    cout << "1 ^ 0 = " << test3_res << endl;
    cout << "1 ^ 1 = " << test3_res2 << endl;
    cout << endl;


    string str4 = "5";
    DBASE num4 = 15;
    BigNumber test4(str4, 1);
    BigNumber test4_res= test4.fastPow(num4);
    
    cout << "5 ^ 15 = " << test4_res << endl;
    cout << endl;


    string str5 = "1234567";
    DBASE num5 = 20;
    BigNumber test5(str5, 1);
    BigNumber test5_res = test5.fastPow(num5);

    cout << "1234567 ^ 20 = " << test5_res << endl;
    cout << endl;
    

    string str6 = "9999999999999";
    DBASE num6 = 50;
    BigNumber test6(str6, 1);
    BigNumber test6_res = test6.fastPow(num6);

    cout << "999999 ^ 50  = " << test6_res << endl;
    cout << endl;

    TestBarrettReductionBasic();

    TestBarrettReduction();



    return 0;
}
