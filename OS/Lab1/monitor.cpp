#include <iostream>
#include <mutex>
#include <thread>
#include <chrono>
#include <condition_variable>
#include <vector>


using namespace std;
using namespace chrono;
using namespace this_thread;

class mini_string//простая версия класса string
{
    string text;
public:

    mini_string(string s = "") : text(s) {};
    void add_one() { text += '!'; };
    string get_text() { return text; };
    
    void set_string(string s) { text = s; };
    void print_text() { cout << text << endl; };
    
    mini_string(const mini_string& other) = default;
    mini_string(mini_string&& other) = default;

    ~mini_string() = default;
};


class monitor
{
    mini_string* current_ptr = nullptr;
    bool new_data = false;
    bool data_end = false;
    mutex mut;
    condition_variable condition;

public:
    bool data_ready()//функция проверки условия ожидания
    {
        return new_data || data_end;
    }

    void send_string(mini_string* mini_str_ptr)//метод для отправки строки
    {
        unique_lock<mutex> lock(mut);

        current_ptr = mini_str_ptr;

        mini_str_ptr->add_one();
        cout << "potok_maker - ";
        mini_str_ptr->print_text();

        new_data = true;
        condition.notify_one();
        //когда подготавливаем данные мы добавляем поток котоырй ждет изменение условие
    }

    mini_string* wait_string()//получаем строку
    {
        unique_lock<mutex> lock(mut);
        
        condition.wait(lock, [this] { return data_ready(); });//освобождаем mutex и блокируем поток
        
        sleep_for(seconds(2));//задержка

        if (data_end)
        {
            return nullptr;
        }

        cout << "potok_consumer - ";
        current_ptr->print_text();
        new_data = false;

        cout << endl;

        return current_ptr;
    }
    
    void end_work()
    {
        unique_lock<mutex> lock(mut);

        data_end = true;
        condition.notify_one();//разблокируем потребителя
    }

    bool is_finish()
    {
        lock_guard<mutex> lock(mut);
        return data_end;
    }

};

void potok_maker(monitor& monnitor)
{
    vector<mini_string*> strings;

    strings.push_back(new mini_string("Hello word"));//строки для передачи
    strings.push_back(new mini_string("test1"));
    strings.push_back(new mini_string("test2"));
    strings.push_back(new mini_string("end testing"));

    for (auto* str : strings)
    {
        monnitor.send_string(str);
        sleep_for(seconds(2));
    }

    sleep_for(seconds(2));
    monnitor.end_work();//завершаем работу

    for (auto* str : strings)
    {
        delete str;
    }
}

void potok_consumer(monitor& monik)
{
    while (true)
    {
        mini_string* str = monik.wait_string();

        if (str == nullptr)
        {
            cout << "END" << endl;
            break;
        }
    }
}


int main()
{
    monitor monnitor;
    
    thread producer_thread(potok_maker, ref(monnitor));
    thread consumer_thread(potok_consumer, ref(monnitor));

    producer_thread.join();
    consumer_thread.join();
    return 0;
}
