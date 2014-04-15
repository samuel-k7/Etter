int a = 15;
string x = "Hello, GLOBAL!";

int func();

int main()
{
    if(a<10)
    {
        print("Hello, BEFORE!");
    }
    func();

    if(a<10)
    {
        print("Hello, AFTER!");
    }
    print(x);
}

int func()
{
    int b = 5;
    string y = x;
    string x = "Hello, LOCAL!";
    print(y);
    print(x);
    a = 5;
    return b;
}