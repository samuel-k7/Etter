int a;
int b;

int fact() {
    int z = a;
    if(a>1) {
        a = a-1;
        return z*fact();
        print("TOTO NEVYPISUJ!");
    }
    return 1;
}

int f2();

int main() {
    a = 8;
    b = fact();
    print(b);
}
