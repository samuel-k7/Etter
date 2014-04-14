int a = 6;
int b;
int c = 3;

int f1() {
    return a;
}

int f2() {
    b = 7;
    a = 5;
}

int f3(int m) {
    return m*c;
}

int main() {
    int z = 8;
    int y = f1(); // y = 6
    int x; // x = 0
    
    z = z + f1(); // z = 14
    
    print(y); // print(6);
    print(z); // print(14);

    print(f3(4)); // print(12);

    f2(); // a = 5; b = 7

    c = 10;

    x = f3(5) + a + x; // x = 55

    print(x); // 
}
