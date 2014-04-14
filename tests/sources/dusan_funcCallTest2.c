int a = 6;
int b;
int c = 7;

int f1() {
    return a;
}

int f2() {
    b = 5;
    c = 8;
}

int f3(int m) {
    return c*m;
}

int main() {
    int z = f1(); // 6
    int y = 9; // 9
    int x; // 0

    print(f1()); // print(6);

    y = y + f1(); // 15

    print(y); // print(15);
    print(z+c); // print(13);

    x = z + f3(4) + a; // 6+28+6 = 40

    print(x - f3(1)); // print(33);

    f2(); // b = 5, c = 8

    x = f3(4); // 32

    print(x); // print(32);
}
