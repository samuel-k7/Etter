int d = 3;

int func() {
  d = 5;
}

int func1() {
  d = 6;
}

int main() {
  int a = func(); // 0
  print(d); // 5
  a = func1(); // 0
  print(d); // 6
}
