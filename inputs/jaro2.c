int d = 3;

int func() {
  d = 5;
}

int func1() {
  d = 6;
}

int main() {
  int a = func();
  print(d);
  a = func1();
  print(d);
}
