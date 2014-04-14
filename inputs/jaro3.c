int p = 4;

int func();


int func2(int a, int b) {
  print(a);
  print("ahoj");
  print(b);
}

int main() {
  func2(2 + 4 + func(), func());
}

int func() {
  p = p + 8;
  print(p);
  return 2;
}