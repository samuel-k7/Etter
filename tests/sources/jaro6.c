int d = 2;

int func() {
  d = 4;
}

int hoh(int a) {
  return func() + 2;
}

int main() {
  int x;
  hoh(func());
  print(x);
  print(d);
}