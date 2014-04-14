int d = 2;

int func() {
  d = 4;
}

int hoh() {
  return func() + 2;
}

int main() {
  int x;
  hoh();
  print(x);
  print(d);
}