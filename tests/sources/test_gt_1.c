int d = 2;
int func() {
  d = 4;
}
int main() {
  int x;
  x =  func() + 2 + 2;
  print(x);
  print(d);
}