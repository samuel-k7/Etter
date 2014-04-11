int func(int e);
int func2();

int func2(){
  return func(2);
}

int main() {
  int d;
  d = func(func2()); // 6
  d = d + func(d) + 4; // 18
  print(100 + func2() + d + func(0)); // 100 + 4 + 18 + 2 = 124
}

int func(int e) {
  return 2+e;
}
