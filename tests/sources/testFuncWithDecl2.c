int func2();

int func() {
  func2();
}

int func2() {
  print("ahoj");
}

int main() {
  func();
}
