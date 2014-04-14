int d;

int func() {
  int c;
  d = d + 4;
  return 2;  
}

int main() {
  int h;
  h = func() + func(); // 2 + 2
  
  if(func()) {
    print(d); // 12
  } else {
    print("ahoj");
  }
}