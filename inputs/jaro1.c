int d;

int func() {
  int c;
  d = d + 4;
  return 2;  
}

int main() {
  int h;
  h = func() + func();
  
  if(func()) {
    print(d);
  } else {
    print("ahoj");
  }
}