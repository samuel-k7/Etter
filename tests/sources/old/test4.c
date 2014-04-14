int d;
int e;

int func() {
  if(1) {
    print("Text 1");
  }
  else {
    print("Text 2");
    return 7;
    print("Text 3");
  }
  print("Text 4");
  return 2+6;
  print("Text 5");
}

int main()
{
  int x;
  print("Text 6");
  x = func();
  print(x);
  return 0;
  print("Text 7");
}
