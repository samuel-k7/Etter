string copyright = "(c) Dusan FLP";

string rok() {
  return "2014";
}

string opakuj(string text, int pocet) {
  int i = pocet;
  string vypis;
  while(i>0) {
    vypis = vypis + text;
    i = i-1;
  }
  return vypis;
}

int kresliTrojuholnik(int zakladna) {
  int aktZakladna = zakladna;
  int okraj = 0;
  string stringOkraj;
  string stringStred;
  while(aktZakladna>0) {
    okraj = (zakladna - aktZakladna)/2;
    stringOkraj = opakuj(" ",okraj);
    stringStred = opakuj("*",aktZakladna);
    print(stringOkraj + stringStred + stringOkraj);
    aktZakladna = aktZakladna - 2;
  }
}

int main() {
  kresliTrojuholnik(29);
  print(copyright + " " + rok());
}
