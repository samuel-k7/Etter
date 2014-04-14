int limit = 100;

int jeParne(int x) {
    return x == ((x/2)*2);
}

int f(int x) {
    while(x<limit) {
        if(jeParne(x)) {
            f(x-1);
            return limit;
        }
        else {
            print(x);
            if((x+1)*2 >= limit) {
                return limit;
            }
            f((x+1)*2);
            return limit;
        }
    }
}

int main() {
    f(2);
}
