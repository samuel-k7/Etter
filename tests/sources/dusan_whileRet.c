int test() {
    int x = 5;
    while(x>0){
        print(x);
        x = x-1;
        if(x==0) {
            return 0;
        }
    }
}

int main() {
    test();
}
