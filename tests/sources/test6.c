int mu = 0;

int main();

int func(double a)
{
    if(mu==0)
    {
        mu = 1;
        main();
    }

}

int main()
{
	int a;
	a=0;
	func(10.0);
}