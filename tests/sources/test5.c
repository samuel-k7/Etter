int func1()
{
	print("Samo");
}

int space()
{
	print(" ");
}

int func2()
{
	print("je");
}

int func3()
{
	print("skiller");
}

int dot()
{
	print(".");
}

int main()
{
	string a;
	scan(a);
	func1();
	space();
	func2();
	space();
	if(a=="")
	{
		func3();
	} else {
		print(a);
	}
	dot();
}