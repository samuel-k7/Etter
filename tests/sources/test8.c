string space()
{
	print(" ");
	return " ";
}

string func(string a)
{
	print(a);
	return a;
}

int main()
{
	/*int x;
	int y;
	int z;*/
	string d;
	/*string a;
	string b;*/

	d = func("ble") + space() + func("bla");

	print(d);

	/*x = func(a);
	y = space();
	z = func(a);*/
}