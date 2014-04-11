// Fibonaci iterativne
int vysl;

int main()
{
	int a;
	print("Zadejte cislo pro vypocet faktorialu: ");
	scan(a);
	print("Zadali ste: ");
	print(a);
	print("\n");
	if (a < 0) 
	{
		print("Faktorial nelze spocitat\n");
	}
	 else 
	{
		vysl = 1;
		while (a > 0) 
		{
			vysl = vysl * a;
			a = a - 1;
		}

		print("Vysledek je: ");
		print(vysl);
		print("\n");
	}
}

