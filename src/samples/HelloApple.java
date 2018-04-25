class Test1
{
	public final static int MYCONST = 100000;
	public int myInt;
	
	public void Hello()
	{
		conio.putchar('1');
		conio.putcr();
	}
}

class Test2 extends Test1
{
	public void Hello()
	{
		conio.putchar('2');
		conio.putcr();
	}
}

public class TestThread extends Thread
{
	private static void putdigit(int i)
	{
		if (i > 9)
		{
			putdigit(i / 10);
		}
		conio.putchar((char)('0' + i % 10));
	}
	public static void putint(int i)
	{
		if (i < 0)
		{
			conio.putchar('-');
			i = -i;
		}
		putdigit(i);
	}
	public void run()
	{
		int i;
		for (i = 0; i < 1000; i++)
		{
			conio.gotoxy(20,8);
			putint(i);
			conio.putcr();
		}
		conio.putcr();
	}
	public static void main(String[] args)
	{
		int i, j, k;
		float f, g, h;
		TestThread thread2 = new TestThread();

		thread2.setPriority(6);
		thread2.start();
		for (i = 0; i < 1000; i++)
		{
			conio.gotoxy(20,16);
			putint(i);
			conio.putcr();
		}
		conio.putcr();
		return;
	}
}

