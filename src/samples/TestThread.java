public class TestThread extends Thread
{
	private static boolean done;
	private int htab, vtab;
	
	TestThread(int x, int y)
	{
		htab = x;
		vtab = y;
	}
	
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
	synchronized public static void printat(int x, int y, int i)
	{
		conio.gotoxy(x, y);
		putint(i);
	}
	synchronized public static void printat(int x, int y, char c)
	{
		conio.gotoxy(x, y);
		conio.putchar(c);
	}
	public void run()
	{
		int i = 0;
		while (!done)
		{
			printat(htab, vtab, i++);
		}
	}
	public static void main(String[] args)
	{
		int cx, cy;
		char key;
		TestThread thread2 = new TestThread(20, 8);
		done = false;
		conio.home();
		cx = 0;
		cy = 20;
		thread2.start();
		do 
		{
			key = conio.getchar();
			printat(cx, cy, key);
			if (++cx > 39)
			{
				while (--cx > 0)
					printat(cx,cy, ' ');
			}
		} while ((key & 0x7F) != 'Q');
		done = true;
		return;
	}
}

