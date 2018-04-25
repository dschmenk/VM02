import apple2.*;

public class HelloWorld extends Thread
{
	private static boolean finished = false;
	private static Object sync = new Object();
	public short SampleShort;
	public int SampleInst;
	public String Title;
	public int TitleRow;
	
	HelloWorld(String s, int r)
	{
		Title = s;
		TitleRow = r;
	}
	
	public void run()
	{
		int i = 0;
		int ix = 1;
		while (!finished)
		{
			synchronized (sync)
			{
				conio.gotoXY(i, TitleRow);
				conio.print(Title);
			}
			i = i + ix;
			if (i < 0)
			{
				i = 1;
				ix = 1;
			}
			else if (i > (39 - Title.length()))
			{
				i = 38 - Title.length();
				ix = -1;
			}
			sleep(100);
		}
	}

	public static void main(String[] args)
	{
		int i, px, key;
		HelloWorld h1 = new HelloWorld(" Hello world ", 0);
		HelloWorld h2 = new HelloWorld(" Press ESC key to quit... ", 3);
		
		conio.home();
		conio.gotoXY(0, 5);
		conio.print(":_");
		Thread.currentThread().setPriority(6);
		h1.start(); // Start HelloWorld thread 1
		h2.start(); // Start HelloWorld thread 2
		px = 1;
		while ((key = conio.getKey()) != 27)
		{
			synchronized (sync)
			{
				if (key < 32)
				{
					if (key == 8) // LEFT ARROW
					{
						if (--px > 0)
						{
							conio.gotoXY(px,5);
							conio.print("_ ");
						}
						else
							px = 1;
					}
				}
				else
				{
					conio.gotoXY(px, 5);
					conio.print((char)key);
					conio.print('_');
					if (++px > 38)
					{
						conio.gotoXY(0,5);
						conio.print(":_                                      ");
						px = 1;
					}
				}
			}
		}
		finished = true; // Flag HelloWorld threads to exit
		sleep(500);
		conio.home();
		Thread.currentThread().dumpStack();
		conio.println("That's all folks...");
	}
}

