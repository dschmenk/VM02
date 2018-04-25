import apple2.*;

public class TestSelect
{
	public static int select(int mask) throws InterruptedException
	{
		return (vm02.call(mask | 0x08, 0x80) & 0xFF);
	}
	public static void main(String args[])
	{
		int curtid = vm02.call(0, 0x1C) & 0x00FF0000; // THREAD_GET_CURRENT
		int selectMask;
		
		conio.home();
		if (Mouse.enable())
		{
			selectMask = Mouse.slotMask();
			conio.print("Mouse slot mask = ");
			conio.println(selectMask);
		}
		else
		{
			conio.println("Unable to find mouse card");
			return;
		}
		do
		{
			vm02.call(curtid | 10000, 0x1E);  // THREAD_SETTIMEOUTL for 10 seconds
			// vm02.call(curtid | (tparam >> 16   ), 0x20);  // THREAD_SETTIMEOUTH
			try
			{
				if (select(selectMask | 0x08) == selectMask)
				{
					Mouse.update();
					conio.gotoXY(8,6);
					conio.print("X Pos:");  conio.print(Mouse.xPos);  conio.print("    ");
					conio.gotoXY(8,8);
					conio.print("Y Pos:");  conio.print(Mouse.yPos);  conio.print("    ");
					conio.gotoXY(8,10);
					conio.print("Status:"); conio.print(Mouse.status); conio.print("    ");
				}
			}
			catch (InterruptedException e)
			{
				conio.println("Timed out");
			}
		} while (!conio.keyPressed());
		Mouse.disable();
	}
}