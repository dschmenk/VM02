package apple2;

public class conio {
	public static void print(char c)
	{
		vm02.call(c, 0x86);
		
	}
	private static void putdigit(int i)
	{
		if (i > 9)
			putdigit(i / 10);
		print((char)('0' + i % 10));
	}
	public static void print(int i)
	{
		if (i < 0)
		{
			print('-');
			i = -i;
		}
		putdigit(i);
	}
	public static void println(int i)
	{
		print(i);
		print('\r');
	}
	public static void print(String s)
	{
		vm02.call(vm02.refAsBits(s), 0x50);
	}
	public static void println(String s)
	{
		vm02.call(vm02.refAsBits(s), 0x52);
	}
	public static void print(byte text[], int offset, int len)
	{
		while (--len >= 0)
			vm02.call(text[offset++], 0x86);
	}
	public static void clreol()
	{
		vm02.call(0, 0xFC9C); // CLREOL
	}
	public static void fill(char c, int len)
	{
		while (--len >= 0)
			vm02.call(c, 0x86);
	}
	public static void text80()
	{
		vm02.call((20 << 19), 0x96); // CONCTL_TEXT80 on slot #3
	}
	public static void home()
	{
		vm02.call(0, 0xFC58);
	}
	public static void gotoXY(int x, int y)
	{
		vm02.pokeByte(0x24, (byte)x);
		vm02.pokeByte(0x25, (byte)y);
		vm02.call(y, 0xFBC1);
	}
	public static void window(int left, int right, int top, int bottom)
	{
		vm02.pokeByte(0x20, (byte)left);
		vm02.pokeByte(0x21, (byte)(right - left));
		vm02.pokeByte(0x22, (byte)top);
		vm02.pokeByte(0x23, (byte)bottom);
	}
	public static void normal()
	{
		vm02.pokeByte(0x32, (byte)0xFF);
	}
	public static void inverse()
	{
		vm02.pokeByte(0x32, (byte)0x3F);
	}
	public static void flash()
	{
		vm02.pokeByte(0x32, (byte)0x7F);
	}
	public static boolean keyPressed()
	{
		return ((vm02.call((5 << 19), 0x96) & 0xFF) > 0);
	}
	public static char getKey()
	{
		int key = vm02.call(0, 0x76) & 0x7F;	// KEYBD_READ
		return ((char)key);
	}
	public static int getLine(byte line[])
	{
		int key,i = 0;
		
		while (i < line.length)
		{
			vm02.pokeByte(0x32, (byte)0x7F);	// FLASH
			vm02.call(0x20, 0x86);		// PRINT PROMPT
			vm02.pokeByte(0x32, (byte)0xFF);	// NORMAL
			vm02.call(0x08, 0x86);		// BS
			key = vm02.call(0, 0x76) & 0x7F;	// KEYBD_READ
			if (key < 0x20 || key == 0x7F)
			{
				vm02.call(0x20, 0x86);	// SPACE - ERASE PROMPT
				vm02.call(0x08, 0x86);	// BS
			}
			else
				vm02.call(key, 0x86);	
			switch (key)
			{
				case -1:
				case '\r':
				case '\n':
					return i;
				case 0x08: // BS
				case 0x7F: // DEL
					if (i > 0)
					{
						i--;
						vm02.call(0x08, 0x86); // BS
					}
					else
						vm02.call(0x07, 0x86); // BELL
					break;
				default:
					line[i++] = (byte)key;
			}
		}
		return i;
	}

}