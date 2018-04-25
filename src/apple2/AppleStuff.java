package apple2;

public class AppleStuff {
	private static int hrHandle = 0;

	public static void text()
	{
		if (hrHandle != 0)
		{
			// Free hires memory
			vm02.call(hrHandle, 0x0C);
			hrHandle = 0;
		}
		vm02.peekByte(0xC051);
		vm02.peekByte(0xC054);
		vm02.pokeByte(0x22, (byte)0);
		vm02.call(0, 0xFC58);
	}
	public static boolean keyPressed()
	{
		return (vm02.peekByte(0x0280) > 0);
	}
	public static char getKey()
	{
		int key = vm02.call(0, 0x76) & 0x7F;	// CONSOLE READ
		return ((char)key);
	}
	public static void loResMix()
	{
		vm02.peekByte(0xC056);
		vm02.peekByte(0xC053);
		vm02.peekByte(0xC050);
		vm02.call(0, 0xF836);
		vm02.pokeByte(0x22, (byte)20);
		vm02.call(0, 0xFC58);
	}
	public static void loRes()
	{
		vm02.peekByte(0xC056);
		vm02.peekByte(0xC052);
		vm02.peekByte(0xC050);
		vm02.call(0, 0xF832);
	}
	public static void lrColor(int color)
	{
		//vm02.call(color, 0xF864);
	}
	public static void lrHLine(int xLeft, int xRight, int y, int color)
	{
		//vm02.call(color, 0xF864);
		//vm02.pokeByte(0x2C, (byte)xRight);
		//vm02.call((xLeft << 16) | y, 0xF819);
	}
	public static void lrHLine(int xLeft, int xRight, int y)
	{
		//vm02.pokeByte(0x2C, (byte)xRight);
		//vm02.call((xLeft << 16) | y, 0xF819);
	}
	public static void lrVLine(int x, int yTop, int yBottom, int color)
	{
		//vm02.call(color, 0xF864);
		//vm02.pokeByte(0x2D, (byte)yBottom);
		//vm02.call((x << 16) | yTop, 0xF828);
	}
	public static void lrVLine(int x, int yTop, int yBottom)
	{
		//vm02.pokeByte(0x2D, (byte)yBottom);
		//vm02.call((x << 16) | yTop, 0xF828);
	}
	public static void lrPlot(int x, int y, int color)
	{
		//vm02.call(color, 0xF864);
		//vm02.call((x << 16) | y, 0xF800);
	}
	public static void lrPlot(int x, int y)
	{
		//vm02.call((x << 16) | y, 0xF800);
	}
	public static boolean hiRes()
	{
		int result;
		
		// Allocate hires page2 memory
		do
		{
			result = vm02.call(0x014020, 0x0A);	// HMEM_ALLOC_FIXED
			if ((result & 0x01000000) == 0)		// CARRY CLEAR == ALLOCED
			{
				hrHandle = result & 0xFFFF;
				vm02.peekByte(0xC052);
				vm02.peekByte(0xC057);
				vm02.peekByte(0xC055);
				vm02.peekByte(0xC050);
				vm02.call(0x4000, 0x40);	// SETDST 
				vm02.call(0x2000, 0x44);	// MEMCLR 
				return true;
			}
			result = vm02.call(100, 0x64);	// GC - MAX 100 ITERATIONS
		} while ((result & 0x01000000) == 0);
		return false;
	}
	public static void hrColor(int color)
	{
		//vm02.call(color << 8, 0xF6F0); // HCOLOR
	}
	public static void hrPlot(int x, int y)
	{
		//vm02.call((x << 8) | y, 0xF457); // HPLOT
	}	
	public static void hrLine(int x1, int y1, int x2, int y2)
	{
		//vm02.call((x1 << 8) | y1, 0xF411); // HPOSN
		//vm02.call(x2 | (y2 << 16), 0xF53A); // HLINE
	}
	public static void hrLineTo(int x, int y)
	{
		//vm02.call(x | (y << 16), 0xF53A); // HLINE
	}
	public static int paddle(int num)
	{
		//return ((vm02.call(num << 8, 0xFB1E) >> 16) & 0xFF);
		return 0;
	}
	public static boolean button(int num)
	{
		//return (vm02.peekByte(0xC061+num) >= 128);
		return false;
	}
	public static void tone(int pitch, int duration)
	{
		tone(pitch, pitch >> 1, duration);
	}
	public static void tone(int pitch, int timbre, int duration)
	{
	}
}