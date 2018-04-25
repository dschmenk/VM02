//
//  java - character user interface
// 
package org.vm02.cui;
import apple2.*;

public class cui
{
	//
	// PARAM INDECES
	//
	private final static byte OP        = 0;
	private final static byte X         = 1;
	private final static byte Y         = 2;
	private final static byte WIDTH     = 3;
	private final static byte HEIGHT    = 4;
	private final static byte OFFSET    = 3;
	private final static byte COUNT     = 4;
	private final static byte FREQUENCY = 1;
	private final static byte DURATION  = 2;
	private final static byte TIMBRE    = 3;
	private final static byte XDST      = 5;
	private final static byte YDST      = 6;
	private final static byte CHAR_EVEN = 5;
	private final static byte CHAR_ODD  = 6;
	private final static byte CURSOR    = 7;
	private final static byte FLAGS     = 7;
	private final static byte PARAMS    = 8;
	//
	// OPS
	//
	private final static byte INIT        = 0;
	private final static byte EXIT        = 1;
	private final static byte FILLRECT    = 2;
	private final static byte SCROLLRECT  = 3;
	private final static byte PUTCHARS    = 4;
	private final static byte PUTSTR      = 5;
	private final static byte PUTINVSTR   = 6;
	private final static byte PUTMOUSESTR = 7;
	private final static byte GETCHARS    = 8;
	private final static byte UPDATEMOUSE = 10;
	private final static byte SHOWMOUSE   = 11;
	private final static byte HIDEMOUSE   = 12;
	private final static byte TONE        = 16;
	//
	// EVENTS
	//
	public final static int INPUT_UNKNOWN       = 0x00;
	public final static int INPUT_KEYBOARD      = 0x01;
	public final static int INPUT_MOUSE         = 0x0E;
	public final static int INPUT_MOUSEBTTN     = 0x06;
	public final static int INPUT_MOUSEBTTNDOWN = 0x02;
	public final static int INPUT_MOUSEBTTNUP   = 0x04;
	public final static int INPUT_MOUSEMOVE     = 0x08;
	public final static int INPUT_TIMEOUT       = 0x80;
	//
	// KEY CODES
	//
	public final static char KEY_TAB        = 0x09;
	public final static char KEY_ARROWLEFT  = 0x08;
	public final static char KEY_ARROWRIGHT = 0x15;
	public final static char KEY_ARROWUP    = 0x0B;
	public final static char KEY_ARROWDOWN  = 0x0A;
	public final static char KEY_RETURN     = 0x0D;
	public final static char KEY_ESC        = 0x1B;
	public final static char KEY_SPACEBAR   = 0x20;
	public final static char KEY_DELETE     = 0x7F;
	//
	// KEY CODE MODS
	//
	public final static char MODKEY_OPENAPPLE  = 0x0080;
	public final static char MODKEY_CLOSEAPPLE = 0x0100;


	private static   byte selectMask  = 0x08; // CONSOLE by default
	private static   byte selectMouse = 0x00; // CONSOLE by default
	private static   byte cursorMouse;
	private static   int  mouseSlot, mouseCtrl, ctrlRead, addrXPos, addrYPos, addrStatus;
	protected static byte staticParams[] = new byte[PARAMS];
	public static    int  timeMouseEvent;
	public static    byte xMouse, yMouse, bttnMouse;
	public static    char codeKey;
	public static    byte leftBorder   = (byte)'Z';
	public static    byte rightBorder  = (byte)'_';
	public static    byte topBorder    = (byte)(0x80|'_');
	public static    byte bottomBorder = (byte)'L';
	public static    byte titleBar     = (byte)(0x80|'_');

	public static int select() throws InterruptedException
	{
		return (vm02.call(selectMask, 0x80) & 0xFF);
	}
	public static int updateMouse()
	{
		int update = 0;
		int sxy = vm02.call(ctrlRead, mouseCtrl); // CALL_FW ReadMouse
		if ((sxy & 0x80) != (bttnMouse & 0x80))
		{
			bttnMouse = (byte)(sxy & 0x80);
			update = (bttnMouse == 0) ? INPUT_MOUSEBTTNUP : INPUT_MOUSEBTTNDOWN;
			timeMouseEvent = (vm02.call(0, 0x3C) << 16) | (vm02.call(0, 0x3A) & 0xFFFF);
		}
		if ((sxy & 0x20) != 0)
		{
			int x = (sxy & 0x0000FF00) >> 9;
			int y = (sxy & 0x00FF0000) >> 18;
			if (x >= 80) x = 79;
			if (y >= 24) y = 23;
			if (x != xMouse || y != yMouse)
			{
				byte params[] = staticParams;
				xMouse = (byte)x;
				yMouse = (byte)y;
				params[OP]     = UPDATEMOUSE;
				params[X]      = (byte)x;
				params[Y]      = (byte)y;
				params[CURSOR] = cursorMouse;
				cuiDriver.dispatch(params, null);
				update |= INPUT_MOUSEMOVE;
			}
		}
		return update;
	}
	public static int getInputEvent(int eventMask, int timeOut)
	{
		int inputSlot, inputEvent;
		int curTID = vm02.call(0, 0x1C) & 0x00FF0000; // THREAD_GET_CURRENT
		if (timeOut < 0)
		{
			vm02.call(curTID, 0x22);  // LINK_THREADNOTIMEOUT
		}
		else
		{
			vm02.call(curTID | (timeOut & 0xFFFF), 0x1E);  // THREAD_SETTIMEOUTL
			vm02.call(curTID | (timeOut >> 16   ), 0x20);  // THREAD_SETTIMEOUTH
		}
		do
		{
			inputEvent = 0;
			try
			{
				inputSlot = select();
			}
			catch (InterruptedException e)
			{
				return INPUT_TIMEOUT;
			}
			if ((inputSlot & selectMouse) != 0) // MOUSE event
				inputEvent = updateMouse();
			if ((inputSlot & 0x08) != 0) // KEYBOARD event
			{
				codeKey = (char)(vm02.call(0, 0x76)    & 0xFF);   // KEYBD_READ
				// moved following into console driver
				//        |        (vm02.peekByte(0xC061) & 0x80)); // PB0 (OPEN-APPLE)
				//      |       ((vm02.peekByte(0xC062) & 0x80) << 1));// PB1 (CLOSED-APPLE OR OPTION)
				inputEvent |= INPUT_KEYBOARD;
			}
		} while ((inputEvent & eventMask) == 0);
		return (inputEvent & eventMask);
	}
	public static byte[][] saveRect(int x, int y, int width, int height)
	{
		byte params[] = staticParams;
		byte saveUnder[][] = new byte[height][width];
		params[OP]     = GETCHARS;
		params[X]      = (byte)x;
		params[OFFSET] = 0;
		params[COUNT]  = (byte)width;
		while (--height >= 0)
		{
			params[Y] = (byte)y++;
			cuiDriver.dispatch(params, saveUnder[height]);
		}
		return saveUnder;
	}
	public static void restoreRect(int x, int y, byte saveUnder[][])
	{
		byte params[] = staticParams;
		int height         = saveUnder.length;
		params[OP]     = PUTCHARS;
		params[X]      = (byte)x;
		params[OFFSET] = 0;
		params[COUNT]  = (byte)saveUnder[0].length;
		while (--height >= 0)
		{
			params[Y] = (byte)y++;
			cuiDriver.dispatch(params, saveUnder[height]);
		}
	}
	public static void scrollRect(int x, int y, int width, int height, int yDst)
	{
		byte params[] = staticParams;
		params[OP]        = SCROLLRECT;
		params[X]         = (byte)x;
		params[Y]         = (byte)y;
		params[WIDTH]     = (byte)width;
		params[HEIGHT]    = (byte)height;
		params[YDST]      = (byte)yDst;
		cuiDriver.dispatch(params, null);
	}
	public static void fillRect(int x, int y, int width, int height, int charEven, int charOdd)
	{
		byte params[] = staticParams;
		params[OP]        = FILLRECT;
		params[X]         = (byte)x;
		params[Y]         = (byte)y;
		params[WIDTH]     = (byte)width;
		params[HEIGHT]    = (byte)height;
		params[CHAR_EVEN] = (byte)charEven;
		params[CHAR_ODD]  = (byte)charOdd;
		cuiDriver.dispatch(params, null);
	}
	public static void frameRect(int x, int y, int width, int height, int charFill, boolean borderTop, String title)
	{
		byte params[] = staticParams;
		params[OP] = FILLRECT;
		if (charFill >= 0)
		{
			params[X]         = (byte)(x + 1);
			params[Y]         = (byte)(y + 1);
			params[WIDTH]     = (byte)(width - 2);
			params[HEIGHT]    = (byte)(height - 2);
			params[CHAR_EVEN] = (byte)charFill;
			params[CHAR_ODD]  = (byte)charFill;
			cuiDriver.dispatch(params, null);
		}
		// LEFT
		params[X]         = (byte)x;
		params[Y]         = (byte)(y + 1);
		params[WIDTH]     = 1;
		params[HEIGHT]    = (byte)(height - 2);
		params[CHAR_EVEN] = leftBorder;
		params[CHAR_ODD]  = leftBorder;
		cuiDriver.dispatch(params, null);
		// RIGHT
		params[X]         = (byte)(x + width - 1);
		params[CHAR_EVEN] = rightBorder;
		params[CHAR_ODD]  = rightBorder;
		cuiDriver.dispatch(params, null);
		// BOTTOM
		params[X]         = (byte)x;
		params[Y]         = (byte)(y + height - 1);
		params[WIDTH]     = (byte)width;
		params[HEIGHT]    = 1;
		params[CHAR_EVEN] = (byte)(0x80 | ' ');
		params[CHAR_ODD]  = (byte)(0x80 | ' ');
		cuiDriver.dispatch(params, null);
		params[X]         = (byte)(x + 1);
		params[WIDTH]     = (byte)(width - 2);
		params[CHAR_EVEN] = bottomBorder;
		params[CHAR_ODD]  = bottomBorder;
		cuiDriver.dispatch(params, null);
		if (borderTop)
		{
			// TOP		
			params[X]         = (byte)x;
			params[Y]         = (byte)y;
			params[WIDTH]     = (byte)width;
			params[CHAR_EVEN] = (byte)(0x80 | ' ');
			params[CHAR_ODD]  = (byte)(0x80 | ' ');
			cuiDriver.dispatch(params, null);
			params[X]         = (byte)(x + 1);
			params[WIDTH]     = (byte)(width - 2);
			params[CHAR_EVEN] = topBorder;
			params[CHAR_ODD]  = topBorder;
			cuiDriver.dispatch(params, null);
			if (title != null)
			{
				params[X]         = (byte)(x + 1);
				params[Y]         = (byte)(y + 1);
				params[WIDTH]     = (byte)(width - 2);
				params[CHAR_EVEN] = titleBar;
				params[CHAR_ODD]  = titleBar;
				cuiDriver.dispatch(params, null);
				// TITLE
				params[OP]      = PUTINVSTR;
				params[X]       = (byte)(x + ((width - title.length()) >> 1));
				params[OFFSET]  = 0;
				params[COUNT]   = (byte)(title.length());
				cuiDriver.dispatch(params, title);
			}
		}
	}
	public static void drawString(int x, int y, String str)
	{
		byte params[] = staticParams;
		params[OP]      = PUTSTR;
		params[X]       = (byte)x;
		params[Y]       = (byte)y;
		params[OFFSET]  = 0;
		params[COUNT]   = (byte)str.length();
		cuiDriver.dispatch(params, str);
	}
	public static void drawString(int x, int y, int offset, int count, String str, int charFill)
	{
		byte params[] = staticParams;
		params[OP]        = PUTSTR;
		params[X]         = (byte)x;
		params[Y]         = (byte)y;
		params[OFFSET]    = (byte)offset;
		params[COUNT]     = (byte)count;
		params[CHAR_EVEN] = (byte)charFill;
		params[CHAR_ODD]  = (byte)charFill;
		cuiDriver.dispatch(params, str);
	}
	public static void drawInverseString(int x, int y, String str)
	{
		byte params[] = staticParams;
		params[OP]      = PUTINVSTR;
		params[X]       = (byte)x;
		params[Y]       = (byte)y;
		params[OFFSET]  = 0;
		params[COUNT]   = (byte)str.length();
		cuiDriver.dispatch(params, str);
	}
	public static void drawInverseString(int x, int y, int offset, int count, String str, int charFill)
	{
		byte params[] = staticParams;
		params[OP]        = PUTINVSTR;
		params[X]         = (byte)x;
		params[Y]         = (byte)y;
		params[OFFSET]    = (byte)offset;
		params[COUNT]     = (byte)count;
		params[CHAR_EVEN] = (byte)charFill;
		params[CHAR_ODD]  = (byte)charFill;
		cuiDriver.dispatch(params, str);
	}
	public static void drawMouseString(int x, int y, String str)
	{
		byte params[] = staticParams;
		params[OP]      = PUTMOUSESTR;
		params[X]       = (byte)x;
		params[Y]       = (byte)y;
		params[OFFSET]  = 0;
		params[COUNT]   = (byte)str.length();
		cuiDriver.dispatch(params, str);
	}
	public static void drawMouseString(int x, int y, int offset, int count, String str, int charFill)
	{
		byte params[] = staticParams;
		params[OP]        = PUTMOUSESTR;
		params[X]         = (byte)x;
		params[Y]         = (byte)y;
		params[OFFSET]    = (byte)offset;
		params[COUNT]     = (byte)count;
		params[CHAR_EVEN] = (byte)charFill;
		params[CHAR_ODD]  = (byte)charFill;
		cuiDriver.dispatch(params, str);
	}
	public static void putChars(int x, int y, byte chars[])
	{
		byte params[] = staticParams;
		params[OP]      = PUTCHARS;
		params[X]       = (byte)x;
		params[Y]       = (byte)y;
		params[OFFSET]  = 0;
		params[COUNT]   = (byte)chars.length;
		cuiDriver.dispatch(params, chars);
	}
	public static void putChars(int x, int y, int offset, int count, byte chars[])
	{
		byte params[] = staticParams;
		params[OP]      = PUTCHARS;
		params[X]       = (byte)x;
		params[Y]       = (byte)y;
		params[OFFSET]  = (byte)offset;
		params[COUNT]   = (byte)count;
		cuiDriver.dispatch(params, chars);
	}
	public static void getChars(int x, int y, byte chars[])
	{
		byte params[] = staticParams;
		params[OP]      = GETCHARS;
		params[X]       = (byte)x;
		params[Y]       = (byte)y;
		params[OFFSET]  = 0;
		params[COUNT]   = (byte)chars.length;
		cuiDriver.dispatch(params, chars);
	}
	public static void getChars(int x, int y, int offset, int count, byte chars[])
	{
		byte params[] = staticParams;
		params[OP]      = GETCHARS;
		params[X]       = (byte)x;
		params[Y]       = (byte)y;
		params[OFFSET]  = (byte)offset;
		params[COUNT]   = (byte)count;
		cuiDriver.dispatch(params, chars);
	}
	public static void showMouse()
	{
		staticParams[OP]     = SHOWMOUSE;
		staticParams[X]      = (byte)xMouse;
		staticParams[Y]      = (byte)yMouse;
		staticParams[CURSOR] = cursorMouse;
		cuiDriver.dispatch(staticParams, null);
	}
	public static void hideMouse()
	{
		staticParams[OP] = HIDEMOUSE;
		cuiDriver.dispatch(staticParams, null);
	}
	public static byte setMouseCursor(int newCursor)
	{
		byte oldCursor = cursorMouse;
		cursorMouse    = (byte)newCursor;
		staticParams[OP]     = UPDATEMOUSE;
		staticParams[X]      = (byte)xMouse;
		staticParams[Y]      = (byte)yMouse;
		staticParams[CURSOR] = (byte)newCursor;
		cuiDriver.dispatch(staticParams, null);
		return oldCursor;
	}
	public static void tone(int frequency, int timbre, int duration)
	{
		staticParams[OP]        = TONE;
		staticParams[FREQUENCY] = (byte)frequency;
		staticParams[TIMBRE]    = (byte)timbre;
		staticParams[DURATION]  = (byte)duration;
		cuiDriver.dispatch(staticParams, null);
	}
	public static void gc(int iterations)
	{
		vm02.call(iterations & 0xFFFF, 0x64);
	}
	public static boolean startUp()
	{
		staticParams[OP] = INIT;
		if (cuiDriver.dispatch(staticParams, null) != 0)
			return false;
		//
		// Search for mouse card
		//
		for (int slot = 1; slot < 8; slot++)
		{
			int mouse = vm02.call((1 << 19), 0x90 + (slot << 1)); // ID device
			if ((mouse & 0x010000FF) == 0x20) // CARRY clear == valid device IOCTL, 0x20 == mouse card ID
			{
				mouseCtrl  = 0x90 + (slot << 1);
				mouseSlot  = slot << 16;
				ctrlRead   = mouseSlot | (20 << 19);
				addrXPos   = vm02.peekWord(0x0370 + (slot << 1)) + 2;
				addrYPos   = addrXPos + 2;
				addrStatus = addrYPos + 2;
				if ((vm02.call(mouseSlot | (3 << 19), mouseCtrl) & 0x01000000) == 0) // open port
				{
					vm02.call(mouseSlot | (18 << 19) | 160, mouseCtrl); // clamp X
					vm02.call(mouseSlot | (19 << 19) | 96,  mouseCtrl); // clamp Y
					selectMouse = (byte)(1 << slot);
					selectMask |= selectMouse;
					xMouse      = 40;
					yMouse      = 12;
					cursorMouse = (byte)'B';
					showMouse();
				}
			}
		}
		return true;
	}
	public static void shutDown()
	{
		if (selectMouse != 0)
		{
			staticParams[OP] = HIDEMOUSE;
			cuiDriver.dispatch(staticParams, null);
			vm02.call(mouseSlot | (4<<19), mouseCtrl); // close port
		}
		staticParams[OP] = EXIT;
		cuiDriver.dispatch(staticParams, null);
	}

}
