package java.lang;
import apple2.*;

public final class Integer extends Number
{
	private int value;
	public final static int MAX_VALUE = 0x7FFFFFFF;
	public final static int MIN_VALUE = 0x80000000;
	
	public Integer(int i)
	{
		value = i;
	}
	public Integer(String s)
	{
		value = parseInt(s);
	}
	
	
	public float floatValue(){return (float)value;}
	public int intValue(){return value;}
	
	public static int parseInt(String s){return parseInt(s, 10);}
	public static int parseInt(String s, int radix)
	{
		char c;
		int digit, value = 0, scanPos = 0;
		boolean neg = false;
		
		if (s.charAt(0) == '-')
		{
			neg = true;
			scanPos++;
		}
		while (scanPos < s.length())
		{
			c = s.charAt(scanPos++);
			if (c <= '9')
				digit = c - '0';
			else if (c >= 'a')
				digit = c - ('a' - 10);
			else
				digit = c - ('A' - 10);
			if (digit < 0 || digit >= radix)
				break;
			value = (value * radix) + digit;
		}
		return neg ? -value : value;
	}
	public static String toBinaryString(int i){return toString(i, 1);}
	public static String toOctalString(int i){return toString(i, 8);}
	public static String toHexString(int i){return toString(i, 16);}
	public String toString(){return toString(value, 10);}
	public static String toString(int i){return toString(i, 10);}
	public static String toString(int i, int radix)
	{
		byte buffer[] = new byte[33];
		boolean neg = false;
		int digit, buffptr, refStr, scanPos = 32;
		
		if (i < 0)
		{
			neg = true;
			i = -i;
		}
		do
		{
			digit = i % radix;
			buffer[scanPos--] = (byte)(digit + ((digit > 9) ? ('A' - 10) : '0'));
			i /= radix;
		} while (i > 0);
		if (neg)
			buffer[scanPos--] = (byte)'-';
		buffer[scanPos] = (byte)(32 - scanPos);
		buffptr = vm02.call(vm02.refAsBits((Object)buffer), 0x0E) + 2 + scanPos; // HMEM_LOCK
		refStr = vm02.call(buffptr, 0x46); // HSTRPL_ADD
		vm02.call(vm02.refAsBits((Object)buffer), 0x10); // HMEM_UNLOCK
		return (String)vm02.bitsAsRef((refStr & 0xFFFF) | 0x00830000 | ((refStr << 8) & 0xFF000000));
	}
	public static Integer valueOf(String s){return new Integer(s);}
	public static Integer valueOf(String s, int radix){return new Integer(parseInt(s, radix));}
}