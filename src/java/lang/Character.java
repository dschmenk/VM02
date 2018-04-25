package java.lang;
import apple2.*;

public final class Character
{
	private char value;
	public final static int MAX_RADIX = 36;
	public final static int MIN_RADIX = 2;
	public final static char MAX_VALUE = (char)0xFFFF;
	public final static char MIN_VALUE = (char)0x0000;
	
	public Character(char ch){value = ch;}
	
	public char charValue(){return value;}
	public static int digit(char ch, int radix)
	{
		if (ch >= '0' && ch <= '9')
			return ch - '0';
		if  (ch >= 'a' && ch <= 'z')
			return ch - 'a' + 10;
		if  (ch >= 'A' && ch <= 'Z')
			return ch - 'A' + 10;
		return -1;
	}
	public static char forDigit(int digit, int radix){return (char)(digit + (digit > 9 ? 'a' : '0'));}
	public static boolean isDefined(char ch){return ch >= 0 && ch <= 255;}
	public static boolean isDigit(char ch){return ch >= '0' && ch <= '9';}
	public static boolean isJavaLetter(char ch){return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch == '$') || (ch == '_');}
	public static boolean isJavaLetterOrDigit(char ch){return (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch == '$') || (ch == '_');}
	public static boolean isLetter(char ch){return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch == '$') || (ch == '_');}
	public static boolean isLetterOrDigit(char ch){return (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch == '$') || (ch == '_');}
	public static boolean isUpperCase(char ch){return (ch >= 'A' && ch <= 'Z');}
	public static boolean isLowerCase(char ch){return (ch >= 'a' && ch <= 'z');}
	public static char toUpperCase(char ch){return (char)((ch >= 'a' && ch <= 'z') ? (ch - 'A' + 'a') : ch);}
	public static char toLowerCase(char ch){return (char)((ch >= 'A' && ch <= 'Z') ? (ch - 'a' + 'A') : ch);}
	public static char toTitleCase(char ch){return ch;}
	public String toString()
	{
		byte buffer[] = new byte[2];
		int buffptr, refStr;
		buffer[0] = 1;
		buffer[2] = (byte)value;
		buffptr = vm02.call(vm02.refAsBits((Object)buffer), 0x0E) + 2; // HMEM_LOCK
		refStr = vm02.call(buffptr, 0x46); // HSTRPL_ADD
		vm02.call(vm02.refAsBits((Object)buffer), 0x10); // HMEM_UNLOCK
		return (String)vm02.bitsAsRef((refStr & 0xFFFF) | 0x00830000 | ((refStr << 8) & 0xFF000000));
	}
}