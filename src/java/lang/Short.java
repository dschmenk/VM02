package java.lang;

public final class Short extends Number
{
	private int value;
	public final static int MAX_VALUE = 0x7FFF;
	public final static int MIN_VALUE = 0x8000;

	public Short(int i)
	{
		value = i;
	}
	public Short(String s)
	{
		value = parseShort(s);
	}


	public float floatValue(){return (float)value;}
	public int intValue(){return value;}

	public static int parseShort(String s){return parseShort(s, 10);}
	public static int parseShort(String s, int radix)
	{
            return Integer.parseInt(s, radix);
	}
	public static String toBinaryString(int i){return toString(i, 1);}
	public static String toOctalString(int i){return toString(i, 8);}
	public static String toHexString(int i){return toString(i, 16);}
	public String toString(){return toString(value, 10);}
	public static String toString(int i){return toString(i, 10);}
	public static String toString(int i, int radix)
	{
            return Integer.toString(i, radix);
	}
	public static Short valueOf(String s){return new Short(s);}
	public static Short valueOf(String s, int radix){return new Short(parseShort(s, radix));}
}