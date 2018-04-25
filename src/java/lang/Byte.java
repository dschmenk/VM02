package java.lang;

public final class Byte extends Number
{
	private int value;
	public final static int MAX_VALUE = 0x7F;
	public final static int MIN_VALUE = 0x80;

	public Byte(int i)
	{
		value = i;
	}
	public Byte(String s)
	{
		value = parseByte(s);
	}


	public float floatValue(){return (float)value;}
	public int intValue(){return value;}

	public static int parseByte(String s){return parseByte(s, 10);}
	public static int parseByte(String s, int radix)
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
	public static Byte valueOf(String s){return new Byte(s);}
	public static Byte valueOf(String s, int radix){return new Byte(parseByte(s, radix));}
}