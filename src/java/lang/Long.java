package java.lang;

public final class Long extends Number
{
	private int value;
	public final static int MAX_VALUE = 0x7FFFFFFF;
	public final static int MIN_VALUE = 0x80000000;

	public Long(int i)
	{
		value = i;
	}
	public Long(String s)
	{
		value = parseLong(s);
	}


	public float floatValue(){return (float)value;}
	public int intValue(){return value;}

	public static int parseLong(String s){return parseLong(s, 10);}
	public static int parseLong(String s, int radix)
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
	public static Long valueOf(String s){return new Long(s);}
	public static Long valueOf(String s, int radix){return new Long(parseLong(s, radix));}
}