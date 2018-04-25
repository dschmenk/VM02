package java.lang;

public final class Double extends Number
{
	private double value;
	public Double(double d)
	{
	}
	public Double(String s)
	{
	}


	public float floatValue(){return (float)value;}
	public int intValue(){return (int)value;}

	public static double parseDouble(String s){return parseDouble(s, 10);}
	public static double parseDouble(String s, int radix)
	{
           return 0.0;
	}
	public static String toBinaryString(double d){return toString(d, 1);}
	public static String toOctalString(double d){return toString(d, 8);}
	public static String toHexString(double d){return toString(d, 16);}
	public String toString(){return toString(value, 10);}
	public static String toString(double d){return toString(d, 10);}
	public static String toString(double d, int radix)
	{
            return "";
	}
	public static Double valueOf(String s){return new Double(s);}
	public static Double valueOf(String s, int radix){return new Double(parseDouble(s, radix));}
}
