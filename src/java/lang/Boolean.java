package java.lang;

public final class Boolean
{
	private boolean value;
	public final static boolean TRUE = true;
	public final static boolean FALSE = false;
	
	public Boolean(boolean b){value = b;}
	public Boolean(String s){value = valueOf(s);}
	
	public boolean booleanValue(){return value;}
	public static boolean getBoolean(String name)
	{
		char b = (name != null) ? name.charAt(0) : 'F';
		return (b == 't' || b == 'T');
	}
	public String toString(){return value ? "true" : "false";}
	public static Boolean valueOf(String s){return new Boolean(getBoolean(s));}
}