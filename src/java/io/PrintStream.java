package java.io;

public class PrintStream extends FilterOutputStream {
	public PrintStream(OutputStream out)
	{
		super(out);
	}
	public PrintStream(OutputStream out, boolean  autoFlush)
	{
		super(out);
	}
	
	public boolean checkError()
	{
		flush();
		return false;
	}
	public void print(boolean b)
	{
		if (b)
			print("true");
		else
			print("false");
	}
	public void print(char c)
	{
		write((byte)c);
	}
	public void print(char s[])
	{
		int i;
		for (i = 0; i < s.length; i++)
			write((byte)s[i]);
	}
	public void print(float f)
	{
		print(Float.toString(f));
	}
	public void print(int i)
	{
		byte buffer[] = new byte[15];
		boolean neg = false;
		int scanPos = 14;
		
		if (i < 0)
		{
			neg = true;
			i = -i;
		}
		do
		{
			buffer[scanPos--] = (byte)('0' + i % 10);
			i /= 10;
		} while (i > 0);
		if (neg)
			buffer[scanPos--] = (byte)'-';
		try
		{
			write(buffer, scanPos + 1, 14 - scanPos);
		}
		catch (IOException e)
		{
			return;
		}
	}
	public void print(Object o)
	{
	}
	public void print(String s)
	{
		int i, l = s.length();
		for (i = 0; i < l; i++)
			print(s.charAt(i));
	}
	public void println(boolean b)
	{
		print(b);
		print('\r');
	}
	public void println(char c)
	{
		print(c);
		print('\r');
	}
	public void println(char s[])
	{
		print(s);
		print('\r');
	}
	public void println(float f)
	{
		print(Float.toString(f));
		print('\r');
	}
	public void println(int i)
	{
		print(i);
		print('\r');
	}
	public void println(Object o)
	{
		print(o);
		print('\r');
	}
	public void println(String s)
	{
		print(s);
		print('\r');
	}
}