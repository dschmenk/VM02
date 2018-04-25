package java.io;

public class OutputStream extends Object {
	public OutputStream()
	{
	}
	
	public void close(){}
	public void flush(){}
	public void write(byte b[]) throws IOException
	{
		write(b, 0, b.length);
	}
	public void write(byte b[], int off, int len) throws IOException
	{
		int i, l = off + len;
		for (i = off; i < l; i++)
			write(b[i]);
	}
	public void write(byte b){}
}