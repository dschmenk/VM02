package java.io;

public class FilterOutputStream extends OutputStream {

	protected OutputStream out;
	
	public FilterOutputStream(OutputStream os)
	{
		out = os;
	}

	public void close()
	{
		out.flush();
		out.close();
	}
	public void flush()
	{
		out.flush();
	}
	public void write(byte b)
	{
		out.write(b);
	}
	public void write(byte b[], int off, int len) throws IOException
	{
		out.write(b, off, len);
	}
}