package java.io;

public class InputStream extends Object {
	public InputStream()
	{
	}
	
	public int available(){return 0;}
	public void close(){}
	public void mark(int readLimit){}
	public boolean markSupported(){return false;}
	public int read() throws IOException {return 0;}
	public int read(byte b[]) throws IOException
	{
		return read(b, 0, b.length);
	}
	public int read(byte b[], int off, int len) throws IOException
	{
		int c, i, l = off + len;
		
		if (b == null)
			return (int)skip(len);
		else
			for (i = off; i < l; i++)
			{
				c = read();
				if (c < 0)
					return (l - i);
				b[i] = (byte)c;
			}
		return len;
	}
	public void reset(){};
	public long skip(long n)
	{
		int s = (int)n;
        try
        {
            while ((s-- != 0) && (read() > 0));
        }
        catch (IOException e)
        {
            n = 0;
        }
        finally
        {
            return n;            
        }
	}
}