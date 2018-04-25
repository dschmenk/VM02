package apple2;

public class InputConsole extends java.io.InputStream {
	public int available()
	{
		return vm02.call((5 << 19), 0x96) & 0xFF;	// INPUT AVAIL
	}
	public void close(){}
	public void mark(int readLimit){}
	public boolean markSupported(){return false;}
	public int read()
	{
		int key;
		vm02.pokeByte(0x32, (byte)0x7F);	// FLASH
		vm02.call(0x20, 0x86);		// PRINT PROMPT
		vm02.pokeByte(0x32, (byte)0xFF);	// NORMAL
		vm02.call(0x08, 0x86);		// BS
		key = vm02.call(0, 0x76) & 0x7F;	// KEYBD_READ
		if (key < 0x20 || key == 0x7F)
		{
			vm02.call(0x20, 0x86);		// SPACE
			vm02.call(0x08, 0x86);		// BS
		}
		else
			vm02.call(key, 0x86);	
		return (key);
	}
	public int read(byte b[])
	{
		return read(b, 0, b.length);
	}
	public int read(byte b[], int off, int len)
	{
		int i, l = off + len;
		
		if (b == null)
			skip(len);
		else
			for (i = off; i < l; i++)
				b[i] = (byte)read();
		return len;
	}
	public void reset(){};
	public long skip(long n)
	{
		int s = (int)n;
		while ((s-- != 0) && (read() > 0));
		return n;
	}
}