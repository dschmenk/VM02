package apple2;

public class OutputConsole extends java.io.OutputStream {
	public void write(byte b)
	{
		int h;
		switch (b)
		{
			case '\n':
				vm02.call('\r', 0X86);
				break;
			case 9: // TAB
				h = vm02.peekByte(0x0024);
				do {
					vm02.call(0x20, 0x86);
				} while (((++h) & 0x07) != 0);
				break;
			default:
				vm02.call(b, 0x86);
		}
	}
	public void write(byte b[], int off, int len)
	{
		int i, h, c, l = off + len;
		for (i = off; i < l; i++)
		{
			c = b[i];
			switch (c)
			{
				case '\n':
					vm02.call('\r', 0x86);
					break;
				case 9: // TAB
					h = vm02.peekByte(0x0024);
					do {
						vm02.call(0x20, 0x86);
					} while (((++h) & 0x07) != 0);
					break;
				default:
					vm02.call(c, 0x86);
			}
		}
	}

}