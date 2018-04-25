package apple2;

public class OutputSSC extends java.io.OutputStream
{
	private int sscSlot, sscWrite;

	public OutputSSC(int slot)
	{
		sscWrite  = 0x80 + (slot << 1);
		sscSlot   = slot << 16;
	}
	
	public void write(byte b)
	{
		vm02.call(sscSlot | b , sscWrite);
	}
}