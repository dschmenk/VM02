package apple2;

public class InputSSC extends java.io.InputStream
{
	private int numSlot, sscSlot, sscRead, sscCtrl;
	private byte inBuff[];
	
	public InputSSC(int slot)
	{
		int bufferptr;
		inBuff    = new byte[256];		
		sscRead   = 0x70 + (slot << 1);
		sscCtrl   = 0x90 + (slot << 1);
		numSlot   = slot;
		sscSlot   = slot << 16;
		vm02.call(sscSlot | (7 << 19) | (vm02.call(vm02.refAsBits((Object)inBuff), 0x0E) & 0xFFFF) + 2, sscCtrl); // HMEM_LOCK IOCTL_INBUFF
	}
		
	public int available()
	{
		return vm02.call(sscSlot | (5 << 19), sscCtrl) & 0xFF;
	}
	public int read()
	{
		return vm02.call(sscSlot, sscRead) & 0xFF;
	}
	public void close()
	{
		vm02.call(sscSlot | (7 << 19), sscCtrl); // IOCTL_INBUFF
		inBuff = null;
	}
}