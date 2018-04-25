//
//  java - character user interface
// 
package org.vm02.cui;
import apple2.*;

public class cuiApp extends cuiControl
{
	protected boolean quit = false; // Not dead yet!
	protected int inputMask = cui.INPUT_KEYBOARD | cui.INPUT_MOUSEBTTNDOWN; // Default input mask
	protected int eventTimeOut = -1; // Default time-out
	
	public cuiApp()
	{
		super(0, 0, 80, 24, 0, (char)-1, 0, (cuiControl)null);
		if (!cui.startUp())
		{
			System.out.println("Reqiures 128K IIe or IIc");
			System.exit(-1);
		}
		cuiControl.list    = this;
		cuiControl.listTop = this;
	}
	protected void start()
	{
		cui.fillRect(0, 0, 80, 24, (byte)'V', (byte)'W');
	}
	protected void stop()
	{
		cui.shutDown();
	}
	protected void chain(String chainCmd)
	{
		if (chainCmd != null)
		{
			int stringptr = vm02.call(vm02.refAsBits((Object)chainCmd), 0x0E) & 0xFFFF; // HMEM_LOCK
			vm02.call(stringptr + 1, 0x3E);                                             // MEMSRC
			vm02.call(0x0200, 0x40);                                                    // MEMDST
			vm02.call(chainCmd.length(), 0x42);                                         // MEMCPY
			vm02.call(vm02.refAsBits((Object)chainCmd), 0x10);                          // HMEM_UNLOCK
			vm02.pokeByte(0x03EC, (byte)chainCmd.length());                             // CHAIN_CMD
		}
	}
	protected void run(int gc)
	{
		if (gc > 0)
		{
			byte prevCursor = cui.setMouseCursor('C'); // hour glass
			cuiMessageBox waitBox = new cuiMessageBox("Please Wait", "System is coming up", "(forcing garbage collection)", 0);
			cui.frameRect(29, 13, 22, 3, -1, true, null);
			cui.fillRect(30, 14, 20, 1, (byte)'V', (byte)'W');
			for (int i = 0; i < 20; i++)
			{
				cui.fillRect(30, 14, i + 1, 1, ' ', ' ');
				cui.gc(gc); // get a leg up on GC
			}
			waitBox.delete();
			waitBox = null;
			cui.setMouseCursor(prevCursor);
		}
		run();
	}
	protected void run()
	{
		while (!quit) eventDispatch(inputMask, eventTimeOut);
	}
}
