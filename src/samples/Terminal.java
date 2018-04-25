//import java.io.*;
import apple2.*;
/*
 * This class interfaces directly with the console and serial port device drivers
 * for functiuonality testing and upmost performance.
 */
public class Terminal extends Thread
{
	static boolean  done;
	static int      slot;
	
	public void run()
	{
		//
		// Set input buffer
		//
		int sscRead  = 0x70 + (slot << 1);
		int sscCtrl  = 0x90 + (slot << 1);
		int sscSlot  = slot << 16;
		byte inBuff[] = new byte[256];
		int bufferptr = (vm02.call(vm02.refAsBits((Object)inBuff), 0x0E) & 0xFFFF) + 2;	// HMEM_LOCK
		vm02.call(sscSlot | ((bufferptr & 0xFF) << 8) | 7, sscCtrl); // IOCTL_INBUFFL
		vm02.call(sscSlot | (bufferptr & 0xFF00)      | 8, sscCtrl); // IOCTL_INBUFFH
		while (!done)
			//
			// Get available char on serial port and send to console
			//
			vm02.call(vm02.call(sscSlot, sscRead), 0x86);
		vm02.call(sscSlot | 7, sscCtrl); // IOCTL_INBUFFL
		vm02.call(sscSlot | 8, sscCtrl); // IOCTL_INBUFFH
	}

	public static void main(String args[])
	{		
		int sscSlot, sscCtrl, sscWrite;
		Terminal t = new Terminal();
		
		//
		// Search for super serial card
		//
		for (slot = 1; slot < 8; slot++)
		{
			int ssc = vm02.call(1, 0x90 + (slot << 1)); // ID device
			if ((ssc & 0x010000FF) == 0x31) // CARRY clear == valid device IOCTL, 0x31 == serial port ID
				break;
		}
		if (slot == 8)
		{
			System.out.println("Unable to find serial card");
			return;
		}
		/*
		 * Disable VBL interrupts to improve serial port IRQ handling.
		 */
		Mouse.disableIRQ();
		System.out.print("Using serial card in slot #");
		System.out.println(slot);
		System.out.println("Settings: 9600,8,1,N");
		System.out.println("Ctrl-Z to quit.");
		sscWrite = 0x80 + (slot << 1);
		sscCtrl  = 0x90 + (slot << 1);
		sscSlot  = slot << 16;
		vm02.call(sscSlot          |  2, sscCtrl); // init port
		vm02.call(sscSlot | 0x0100 | 20, sscCtrl); // XON/XOFF flow control
		vm02.call(sscSlot | 0x0000 | 19, sscCtrl); // 8 data bits
		vm02.call(sscSlot | 0x0000 | 18, sscCtrl); // no parity bits
		vm02.call(sscSlot | 0x0000 | 17, sscCtrl); // 1 stop bit
		vm02.call(sscSlot | 0x0E00 | 16, sscCtrl); // 9600 Baud
		vm02.call(sscSlot          |  3, sscCtrl); // enable port
		done = false;
		t.start(); // Start serial port reader thread 1
		while (!done)
		{
			//
			// Get available char on console and send to serial port
			//
			int key = vm02.call(0, 0x76) & 0x7F;
			if (key == 0x1A) // Ctrl-Z = quit
				done = true;
			else
				vm02.call(sscSlot | key , sscWrite);
		}
		vm02.call(sscSlot | 4, sscCtrl); // disable port
	}
}