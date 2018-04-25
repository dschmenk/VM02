package apple2;
/*
 * This class interfaces directly with the mouse device driver.
 */
public class Mouse
{
	static private int slot, mouseSlot, mouseCtrl, ctrlRead, addrXPos, addrYPos;
	static public  int xPos, yPos, status;
    
	public static boolean enable()
	{
		//
		// Search for mouse card and disable VBL interrupts
		//
		for (slot = 1; slot < 8; slot++)
		{
			int mouse = vm02.call((1 << 19), 0x90 + (slot << 1)); // ID device
			if ((mouse & 0x010000FF) == 0x20) // CARRY clear == valid device IOCTL, 0x20 == mouse card ID
			{
				mouseCtrl  = 0x90 + (slot << 1);
				mouseSlot  = slot << 16;
				ctrlRead   = mouseSlot | 0x801400;
				addrXPos   = vm02.peekWord(0x0370 + (slot << 1)) + 2;
				addrYPos   = addrXPos + 2;
				return (vm02.call(mouseSlot | (3 << 19), mouseCtrl) & 0x01000000) == 0; // open port
			}
		}
		slot = 0;
		return false;
	}
	public static void disable()
	{
		vm02.call(mouseSlot | (4<<19), mouseCtrl); // close port
	}
	public static void disableIRQ()
	{
		int vblSlot, vbl;
		//
		// Search for mouse card and disable/remove interrupts
		//
		for (vblSlot = 1; vblSlot < 8; vblSlot++)
		{
			vbl = vm02.call((1 << 19), 0x90 + (vblSlot << 1)); // ID device
			if ((vbl & 0x010000FF) == 0x20) // CARRY clear == valid device IOCTL, 0x20 == mouse card ID
			{
				vm02.call((vblSlot << 16) | (17 << 19), 0x90 + (vblSlot << 1)); // MOUSECTL_UNVBL
				break;
			}
		}
	}
	public static int slotMask()
	{
		return (1 << slot);
	}
	public static void update()
	{
		status = vm02.call(ctrlRead, mouseCtrl) & 0xFF; // CALL_FW ReadMouse
		xPos   = vm02.peekWord(addrXPos);
		yPos   = vm02.peekWord(addrYPos);
	}
}