//
//  java - character user interface
// 
package org.vm02.cui;
import apple2.*;

public class cuiConsole extends cuiWindow
{	
	public cuiConsole(int conX, int conY, int conWidth, int conHeight, int conID, String conTitle, cuiControl conOwner)
	{
		super(conX, conY, conWidth, conHeight, conID, conTitle, conOwner);
		vm02.pokeByte(0x20, x);
		vm02.pokeByte(0x21, width);
		vm02.pokeByte(0x22, y);
		vm02.pokeByte(0x23, bottom);
		vm02.pokeByte(0x24, x);
		vm02.pokeByte(0x25, y);
		vm02.call(y, 0xFBC1);
	}
}
