//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiWindow extends cuiControl
{	
	public cuiWindow(int winX, int winY, int winWidth, int winHeight, int winID, String winTitle, cuiControl winOwner)
	{
		super(winX, winY, winWidth, winHeight, winID, (char)-1, 0, winOwner);
		cui.frameRect(winX - 1, winY - 1, winWidth + 2, winHeight + 2, 0x80 | ' ', true, winTitle);
	}
}
