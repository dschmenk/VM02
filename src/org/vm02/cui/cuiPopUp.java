//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiPopUp extends cuiControl
{
	protected byte       saveUnder[][];
	protected byte       saveX, saveY;
	private   cuiControl prevFocus;
	
	public cuiPopUp(int popX, int popY, int popWidth, int popHeight,  boolean popTop)
	{
		super(0, 0, 80, 24, 0, (char)-1, FLAGS_NOMOUSEFOCUS, (cuiControl)null);
		prevFocus = clearFocus();
		popX--;
		popY--;
		popWidth  += 2;
		popHeight += 2;
		saveX = (byte)popX;
		saveY = (byte)popY;
		saveUnder = cui.saveRect(saveX, saveY, popWidth, popHeight);
		cui.frameRect(popX, popY, popWidth, popHeight, 0x80 | ' ', popTop, null);
	}
	public void delete()
	{
		super.delete();
		if (saveUnder != null)
		{
			cui.restoreRect(saveX, saveY, saveUnder);
			saveUnder = null;
		}
		if (prevFocus != null)
		{
			prevFocus.setFocus();
			prevFocus = null;
		}
	}
}
