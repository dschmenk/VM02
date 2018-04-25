//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiTopLevelWindow extends cuiWindow
{	
	public cuiTopLevelWindow(int topX, int topY, int topWidth, int topHeight, int topID, String topTitle, cuiControl topOwner)
	{
		super(topX, topY, topWidth, topHeight, topID, topTitle, topOwner);
	}

	public cuiControl setFocus()
	{
		cuiControl prevFocus = super.setFocus();
		nextFocus(this.next);
		return prevFocus;
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				if (cui.codeKey == (cui.MODKEY_OPENAPPLE | cui.KEY_TAB))
				{
					if (focus != null)
						focus.nextFocus(this.next);
					else 
						this.nextFocus(this.next);
					return true;
				}
				break;
		}
		return false;
	}
}
