//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiPopUpMenu extends cuiPopUp
{
	char accels[];
	String items[];
	int  choice, hilite;
	boolean selected = false;
	
	public cuiPopUpMenu(int menuX, int menuY, int menuWidth, String menuItems[],  char menuAccels[])
	{
		super(menuX, menuY, menuWidth, menuItems.length, true);
		items  = menuItems;
		accels = menuAccels;
		for (int l = 0; l < menuItems.length; l++)
			if (menuItems[l] != null)
				cui.drawString(menuX, menuY + l, menuItems[l]);
			else
				cui.fillRect(menuX, menuY + l, menuWidth, 1, (byte)'S', (byte)'S');
	}
	private void chooseMouse()
	{
		while (cui.bttnMouse != 0)
		{
			if ((cui.xMouse >= x)
			 && (cui.xMouse <  right)
			 && (cui.yMouse >= y)
			 && (cui.yMouse <  bottom))
			{
				int oldHilite = hilite;
				hilite = cui.yMouse - y;
				if ((oldHilite >= 0) && (oldHilite != hilite))
				{
					if (items[oldHilite] != null)
						cui.drawString(x, y + oldHilite, items[oldHilite]);
				}
				if (items[hilite] != null)
					cui.drawInverseString(x, y + hilite,items[hilite]);
			}
			else
			{
				if ((hilite >= 0) && (items[hilite] != null))
					cui.drawString(x, y + hilite, items[hilite]);
				hilite = -1;
			}
			cui.updateMouse();
		}
		choice = EVENT_CANCEL;
		if ((cui.xMouse >= x)
		 && (cui.xMouse <  right)
		 && (cui.yMouse >= y)
		 && (cui.yMouse <  bottom)
		 && (items[cui.yMouse - y] != null))
			choice = cui.yMouse - y;
	}
	public int choose()
	{
		choice = -1;
		hilite = -1;
		if (cui.bttnMouse != 0)
			chooseMouse();
		else
			while (choice < 0) eventDispatch(cui.INPUT_KEYBOARD | cui.INPUT_MOUSEBTTNDOWN, -1);
		delete();
		return choice;
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				if (cui.codeKey == cui.KEY_ESC)
				{
					choice = EVENT_CANCEL;
					return true;
				}
				if (cui.codeKey == cui.KEY_RETURN)
				{
					choice = hilite;
					return true;
				}
				if (cui.codeKey == cui.KEY_ARROWUP)
				{
					if (hilite >= 1)
					{
						cui.drawString(x, y + hilite, items[hilite]);
						while (items[--hilite] == null);
						cui.drawInverseString(x, y + hilite,items[hilite]);
					}
					return true;
				}
				if (cui.codeKey == cui.KEY_ARROWDOWN)
				{
					if (y + hilite < bottom - 1)
					{
						if (hilite >= 0) cui.drawString(x, y + hilite, items[hilite]);
						while (items[++hilite] == null);
						cui.drawInverseString(x, y + hilite,items[hilite]);
					}
					return true;
				}
				if (cui.codeKey == cui.KEY_ARROWRIGHT)
				{
					return true;
				}
				if (cui.codeKey == cui.KEY_ARROWLEFT)
				{
					return true;
				}
				for (int a = 0; a < accels.length; a++)
					if (cui.codeKey == accels[a])
					{
						choice = a;
						return true;
					}
				break;
			case EVENT_MOUSEBTTNDOWN:
				chooseMouse();
				break;
		}
		return true;
	}

}
