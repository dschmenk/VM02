//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiDropDownMenu extends cuiPopUp
{
	private char menuAccels[];
	private String menuList[];
	private int  choice, hilite;
	private byte menuLeft, menuRight, menuWidth;
	
	public cuiDropDownMenu(int dropMenuX, int dropMenuWidth, String dropSpecialTitle, String dropMenuList[],  char dropMenuAccels[])
	{
		super(dropMenuX, 1, dropMenuWidth, dropMenuList.length - 1, false);
		menuList   = dropMenuList;
		menuAccels = dropMenuAccels;
		menuLeft   = (byte)dropMenuX;
		menuWidth  = (byte)dropMenuWidth;
		menuRight  = (byte)(dropMenuX + dropMenuWidth);
		if (dropSpecialTitle != null)
			cui.drawMouseString(dropMenuX, 0, dropSpecialTitle);
		else
			cui.drawString(dropMenuX, 0, menuList[0]);
		for (int l = 1; l < menuList.length; l++)
			if (menuList[l] != null)
				cui.drawString(dropMenuX, l, menuList[l]);
			else
				cui.fillRect(dropMenuX, l, dropMenuWidth, 1, (byte)'S', (byte)'S');
	}
	private void chooseMouse()
	{
		int titleRight = menuLeft + menuList[0].length();
		choice = 0;
		while ((cui.bttnMouse != 0)
		   && ((cui.yMouse > 0)
		    || ((cui.xMouse >=  menuLeft)
		     && (cui.xMouse < titleRight))))
		{
			int prevHilite = hilite;
			if ((cui.xMouse >= menuLeft)
			 && (cui.xMouse <  menuRight)
			 && (cui.yMouse >= 1)
			 && (cui.yMouse <  menuList.length))
				hilite = cui.yMouse;
			else
				hilite = -1;
			if (prevHilite != hilite)
			{
				if ((prevHilite > 0) && (menuList[prevHilite] != null))
					cui.drawString(menuLeft, prevHilite, menuList[prevHilite]);
				if ((hilite > 0) && menuList[hilite] != null)
					cui.drawInverseString(menuLeft, hilite, menuList[hilite]);
			}
			cui.updateMouse();
		}
		if ((cui.xMouse >= menuLeft)
		 && (cui.xMouse <  menuRight)
		 && (cui.yMouse <  menuList.length)
		 && (menuList[cui.yMouse] != null))
			choice = cui.yMouse;
	}
	public int choose()
	{
		if (cui.bttnMouse != 0)
		{
			hilite = -1;
			chooseMouse();
		}
		else
		{
			choice = -1;
			hilite = 1;
			cui.drawInverseString(menuLeft, 1, menuList[1]);
			while (choice < 0) eventDispatch(cui.INPUT_KEYBOARD | cui.INPUT_MOUSEBTTNDOWN, -1);
		}
		delete();
		return choice;
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				switch (cui.codeKey)
				{
					case cui.KEY_ESC:
					case cui.KEY_ARROWRIGHT:
					case cui.KEY_ARROWLEFT:
						choice = 0;
						break;
					case cui.KEY_RETURN:
						choice = hilite;
						break;
					case cui.KEY_ARROWUP:
						if (hilite > 1)
						{
							cui.drawString(menuLeft, hilite, menuList[hilite]);
							while (menuList[--hilite] == null);
							cui.drawInverseString(menuLeft, hilite, menuList[hilite]);
						}
						break;
					case cui.KEY_ARROWDOWN:
						if (hilite < menuList.length - 1)
						{
							if (hilite > 0) cui.drawString(menuLeft, hilite, menuList[hilite]);
							while (menuList[++hilite] == null);
							cui.drawInverseString(menuLeft, hilite, menuList[hilite]);
						}
						break;
					default:
						for (int a = 1; a < menuAccels.length; a++)
							if (cui.codeKey == menuAccels[a])
							{
								choice = a + 1;
								return true;
							}
				}
				break;
			case EVENT_MOUSEBTTNDOWN:
				if (choice == -1)
					chooseMouse(); // only switch to mouse mode if currently in keyboard mode
				break;
		}
		return true;
	};

}
