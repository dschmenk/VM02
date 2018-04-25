//
//  java - character user interface
// 
package org.vm02.cui;
import apple2.*;

public class cuiMenuBar extends cuiControl
{
	public final static int MENUBAR_ID = 1;
	public final static int EVENT_MENUBARITEMSELECT = 200;
	private static String   sysMenuList[] = {" _@Z ", " About  ", null, " Reboot "};
	private boolean dropDone;
	private cuiControl menuOwner;
	private String     menuLists[][];
	private char       menuListsAccel[][];
	private byte       menuLeft[], menuRight[], menuWidth[];
	
	public cuiMenuBar(String newMenuLists[][], char newMenuListsAccel[][], cuiControl newMenuOwner)
	{
		super(0, 0, 80, 1, MENUBAR_ID, (char)(cui.MODKEY_OPENAPPLE | cui.KEY_ESC), FLAGS_NOMOUSEFOCUS, (cuiControl)null);
		menuOwner      = newMenuOwner;
		menuLists      = new String[newMenuLists.length + 1][];
		menuListsAccel = new char[newMenuListsAccel.length + 1][];
		menuLeft       = new byte[menuLists.length];
		menuRight      = new byte[menuLists.length];
		menuWidth      = new byte[menuLists.length];
		cui.fillRect(0, 0, 80, 1, ' ', ' ');
		int l = 1;
		for (int b = 0; b < menuLists.length; b++)
		{
			if (b == 0)
			{
				menuLists[0] = sysMenuList;
				cui.drawMouseString(l, 0, menuLists[b][0]);
			}
			else
			{
				menuLists[b]      = newMenuLists[b - 1];
				menuListsAccel[b] = newMenuListsAccel[b - 1];
				cui.drawInverseString(l, 0, menuLists[b][0]);
			}
			menuLeft[b]  = (byte)l;
			l           += menuLists[b][0].length();
			menuRight[b] = (byte)l;
			for (int w = 1; w < menuLists[b].length; w++)
				if ((menuLists[b][w] != null) && (menuLists[b][w].length() > menuWidth[b]))
					menuWidth[b] = (byte)menuLists[b][w].length();
		}
	}
	private void activate(int menuIndex)
	{
		cuiControl prevFocus = setFocus();
		dropDone = false;
		do
		{
			cuiDropDownMenu menuDrop = new cuiDropDownMenu(menuLeft[menuIndex], menuWidth[menuIndex], (menuIndex == 0) ? " _AZ " : (String)null, menuLists[menuIndex], menuListsAccel[menuIndex]);
			int choice = menuDrop.choose();
			menuDrop = null;
			if (choice != 0)
			{
				if (menuIndex == 0)
				{
					cuiMessageBox msgBox;
					if (choice == 1)
					{
						msgBox = new cuiMessageBox("About VM02", "Version 1.0", "Copyright (c) 2010 David Schmenk", cuiMessageBox.MSGBOX_OK);
						msgBox.reply();
					}
					else if (choice == 3)
					{
						msgBox = new cuiMessageBox("Reboot Verification", "Really reboot?", "You will lose all unsaved data.", cuiMessageBox.MSGBOX_OK | cuiMessageBox.MSGBOX_CANCEL);
						if (msgBox.reply() == EVENT_OK)
						{
							cui.shutDown();
							vm02.call(0, 0xEA); // Reboot
						}
					}
					msgBox = null;
				}
				else
					menuOwner.event(EVENT_MENUBARITEMSELECT, (menuIndex << 16) | choice);
				dropDone = true;
			}
			else
			{
				if (cui.bttnMouse != 0)
				{
					while ((cui.bttnMouse != 0)
					 && (cui.yMouse != 0 || (cui.xMouse < menuLeft[0] || cui.xMouse >= menuRight[menuLists.length - 1])))
					 	cui.updateMouse();
					 if (cui.bttnMouse == 0)
					 	return;
					for (menuIndex = 0; menuIndex < menuLists.length; menuIndex++)
					{
						if (cui.xMouse >= menuLeft[menuIndex] && cui.xMouse < menuRight[menuIndex])
							break;
					}
				}
				else
				{
					if (cui.codeKey == cui.KEY_ARROWRIGHT)
					{
						if (++menuIndex >= menuLists.length)
							menuIndex = 0;
					}
					else if (cui.codeKey == cui.KEY_ARROWLEFT)
					{
						if (--menuIndex < 0)
							menuIndex =  menuLists.length - 1;
					}
					else
						dropDone = true;
				}
			}
		} while (!dropDone);
		if (prevFocus != null) prevFocus.setFocus();
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				if (cui.codeKey == accel)
				{
					activate(0);
					return true;
				}
				for (int i = 0; i < menuListsAccel.length; i++)
					for (int j = 0; j < menuListsAccel[i].length; j++)
						if (cui.codeKey == menuListsAccel[i][j])
						{
							menuOwner.event(EVENT_MENUBARITEMSELECT, (i << 16) | (j + 1));
							return true;
						}
				break;
			case EVENT_MOUSEBTTNDOWN:
				while ((cui.bttnMouse != 0)
				 && (cui.yMouse != 0 || (cui.xMouse < menuLeft[0] || cui.xMouse >= menuRight[menuLists.length - 1])))
					cui.updateMouse();
				 if (cui.bttnMouse != 0)
					for (int b = 0; b < menuLists.length; b++)
					{
						if (cui.xMouse >= menuLeft[b] && cui.xMouse < menuRight[b])
						{
							activate(b);
							return true;
						}
					}
				return true;
		}
		return false;
	}
}
