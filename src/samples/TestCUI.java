import org.vm02.cui.*;

public class TestCUI extends cuiApp
{
	int xBanner = 55;
	String banner = "This is a scrolling message for your amusement while you figure out what to do...";
	String listText[] = {
		"Welcome to the VM02 Character User Interface",
		"demonstration.   Keyboard users should use the",
		"arrow keys to move up and down in this list. The",
		"shortcut key to the menu bar is OpenApple-ESC.",
		"Use OpenApple-TAB to move between the main controls",
		"in the screen.  OpenApple-X is the accelerator key",
		"for the Exit button.  When a pop-up window contains",
		"a Cancel and/or Okay button, ESC will Cancel, RETURN",
		"will accept.  Mouse users should feel right at home.",
		"Remember that this is just an 8 bit, 1 Mhz computer with",
		"128K of RAM.  Don't expect too much from it.  95% of",
		"this environment is running as Java bytecode; DVM and",
		"6502 code make up the rest in the device drivers.",
		"",
		"Feel free to look around,",
		"Dave..."};
	cuiListBox listBox;
	cuiTextEntry textEnter;
	
	public void start()
	{
		super.start();
		String menuItems[][] = {
			{" File ", " New     O-N ", " Open    O-O ", " Save    O-S ", " Save As     ", " Close   O-W ", null, " Quit    O-Q "},
			{" Edit ", " Undo     O-Z ", " Copy     O-C ", " Paste    O-V ", " Cut      O-X ", null, " Find     O-F ", " Replace  O-R "},
			{" Help ", " Show Help  O-H "}};
		char menuItemsAccel[][] = {
			{(char)(cui.MODKEY_OPENAPPLE|'n'), (char)(cui.MODKEY_OPENAPPLE|'o'), (char)(cui.MODKEY_OPENAPPLE|'s'), (char)-1, (char)(cui.MODKEY_OPENAPPLE|'w'), (char)-1, (char)(cui.MODKEY_OPENAPPLE|'q')},
			{(char)(cui.MODKEY_OPENAPPLE|'c'), (char)(cui.MODKEY_OPENAPPLE|'v'), (char)(cui.MODKEY_OPENAPPLE|'x'), (char)-1, (char)(cui.MODKEY_OPENAPPLE|'f'), (char)(cui.MODKEY_OPENAPPLE|'r')},
			{(char)(cui.MODKEY_OPENAPPLE|'h')}};
		cuiMenuBar menuBar = new cuiMenuBar(menuItems, menuItemsAccel, (cuiControl)this);
		//cuiConsole winDbg = new cuiConsole(2, 18, 40, 5, 666, " Debug Console ", (cuiControl)this);
		cuiTopLevelWindow winMain = new cuiTopLevelWindow(10, 3, 60, 18, 0, " Hello World ", (cuiControl)this);
		listBox                   = new cuiListBox(11, 7, 58, 5, listText.length, (cuiControl)this);
		textEnter                 = new cuiTextEntry(11, 14, 58, "Enter banner text here", true, 0, (cuiControl)this);
		cuiButton bttn            = new cuiButton(33, 17, 15, 3, "Exit", true, (char)(cui.MODKEY_OPENAPPLE|'x'), 69, (cuiControl)this);
		listBox.setFocus();
	}
	public boolean event(int msg, int msgData)
	{
		int itemX, itemY, itemWidth, itemIndex;
		switch (msg)
		{
			case EVENT_TIMEOUT:
				if (xBanner >= 0)
				{
					cui.drawString(xBanner + 12, 5, 0, 56 - xBanner, banner, cui.MODKEY_OPENAPPLE|' ');
					xBanner--;
				}
				else
				{
					cui.drawString(12, 5, -xBanner, 56, banner, cui.MODKEY_OPENAPPLE|' ');
					if (xBanner-- < -banner.length()) 
						xBanner = 55;
				}
				return true;
			case cuiListBox.EVENT_LISTITEMDRAW:
				itemX     = msgData >> 24;
				itemY     = (msgData >> 16) & 0xFF;
				itemWidth = (msgData >> 8) & 0xFF;
				itemIndex = msgData & 0xFF;
				cui.drawString(itemX, itemY, 0, itemWidth, listText[itemIndex], cui.MODKEY_OPENAPPLE | ' ');
				return true;
			case cuiListBox.EVENT_LISTITEMDRAWHILITE:
				itemX     = msgData >> 24;
				itemY     = (msgData >> 16) & 0xFF;
				itemWidth = (msgData >> 8) & 0xFF;
				itemIndex = msgData & 0xFF;
				cui.drawInverseString(itemX, itemY, 0, itemWidth, listText[itemIndex], ' ');
				return true;
			case cuiListBox.EVENT_LISTITEMSELECT:
				textEnter.setText(listText[msgData]);
				return true;
			case cuiMenuBar.EVENT_MENUBARITEMSELECT:
				switch (msgData)
				{
					case 0x00010001:
						fileNew();
						break;
					case 0x00010002:
						fileOpen();
						break;
					case 0x00010003:
						fileSave();
						break;
					case 0x00010004:
						fileSaveAs();
						break;
					case 0x00010005:
						fileClose();
						break;
					case 0x00010007:
						quit();
						break;
					case 0x00020001:
						editCopy();
						break;
					case 0x00020002:
						editPaste();
						break;
					case 0x00020003:
						editCut();
						break;
					case 0x00030001:
						help();
						break;
				}
				return true;
			case cuiTextEntry.EVENT_TEXTENTRY:
				banner = textEnter.getText();
			case cuiButton.EVENT_BUTTONPRESS:
				if (msgData == 69)
					quit();
				break;
		}
		return false;
	}
	private void quit()
	{
		cuiMessageBox msgBox = new cuiMessageBox("Quit Verification", "Really quit?", cuiMessageBox.MSGBOX_OK | cuiMessageBox.MSGBOX_CANCEL);
		if (msgBox.reply() == EVENT_OK)
			quit = true;
	}
	private void fileNew()
	{
	}
	private void fileOpen()
	{
		cuiMessageBox msgBox = new cuiMessageBox("OpenFile", "Select a file here, someday", cuiMessageBox.MSGBOX_OK);
		msgBox.reply();
	}
	private void fileSave()
	{
	}
	private void fileSaveAs()
	{
	}
	private void fileClose()
	{
	}
	private void editCopy()
	{
	}
	private void editPaste()
	{
	}
	private void editCut()
	{
	}
	private void help()
	{
	}
	
	public static void main(String args[])
	{
		TestCUI testApp = new TestCUI();
		testApp.start();
		testApp.eventTimeOut = 333;
		testApp.run(12);
		testApp.stop();
	}
}