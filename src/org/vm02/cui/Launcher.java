package org.vm02.cui;
import apple2.*;

public class Launcher extends cuiApp
{
	public static String Months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
	private static byte infoBuilder[] = {(byte)'T', (byte)'Y', (byte)'P',
	                                     (byte)' ', (byte)' ', (byte)' ',
	                                     (byte)'0', (byte)'0', (byte)'-', (byte)'J', (byte)'a', (byte)'n', (byte)'-', (byte)'0', (byte)'0',
	                                     (byte)' ', (byte)' ', (byte)' ',
	                                     (byte)'0', (byte)'0', (byte)':', (byte)'0', (byte)'0', (byte)' ', (byte)'A', (byte)'M'};
	private int ioBuffer;
	private String dirPrefix, launchCmd;
	private String dirFileName[];
	private byte   dirFileType[];
	private String dirFileInfo[];
	private short  dirFileLength[];
	private short  dirAuxType[];
	private short  dirModDate[];
	private cuiListBox fileList;
	private cuiTextEntry params;

	public void start(String path)
	{
		super.start();
		ioBuffer  = ProDOS.allocIOBuffer();
		dirPrefix = ProDOS.getPrefix();
		if (path != null)
		{
			if (path.charAt(path.length() - 1) != '/')
				path += '/';
			if (path.charAt(0) == '/')
				dirPrefix = path;
			else
				dirPrefix += path;
		}
		cui.fillRect(2, 20, 14, 3, 0x80 | ' ', 0x80 | ' ');
		cui.drawString(4, 21, "Parameters:");
		params             = new cuiTextEntry(17, 21, 60, true, 1, (cuiControl)this);
		cuiButton volBttn  = new cuiButton(61, 2,  16, 1, "Volumes:TAB",  true, (char)cui.KEY_TAB,                1, (cuiControl)this);
		cuiButton upBttn   = new cuiButton(61, 6,  16, 1, "Up Level:ESC", true, (char)cui.KEY_ESC,                2, (cuiControl)this);
		cuiButton runBttn  = new cuiButton(61, 10, 16, 1, "Launch:OA-L",  true, (char)(cui.MODKEY_OPENAPPLE|'l'), 3, (cuiControl)this);
		cuiButton exitBttn = new cuiButton(61, 17, 16, 1, "Exit:OA-X",    true, (char)(cui.MODKEY_OPENAPPLE|'x'), 4, (cuiControl)this);
		dirLoadList();
	}
	public void stop()
	{
		ProDOS.freeIOBuffer(ioBuffer);
		super.stop();
		super.chain(launchCmd);
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				switch (cui.codeKey)
				{
					case cui.KEY_ARROWLEFT:
						dirExit();
						return true;
					case cui.KEY_ARROWRIGHT:
						dirEnter(fileList.getHilite());
						return true;
					case cui.MODKEY_OPENAPPLE | cui.KEY_TAB:
						if (focus != null)
							focus.nextFocus(this.next);
						else 
							this.nextFocus(this.next);
						return true;
				}
				break;
			case cuiListBox.EVENT_LISTITEMSELECT:
				if (!dirEnter(msgData))
					launch(msgData);
				return true;
			case cuiListBox.EVENT_LISTITEMDRAW:
			case cuiListBox.EVENT_LISTITEMDRAWHILITE:
				int itemX     = (msgData >> 24) + 1;
				int itemY     = (msgData >> 16) & 0xFF;
				int itemWidth = (msgData >> 8) & 0xFF;
				int itemIndex = msgData & 0xFF;
				if (dirFileType[itemIndex] == 0x0F) // Is it a directory?
					cui.drawMouseString(itemX, itemY, "XY"); 
				else
					cui.drawString(itemX, itemY, dirFileType[itemIndex] == (byte)0xED ? " *" : "  ");
				if (msg == cuiListBox.EVENT_LISTITEMDRAWHILITE)
				{
					cui.drawInverseString(itemX + 3, itemY, 0, 16, dirFileName[itemIndex], ' ');
					cui.drawInverseString(itemX + 19, itemY, dirFileInfo[itemIndex]);
				}
				else
				{
					cui.drawString(itemX + 3, itemY, 0, 16, dirFileName[itemIndex], 0x80 | ' ');
					cui.drawString(itemX + 19, itemY, dirFileInfo[itemIndex]);
				}
				return true;
			case cuiButton.EVENT_BUTTONPRESS:
				switch (msgData)
				{
					case 1:
						dirPrefix = "/";
						dirLoadList();
						return true;
					case 2:
						dirExit();
						return true;
					case 3:
						launch(fileList.getHilite());
						return true;
					case 4:
						quit = true;
						return true;
				}
			case cuiTextEntry.EVENT_TEXTENTRY:
				launch(fileList.getHilite());
				return true;
				
		}
		return false;
	}
	public boolean launch(int index)
	{
		if (dirFileType[index] == (byte)0xED)
		{
			launchCmd = dirPrefix + dirFileName[index] + " " + params.getText();
			quit = true;
			return true;
		}
		return false;
	}
	public boolean dirEnter(int index)
	{
		if (dirFileType[index] == (byte)0x0F) // Is it a directory?
		{
			dirPrefix = dirPrefix + dirFileName[index] + "/";
			dirLoadList();
			return true;
		}
		return false;
	}
	public void dirExit()
	{
		if (dirPrefix != "/")
		{
			byte prefixChars[] = new byte[dirPrefix.length()];
			int lastDir = 0;
			for (int i = dirPrefix.length() - 2; i >= 0; i--)
			{
				prefixChars[i] = (byte)dirPrefix.charAt(i);
				if (prefixChars[i] == '/' && lastDir == 0)
					lastDir = i + 1;
			}
			if (lastDir > 1)
				dirPrefix = new String(prefixChars, 0, lastDir);
			else
				dirPrefix = "/";
			dirLoadList();
		}
	}
	public void dirLoadList()
	{
		byte prevCursor = cui.setMouseCursor('C'); // hour glass
		if (fileList != null) fileList.delete();
		dirFileName = null;
		dirFileType = null;
		dirFileInfo = null;
		if (dirPrefix == null || dirPrefix == "/")
		{
			dirFileName = ProDOS.online();
			dirFileType = new byte[dirFileName.length];
			dirFileInfo = new String[dirFileName.length];
			for (int i = 0; i < dirFileType.length; i++)
			{
				dirFileType[i] = 0x0F; // set to directory
				dirFileInfo[i] = "VOL                       "; // no mod time
			}
		}
		else
		{
			int entryOffset, entriesBlock = 0, entryLength = 0;
			int refNum = ProDOS.open(dirPrefix, ioBuffer);
			if (refNum < 0)
			{
				cui.setMouseCursor(prevCursor);
				cuiMessageBox errBox = new cuiMessageBox("ProDOS Error", "Unable to read directory:", dirPrefix, cuiMessageBox.MSGBOX_OK);
				errBox.reply();
				quit = true;
				return;
			}
			byte dataBuffer[] = new byte[512];
			int fileCount     = 0;
			int entry         = 0;
			int firstBlock    = 1;
			do
			{
				if (ProDOS.read(refNum, dataBuffer) == 512)
				{
					if (firstBlock == 1)
					{
						entryLength  =   dataBuffer[0x23] & 0xFF;
						entriesBlock =   dataBuffer[0x24] & 0xFF;
						fileCount    =  (dataBuffer[0x25] & 0xFF)
						             | ((dataBuffer[0x26] & 0xFF) << 8);
						//cui.drawString(10, 0, "Dir file count: " + fileCount + "    ");
						if (fileCount > 64) fileCount = 64;
						if (fileCount > 0)
						{
							dirFileName = new String[fileCount];
							dirFileType = new byte[fileCount];
							dirFileInfo = new String[fileCount];
						}
						entryOffset = entryLength + 4;
					}
					else
						entryOffset = 4;
					for (int i = firstBlock; i < entriesBlock; i++)
					{
						int typeLen = dataBuffer[entryOffset];
						if (typeLen != 0)
						{
							dirFileName[entry] = new String(dataBuffer, entryOffset + 1, typeLen & 0x0F);
							dirFileType[entry] = dataBuffer[entryOffset + 0x10];
							int modDate        =  (dataBuffer[entryOffset + 0x21] & 0xFF)
							                   | ((dataBuffer[entryOffset + 0x22] & 0xFF) << 8);
							int modTime        =  (dataBuffer[entryOffset + 0x23] & 0xFF)
							                   | ((dataBuffer[entryOffset + 0x24] & 0xFF) << 8);
							dirFileInfo[entry] = infoString(dirFileType[entry], modDate, modTime);
							entry++;
							if (--fileCount == 0) break;
						}
						entryOffset += entryLength;
					}
					firstBlock  = 0;
				}
				else
					fileCount = 0;
			} while (fileCount > 0);
			ProDOS.close(refNum);
		}
		fileList = new cuiListBox(3, 2, 50, 16, dirFileName.length, (cuiControl)this);
		cui.drawInverseString(3, 1, dirPrefix);
		fileList.setFocus();
		cui.setMouseCursor(prevCursor);
	}
	public String infoString(byte type, int date, int time)
	{
		byte infoChars[] = infoBuilder;
		switch (type)
		{
			case (byte)0x04:
				infoChars[0] = (byte)'T';
				infoChars[1] = (byte)'X';
				infoChars[2] = (byte)'T';
				break;
			case (byte)0x06:
				infoChars[0] = (byte)'B';
				infoChars[1] = (byte)'I';
				infoChars[2] = (byte)'N';
				break;
			case (byte)0x0F:
				infoChars[0] = (byte)'D';
				infoChars[1] = (byte)'I';
				infoChars[2] = (byte)'R';
				break;
			case (byte)0xED:
				infoChars[0] = (byte)'J';
				infoChars[1] = (byte)'V';
				infoChars[2] = (byte)'M';
				break;
			case (byte)0xFC:
				infoChars[0] = (byte)'B';
				infoChars[1] = (byte)'A';
				infoChars[2] = (byte)'S';
				break;
			case (byte)0xFD:
				infoChars[0] = (byte)'V';
				infoChars[1] = (byte)'A';
				infoChars[2] = (byte)'R';
				break;
			case (byte)0xFE:
				infoChars[0] = (byte)'R';
				infoChars[1] = (byte)'E';
				infoChars[2] = (byte)'L';
				break;
			case (byte)0xFF:
				infoChars[0] = (byte)'S';
				infoChars[1] = (byte)'Y';
				infoChars[2] = (byte)'S';
				break;
			default:
				infoChars[0] = (byte)'$';
				infoChars[1] = (byte)(((type >> 4) & 0x0F) + '0');
				if (infoChars[1] > '9')
					infoChars[1] += (byte)('A' - '9' - 1);
				infoChars[2] = (byte)((type & 0x0F) + '0');
				if (infoChars[2] > '9')
					infoChars[2] += (byte)('A' - '9' - 1);
		}
		int year  =  (date >> 9) & 0x7F;
		int month = ((date >> 5) & 0x0F) - 1;
		int day   =   date       & 0x1F;
		if (month < 0 || month > 11) // bogus month
		{
			year  = 0;
			month = 0;
			day   = 0;
		}
		infoChars[6]  = (byte)((day  / 10) + '0');
		infoChars[7]  = (byte)((day  % 10) + '0');
		infoChars[9]  = (byte)Months[month].charAt(0);
		infoChars[10] = (byte)Months[month].charAt(1);
		infoChars[11] = (byte)Months[month].charAt(2);
		infoChars[13] = (byte)((year / 10) + '0');
		infoChars[14] = (byte)((year % 10) + '0');
		int hour   = (time >> 8) & 0x1F;
		int minute =  time       & 0x3F;
		if (hour > 12)
		{
			hour -= 12;
			infoChars[24] = (byte)'P';
		}
		else
		{
			infoChars[24] = (byte)'A';
		}
		infoChars[18] = (byte)((hour   / 10) + '0');
		infoChars[19] = (byte)((hour   % 10) + '0');
		infoChars[21] = (byte)((minute / 10) + '0');
		infoChars[22] = (byte)((minute % 10) + '0');
		return new String(infoChars, 0, 26);
	}
	public static void main(String args[])
	{
		Launcher launchApp = new Launcher();
		launchApp.start((args.length > 0) ? args[0] : (String)null);
		launchApp.run(8);
		launchApp.stop();
	}
}