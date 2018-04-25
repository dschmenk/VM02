//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiMessageBox extends cuiPopUp
{	
	public final static int MSGBOX_OK     = 1;
	public final static int MSGBOX_CANCEL = 2;
	private int msgReply;
	
	public cuiMessageBox(String msgTitle, String msgText, int buttons)
	{
		super(20, 8, 40, 9, false);
		drawBox(msgTitle, null, msgText, null, buttons);
	}
	public cuiMessageBox(String msgTitle, String msgText1, String msgText2, int buttons)
	{
		super(20, 8, 40, 9, false);
		drawBox(msgTitle, msgText1, null, msgText2, buttons);
	}
	public cuiMessageBox(String msgTitle, String msgText1, String msgText2, String msgText3, int buttons)
	{
		super(20, 8, 40, 9, false);
		drawBox(msgTitle, msgText1, msgText2, msgText3, buttons);
	}
	private void drawBox(String msgTitle, String msgText1, String msgText2, String msgText3, int buttons)
	{
		cuiButton bttnCancel, bttnOK;
		cui.fillRect(20, 7, 40, 1, ' ', ' ');
		cui.drawInverseString(20 + ((40 - msgTitle.length()) >> 1), 7, msgTitle);
		if (msgText1 != null) cui.drawString(20 + ((40 - msgText1.length()) >> 1), 9, msgText1);
		if (msgText2 != null) cui.drawString(20 + ((40 - msgText2.length()) >> 1), 10, msgText2);
		if (msgText3 != null) cui.drawString(20 + ((40 - msgText3.length()) >> 1), 11, msgText3);
		if ((buttons & MSGBOX_CANCEL) != 0)
			bttnCancel = new cuiButton(26, 13, 12, 3, "Cancel", true, cui.KEY_ESC, EVENT_CANCEL, (cuiControl)this);
		if ((buttons & MSGBOX_OK) != 0)
			bttnOK = new cuiButton(42, 13, 12, 3, "Okay", true, cui.KEY_RETURN, EVENT_OK, (cuiControl)this);
	}
	public int reply()
	{
		msgReply = -1;
		while (msgReply < 0) eventDispatch(cui.INPUT_KEYBOARD | cui.INPUT_MOUSEBTTNDOWN, -1);
		delete();
		return msgReply;
	}
	public boolean event(int msg, int msgData)
	{
		if (msg == cuiButton.EVENT_BUTTONPRESS)
			msgReply = msgData;
		return true;
	}

}
