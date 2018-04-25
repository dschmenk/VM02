//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiControl
{ 
	public final static int EVENT_KEYPRESS      = 1;
	public final static int EVENT_MOUSEBTTNDOWN = 2;
	public final static int EVENT_MOUSEDBLDOWN  = 4;
	public final static int EVENT_MOUSEMOVE     = 8;
	public final static int EVENT_MOUSEMASK     = 0xFFFFFFF1;
	public final static int EVENT_TIMEOUT       = 16;
	public final static int EVENT_CANCEL        = 20;
	public final static int EVENT_OK            = 21;
	public final static int EVENT_REDRAW        = 30;
	public final static int EVENT_SHUTDOWN      = 99;

	protected final static byte FLAGS_NOMOUSEFOCUS    = 1;
	protected final static byte FLAGS_NOKEYBOARDFOCUS = 2;
	protected final static byte FLAGS_OWNERFOCUS      = 4;
	
	protected static cuiControl list, listTop, focus;
	private   static byte       prevMouseX, prevMouseY;
	private   static int        prevMouseTime;
	public    boolean           isFocus;
	protected byte              x, y, width, height, right, bottom;
	public    byte              flags;
	protected int               ID;
	protected char              accel;
	protected cuiControl        prev, next, owner;

	public cuiControl(int ctrlX, int ctrlY, int ctrlWidth, int ctrlHeight, int ctrlID, char ctrlAccel, int ctrlFlags, cuiControl ctrlOwner)
	{
		x         = (byte)ctrlX;
		y         = (byte)ctrlY;
		width     = (byte)ctrlWidth;
		height    = (byte)ctrlHeight;
		right     = (byte)(ctrlX + ctrlWidth);
		bottom    = (byte)(ctrlY + ctrlHeight);
		ID        = ctrlID;
		accel     = ctrlAccel;
		flags     = (byte)ctrlFlags;
		owner     = (ctrlOwner != null) ? ctrlOwner : this;
		prev      = listTop;
		listTop   = this;
		if (prev != null)
			prev.next = this;
		isFocus   = false;
	}
	public void delete()
	{
		if (isFocus)
			focus.unFocus();
		if (next != null)
			next.delete();
		if (this == listTop)
			listTop = prev;
		if (prev != null)
		{
			prev.next = null;
			prev = null;
		}
		owner = null;
	}
	protected void unFocus()
	{
		if (isFocus)
		{
			focus   = null;
			isFocus = false;
		}
	}
	public static cuiControl clearFocus()
	{
		cuiControl prevFocus = focus;
		if (focus != null)
			focus.unFocus();
		return prevFocus;
	}
	public cuiControl setFocus()
	{
		cuiControl prevFocus = focus;

		if (!isFocus)
		{
			if ((flags & FLAGS_OWNERFOCUS) != 0)
			{
				if (!owner.isFocus)
					owner.setFocus();
			}
			else
			{
				if (prevFocus != null)
					prevFocus.unFocus();		
				focus   = this;
				isFocus = true;
			}
		}
		return prevFocus;
	}
	public void nextFocus(cuiControl wrapFocus)
	{
		cuiControl ctrlFocus = next;
		unFocus();
		if (ctrlFocus == null)
			ctrlFocus = wrapFocus;
		while ((ctrlFocus.flags & FLAGS_NOKEYBOARDFOCUS) != 0)
		{
			ctrlFocus = ctrlFocus.next;
			if (ctrlFocus == null)
				ctrlFocus = wrapFocus;
		}
		ctrlFocus.setFocus();
	}
	public boolean event(int msg, int msgData)
	{
		return false;
	}
	public static void eventDispatch(int mask, int timeOut)
	{
		int msg   = 0;
		int input = cui.getInputEvent(mask, timeOut);
		while (input != 0)
		{
			if ((input & cui.INPUT_KEYBOARD) != 0)
			{
				input &= ~cui.INPUT_KEYBOARD;
				if (focus != null && focus.event(EVENT_KEYPRESS, 0)) // give focus first crack at keyboard input
					msg = 0;
				else
					msg = EVENT_KEYPRESS;
			}
			else if ((input & cui.INPUT_MOUSEBTTNDOWN) != 0)
			{
				input &= ~cui.INPUT_MOUSEBTTNDOWN;
				if ((cui.timeMouseEvent - prevMouseTime < 500)
				 && (cui.xMouse == prevMouseX)
				 && (cui.yMouse == prevMouseY))
					msg = EVENT_MOUSEDBLDOWN;
				else
					msg = EVENT_MOUSEBTTNDOWN;
				prevMouseTime = cui.timeMouseEvent;
				prevMouseX    = cui.xMouse;
				prevMouseY    = cui.yMouse;
				if ((focus != null)
				 && (cui.xMouse >= focus.x)
				 && (cui.xMouse <  focus.right)
				 && (cui.yMouse >= focus.y)
				 && (cui.yMouse <  focus.bottom)
				 && focus.event(msg, 0))
				 	msg = 0;
			}
			else if ((input & cui.INPUT_MOUSEMOVE) != 0)
			{
				input &= ~cui.INPUT_MOUSEMOVE;
				msg = EVENT_MOUSEMOVE;
			}
			else if ((input & cui.INPUT_TIMEOUT) != 0)
			{
				input &= ~cui.INPUT_TIMEOUT;
				msg = EVENT_TIMEOUT;
			}
			if (msg != 0)
			{
				for (cuiControl ctrlHandler = listTop; ctrlHandler != null; ctrlHandler = ctrlHandler.prev)
				{
					if ((msg & EVENT_MOUSEMASK) == 0)
					{
						if ((cui.xMouse >= ctrlHandler.x)
						 && (cui.xMouse <  ctrlHandler.right)
						 && (cui.yMouse >= ctrlHandler.y)
						 && (cui.yMouse <  ctrlHandler.bottom))
						{
							if ((ctrlHandler.flags & FLAGS_OWNERFOCUS) == 0)
							{
								if (!ctrlHandler.isFocus
								 && ((ctrlHandler.flags & FLAGS_NOMOUSEFOCUS) == 0))
									ctrlHandler.setFocus();
							}
							else
							{
								if (!ctrlHandler.owner.isFocus
								 && ((ctrlHandler.owner.flags & FLAGS_NOMOUSEFOCUS) == 0))
									ctrlHandler.owner.setFocus();
							}
							ctrlHandler.event(msg, 0);
							break;
						}
					}
					else if ((focus != ctrlHandler) && ctrlHandler.event(msg, 0))
						break;

				}
			}
		}
	}
}
