//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiScrollBar extends cuiControl
{
	public final static int EVENT_SCROLLUP       = 300;
	public final static int EVENT_SCROLLPAGEUP   = 301;
	public final static int EVENT_SCROLLTOP      = 302;
	public final static int EVENT_SCROLLDOWN     = 303;
	public final static int EVENT_SCROLLPAGEDOWN = 304;
	public final static int EVENT_SCROLLBOTTOM   = 305;
	public final static int EVENT_SCROLLTO       = 306;
	private int pos, range, indicator;
	
	public cuiScrollBar(int scrollX, int scrollY, int scrollHeight, int scrollRange, int scrollID, cuiControl scrollOwner)
	{
		super(scrollX, scrollY, 1, scrollHeight, scrollID, (char)-1, FLAGS_OWNERFOCUS | FLAGS_NOKEYBOARDFOCUS, scrollOwner);
		range = scrollRange;
		pos   = 0;
		cui.fillRect(x, y, 1, 1, 'R', 'R');
		cui.fillRect(x, y + height - 1, 1, 1, 'Q', 'Q');
		updateScrollBar();
	}
	
	public void setRange(int scrollRange)
	{
		range = scrollRange;
		updateScrollBar();
	}
	public void setPosition(int scrollPos)
	{
		pos = scrollPos;
		updateScrollBar();
	}
	public int getPosition()
	{
		return pos;
	}
	private void updateScrollBar()
	{
		indicator = y + 1 + pos * (height - 2) / range;
		cui.fillRect(x, y + 1, 1, height - 2, 'V', 'W');
		cui.fillRect(x, indicator, 1, 1, 'N', 'N');
		
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				if (owner.isFocus)
					switch (cui.codeKey)
					{
						case cui.KEY_ARROWUP:
							owner.event(EVENT_SCROLLUP, ID);
							return true;
						case cui.KEY_ARROWDOWN:
							owner.event(EVENT_SCROLLDOWN, ID);
							return true;
						case (cui.MODKEY_OPENAPPLE | cui.KEY_ARROWUP):
							owner.event(EVENT_SCROLLPAGEUP, ID);
							return true;
						case (cui.MODKEY_OPENAPPLE | cui.KEY_ARROWDOWN):
							owner.event(EVENT_SCROLLPAGEDOWN, ID);
							return true;
						case (cui.MODKEY_OPENAPPLE | ','):
							owner.event(EVENT_SCROLLTOP, ID);
							return true;
						case (cui.MODKEY_OPENAPPLE | '.'):
							owner.event(EVENT_SCROLLBOTTOM, ID);
							return true;
					}
				break;
			case EVENT_MOUSEBTTNDOWN:
				if (cui.yMouse == y)
				{
					do 
					{
						owner.event(EVENT_SCROLLUP, ID);
						cui.updateMouse();
					} while (cui.bttnMouse != 0);
				}
				else if (cui.yMouse == bottom - 1)
				{
					do 
					{
						owner.event(EVENT_SCROLLDOWN, ID);
						cui.updateMouse();
					} while (cui.bttnMouse != 0);
				}
				else if (cui.yMouse == indicator)
				{
					do
					{
						if ((cui.updateMouse() & cui.INPUT_MOUSEMOVE) != 0)
						{
							indicator = cui.yMouse;
							if (indicator <= y)
							{
								pos       = 0;
								indicator = y + 1;
							}
							else if (indicator >= (bottom - 1))
							{
								pos       = range - 1;
								indicator = bottom - 2;
							}
							else
							{
								pos = (indicator - y - 1) * range / (height - 2);
							}
							owner.event(EVENT_SCROLLTO, ID);
						}
						cui.updateMouse();
					} while (cui.bttnMouse != 0);
				}
				else
				{
					do
					{
						if (cui.yMouse < indicator)
							owner.event(EVENT_SCROLLPAGEUP, ID);
						else if (cui.yMouse > indicator)
							owner.event(EVENT_SCROLLPAGEDOWN, ID);
						cui.updateMouse();
					} while (cui.bttnMouse != 0);
				}
				return true;
		}
		return false;
	}
}
