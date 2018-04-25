//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiListBox extends cuiControl
{
	public final static int EVENT_LISTITEMSELECT     = 500;
	public final static int EVENT_LISTITEMDRAW       = 501;
	public final static int EVENT_LISTITEMDRAWHILITE = 502;
	protected int          itemCount, itemTop, hilite;
	protected cuiScrollBar listScroll;
	
	public cuiListBox(int listX, int listY, int listWidth, int listHeight, int listCount, cuiControl listOwner)
	{
		super(listX, listY, listWidth, listHeight, 0, (char)-1, 0, listOwner);
		int i;
		cui.frameRect(listX - 1, listY - 1, listWidth + 2, listHeight + 2, 0x80 | ' ', true, null);
		itemCount = listCount;
		itemTop   = hilite = 0;
		if (listCount > listHeight)
		{
			width--;
			right--;
			listScroll = new cuiScrollBar(x + width, y, height, listCount, 0, (cuiControl)this);
			i = listHeight;
		}
		else
			i = listCount;
		while (--i >= 0)
			drawItem(i);
	}
	protected void drawItem(int itemIndex)
	{
		owner.event(EVENT_LISTITEMDRAW, (x << 24) | ((y + (itemIndex - itemTop)) << 16) | (width << 8) | (itemIndex & 0xFF));
	}
	protected void drawSelectedItem(int itemIndex)
	{
		owner.event(EVENT_LISTITEMDRAWHILITE, (x << 24) | ((y + (itemIndex - itemTop)) << 16) | (width << 8) | (itemIndex & 0xFF));
	}
	public int getHilite()
	{
		return itemTop + hilite;
	}
	private void scrollList(int dir)
	{
		int diff;
		int prevHilite = hilite;
		int prevTop    = itemTop;
		int newTop     = itemTop;
		hilite += dir;
		if (hilite < 0)
		{
			newTop += hilite;
			hilite  = 0;
			if (newTop < 0)
				newTop = 0;
		}
		else
		{
			if (hilite >= height)
			{
				newTop += hilite - height + 1;
				hilite  = height - 1;
			}
			if (hilite + newTop >= itemCount)
			{
				newTop = itemCount - height;
				if (newTop < 0)
				{
					newTop = 0;
					hilite = itemCount - 1;
				}
			}
		}
		drawItem(prevTop + prevHilite);
		itemTop = newTop;
		if (newTop < prevTop)
		{
			diff = prevTop - newTop;
			if (diff < height)
				cui.scrollRect(x, y, width, height - diff, y + diff);
			else
				diff = height;
			while (--diff > 0)
				drawItem(newTop + diff);
		}
		else if (newTop > prevTop)
		{
			diff = newTop - prevTop;
			if (diff < height)
				cui.scrollRect(x, y + diff, width, height - diff, y);
			else
				diff = height;
			while (--diff > 0)
				drawItem(newTop + height - diff - 1);
		}
		drawSelectedItem(newTop + hilite);
		if (listScroll != null) listScroll.setPosition(newTop + hilite);
	}
	public cuiControl setFocus()
	{
		cuiControl prevFocus = super.setFocus();
		drawSelectedItem(itemTop + hilite);
		return prevFocus;
	}
	public void unFocus()
	{
		super.unFocus();
		drawItem(itemTop + hilite);
	}
	public void delete()
	{
		if (listScroll != null)
		{
			listScroll.delete();
			listScroll = null;
		}
		super.delete();
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				if (isFocus)
				{
					if (listScroll != null && listScroll.event(msg, msgData))
						return true;
					if ((cui.codeKey == cui.KEY_RETURN) || (cui.codeKey == cui.KEY_SPACEBAR))
					{
						owner.event(EVENT_LISTITEMSELECT, itemTop + hilite);
						return true;
					}
					if (cui.codeKey == cui.KEY_ARROWUP)
					{
						scrollList(-1);
						return true;
					}
					if (cui.codeKey == cui.KEY_ARROWDOWN)
					{
						scrollList(1);
						return true;
					}
					if (cui.codeKey == (0x80 | cui.KEY_ARROWUP))
					{
						scrollList(-(height >> 1));
						return true;
					}
					if (cui.codeKey == (0x80 | cui.KEY_ARROWDOWN))
					{
						scrollList(height >> 1);
						return true;
					}
				}
				break;
			case EVENT_MOUSEBTTNDOWN:
				while (cui.bttnMouse != 0)
				{
					int dir = cui.yMouse - y - hilite;
					if (dir != 0)
						scrollList(dir);
					cui.updateMouse();
				}
				return true;
			case EVENT_MOUSEDBLDOWN:
				owner.event(EVENT_LISTITEMSELECT, itemTop + hilite);
				return true;
			case cuiScrollBar.EVENT_SCROLLUP:
				scrollList(-1);
				return true;
			case cuiScrollBar.EVENT_SCROLLDOWN:
				scrollList(1);
				return true;
			case cuiScrollBar.EVENT_SCROLLPAGEUP:
				scrollList(-(height >> 1));
				return true;
			case cuiScrollBar.EVENT_SCROLLPAGEDOWN:
				scrollList(height >> 1);
				return true;
			case cuiScrollBar.EVENT_SCROLLTO:
				scrollList(listScroll.getPosition() - itemTop - hilite);
				return true;
			case cuiScrollBar.EVENT_SCROLLTOP:
				scrollList(-(itemTop + hilite));
				return true;
			case cuiScrollBar.EVENT_SCROLLBOTTOM:
				scrollList(itemCount - itemTop);
				return true;
		}
		return false;
	}

}
