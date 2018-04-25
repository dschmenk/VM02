//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiTextEntry extends cuiControl
{
	public final static int EVENT_TEXTENTRY = 400;
	
	private String text;
	private byte textChars[];
	private byte   cursorPos, cursorSave, textLength;
	
	public cuiTextEntry(int enterX, int enterY, int enterWidth, String enterText, boolean enterFrame, int enterID, cuiControl enterOwner)
	{
		super(enterX, enterY, enterWidth, 1, enterID, (char)-1, 0, enterOwner);
		if (enterFrame)
			cui.frameRect(enterX - 1, enterY - 1, enterWidth + 2, 3, 0x80 | ' ', true, null);

		setText(enterText);
	}
	public cuiTextEntry(int enterX, int enterY, int enterWidth, boolean enterFrame, int enterID, cuiControl enterOwner)
	{
		super(enterX, enterY, enterWidth, 1, enterID, (char)-1, 0, enterOwner);
		if (enterFrame)
			cui.frameRect(enterX - 1, enterY - 1, enterWidth + 2, 3, 0x80 | ' ', true, null);
		setText(null);
	}
	
	private void textToChars()
	{
		textChars = new byte[width + 1];
		if (text != null)
		{
			int i;
			textLength = (byte)((text.length() <= width) ? text.length() : width);
			for (i = textLength - 1; i >= 0; i--)
				textChars[i] = (byte)(0x80 | text.charAt(i));
		}
		else
			textLength = 0;
		textChars[textLength] = (byte)(0x80 | ' ');
		cursorSave = textChars[cursorPos];
		textChars[cursorPos] = (byte)(cursorSave & (((cursorSave & 0xE0) == 0xC0) ? 0x1F : 0x7F));
		int textWidth = textLength + ((cursorPos == textLength) ? 1 : 0);
		cui.putChars(x, y, 0, textWidth, textChars);
		if (textWidth < width)
			cui.fillRect(x + textWidth, y, width - textWidth, 1, 0x80 | ' ', 0x80 | ' ');
	}
	private void charsToText()
	{
		int i;
		for (i = textLength - 1; i >= 0; i--)
			textChars[i] &= (byte)0x7F;
		text = new String(textChars, 0, textLength);
		if (isFocus)
			for (i = textLength - 1; i >= 0; i--)
				textChars[i] |= (byte)0x80;
		else
			textChars = null;
		cui.drawString(x, y, 0, width, text, 0x80 | ' ');
	}
	public void setText(String newText)
	{
		text = newText;
		if (text != null)
		{
			cui.drawString(x, y, 0, width, text, 0x80 | ' ');
			cursorPos = (byte)text.length();
		}
		else
		{
			cui.fillRect(x, y, width, 1, 0x80 | ' ', 0x80 | ' ');
			cursorPos = 0; 
		}
		if (cursorPos >= width)
			cursorPos = (byte)width;

		if (isFocus)
			textToChars();
	}
	public String getText()
	{
		if (textChars != null)
			charsToText();
		return text;
	}
	public cuiControl setFocus()
	{
		cuiControl prevFocus = super.setFocus();
		textToChars();
		return prevFocus;
	}
	public void unFocus()
	{
		super.unFocus();
		charsToText();
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				if (isFocus && ((cui.codeKey < 0x80) || (cui.codeKey == (cui.MODKEY_OPENAPPLE | cui.KEY_DELETE))))
				{
					textChars[cursorPos] = cursorSave;
					if (cui.codeKey >= ' ' && cui.codeKey <= '~')
					{
						if (textLength < width)
						{
							if (cursorPos < textLength++)
								for (int c = textLength - 1; c > cursorPos; c--)
									textChars[c] = textChars[c - 1];
							textChars[cursorPos++] = (byte)(0x80 | cui.codeKey);
						}
						else
							cui.tone(100, 15, 1);

					}
					else if (cui.codeKey == cui.KEY_DELETE)
					{
						if (cursorPos > 0)
						{
							textLength--;
							for (int c = --cursorPos; c < textLength ; c++)
								textChars[c] = textChars[c + 1];
						}
						else
							cui.tone(100, 15, 1);
					}
					else if (cui.codeKey == (cui.MODKEY_OPENAPPLE | cui.KEY_DELETE))
					{
						if (cursorPos < textLength)
						{
							textLength--;
							for (int c = cursorPos; c < textLength ; c++)
								textChars[c] = textChars[c + 1];
						}
					}
					else if (cui.codeKey == cui.KEY_ARROWLEFT)
					{
						if (cursorPos > 0)
							cursorPos--;
					}
 					else if (cui.codeKey == cui.KEY_ARROWRIGHT)
					{
						if (cursorPos < textLength)
							cursorPos++;
					}
					else if (cui.codeKey == cui.KEY_RETURN)
					{
						owner.event(EVENT_TEXTENTRY, ID);
					}
					if (cursorPos >= width)
					{
						cursorPos = (byte)(width - 1);
						cui.tone(100, 15, 1);
					}
					textChars[textLength] = (byte)(0x80 | ' ');
					cursorSave = textChars[cursorPos];
					textChars[cursorPos] = (byte)(cursorSave & (((cursorSave & 0xE0) == 0xC0) ? 0x1F : 0x7F));
					int textWidth = textLength + ((cursorPos == textLength) ? 1 : 0);
					cui.putChars(x, y, 0, textWidth, textChars);
					if (textWidth < width)
						cui.fillRect(x + textWidth, y, width - textWidth, 1, 0x80 | ' ', 0x80 | ' ');
					return true;
				}
				break;
			case EVENT_MOUSEBTTNDOWN:
				return true;
		}
		return false;
	}
}
