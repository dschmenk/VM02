//
//  java - character user interface
// 
package org.vm02.cui;

public class cuiButton extends cuiControl
{
	public final static int EVENT_BUTTONPRESS = 100;
	private String label;
	private byte xLabel, yLabel;
	
	public cuiButton(int bttnX, int bttnY, int bttnWidth, int bttnHeight, String bttnLabel, boolean frame, char bttnAccelChar, int bttnID, cuiControl bttnOwner)
	{
		super(bttnX, bttnY, bttnWidth, bttnHeight, bttnID, bttnAccelChar, FLAGS_NOMOUSEFOCUS, bttnOwner);
		label = bttnLabel;
		xLabel = (byte)(x + ((width - label.length()) >> 1));
		yLabel = (byte)(y + (height >> 1));
		if (frame)
		{
			cui.frameRect(bttnX - 1, bttnY - 1, bttnWidth + 2, bttnHeight + 2, 0x80 | ' ', true, null);
			cui.drawString(xLabel, yLabel, label);
		}
		else
			drawNormalButton();
	}
	
	private void drawNormalButton()
	{
		cui.fillRect(x, y, width, height, 0x80|' ', 0x80|' ');
		if (focus == this)
			cui.drawInverseString(xLabel, yLabel, label);
		else
			cui.drawString(xLabel, yLabel, label);
	}
	private void drawInvertedButton()
	{
		//cui.tone(254, 80, 4);
		cui.fillRect(x, y, width, height, ' ', ' ');
		cui.drawInverseString(xLabel, yLabel, label);
	}
	public cuiControl setFocus()
	{
		cuiControl prevFocus = super.setFocus();
		drawNormalButton();
		return prevFocus;
	}
	public void unFocus()
	{
		super.unFocus();
		drawNormalButton();
	}
	public boolean event(int msg, int msgData)
	{
		switch (msg)
		{
			case EVENT_KEYPRESS:
				if ((cui.codeKey == accel)
				 || (isFocus && (cui.codeKey == cui.KEY_RETURN || cui.codeKey == cui.KEY_SPACEBAR)))
				{
					drawInvertedButton();
					drawNormalButton();
					owner.event(EVENT_BUTTONPRESS, ID);
					return true;
				}
				break;
			case EVENT_MOUSEBTTNDOWN:
				boolean inv = true;
				drawInvertedButton();
				while (cui.bttnMouse != 0)
					if ((cui.updateMouse() & cui.INPUT_MOUSEMOVE) != 0)
					{
						if ((cui.xMouse >= x)
						 && (cui.xMouse < right)
						 && (cui.yMouse >= y)
						 && (cui.yMouse < bottom))
						 {
							if (!inv)
							{
								drawInvertedButton();
								inv = true;
							}
						 }
						 else
						 {
							if (inv)
							{
								drawNormalButton();
								inv = false;
							}
						 }
					}
				if (inv) drawNormalButton();
				if ((cui.xMouse >= x)
				 && (cui.xMouse < right)
				 && (cui.yMouse >= y)
				 && (cui.yMouse < bottom))
				 {
					owner.event(EVENT_BUTTONPRESS, ID);
					return true;
				}
				break;
		}
		return false;
	}
}
