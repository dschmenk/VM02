import apple2.*;

public class Moire {

	static void fan(int xc, int yc, int s)
	{
		int x, y;
		
		for (x = 0; x < 279; x += s)
		{
			AppleStuff.hrColor(3);
			AppleStuff.hrLine(x, 0, xc, yc);
			AppleStuff.hrLineTo(279-x, 191);
			AppleStuff.hrColor(0);
			AppleStuff.hrLine(x+1, 0, xc, yc);
			AppleStuff.hrLineTo(278-x, 191);
		}
		for (y = 0; y < 191; y += s)
		{
			AppleStuff.hrColor(3);
			AppleStuff.hrLine(279, y, xc, yc);
			AppleStuff.hrLineTo(0, 191-y);
			AppleStuff.hrColor(0);
			AppleStuff.hrLine(279, y+1, xc, yc);
			AppleStuff.hrLineTo(0, 190-y);
		}
	}
	
	public static void main(String args[])
	{
		AppleStuff.hiRes();
		fan(160, 85, 3);
		while(!AppleStuff.keyPressed());
		AppleStuff.text();
	}
}