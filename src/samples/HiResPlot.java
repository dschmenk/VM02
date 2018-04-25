import apple2.*;

public class HiResPlot
{

	public static void main(String args[])
	{
		float x, y, z;
		
		AppleStuff.hiRes();
		AppleStuff.hrColor(3);
		for (y = -1.0F; y <= 1.0F; y += 0.1F)
		{
			x = -1.0F;
			z = -(x * x - y * y);
			AppleStuff.hrPlot((int)(x*100) + (int)(y*39) + 140, (int)(y*10) + (int)(z*40) + 96);
			while (x < 1.0F)
			{
				x += 0.1F;
				z = -(x * x - y * y);
				AppleStuff.hrLineTo((int)(x*100) + (int)(y*39) + 140, (int)(y*10) + (int)(z*40) + 96);
			}
		}	
		AppleStuff.getKey();
		AppleStuff.text();
	}
}