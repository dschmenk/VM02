import apple2.*;

public class HiResDemo
{
	private static short scanLineAddr[];
	
	public static void bitBLT(int xCoord, int yCoord, byte sprite[][], int width, int height, int fillMode)
	{
		int offset = 0;
		int mod7 = xCoord % 7;
		int div7 = xCoord / 7;
		
		switch (fillMode)
		{
			case 0:
				while (height-- > 0)
				{
					vm02.pokeWord(0xDE, (short)(scanLineAddr[yCoord++] + div7)); // DSTADDR
					vm02.call(width, 0x44); // MEMCLR
				}
				break;
			case 1:
				while (height-- > 0)
				{
					vm02.pokeBytes(scanLineAddr[yCoord++] + div7,  sprite[mod7], offset, width);
					offset += width;
				}
				break;
			case 15:
				while (height-- > 0)
				{
					vm02.pokeWord(0xDE, (short)(scanLineAddr[yCoord++] + div7)); // DSTADDR
					vm02.call(0xFF0000 | width, 0x5A); // MEMSET
				}
				break;
		}
	}
	public static void main(String args[])
	{
		byte ball[][] = {{0x1C, 0x00, 0x3E, 0x00, 0x7F, 0x00, 0x7F, 0x00, 0x7F, 0x00, 0x3E, 0x00, 0x1C, 0x00},
			 {0x38, 0x00, 0x7C, 0x00, 0x7E, 0x01, 0x7E, 0x01, 0x7E, 0x01, 0x7C, 0x00, 0x38, 0x00},
			 {0x70, 0x00, 0x78, 0x01, 0x7C, 0x03, 0x7C, 0x03, 0x7C, 0x03, 0x78, 0x01, 0x70, 0x00},
			 {0x60, 0x01, 0x70, 0x03, 0x78, 0x07, 0x78, 0x07, 0x78, 0x07, 0x70, 0x03, 0x60, 0x01},
			 {0x40, 0x03, 0x60, 0x07, 0x70, 0x0F, 0x70, 0x0F, 0x70, 0x0F, 0x60, 0x07, 0x40, 0x03},
			 {0x00, 0x07, 0x40, 0x0F, 0x60, 0x1F, 0x60, 0x1F, 0x60, 0x1F, 0x40, 0x0F, 0x00, 0x07},
			 {0x00, 0x0E, 0x00, 0x1F, 0x40, 0x3F, 0x40, 0x3F, 0x40, 0x3F, 0x00, 0x1F, 0x00, 0x0E}};
		int y, x, yc, xc, w, h, w7, ox, oy;
		
		if (AppleStuff.hiRes() == false)
		{
			conio.println("Unable to allocate Hi Res screen.");
			return;
		}
		//
		// Fill scanline address array
		//
		scanLineAddr = new short[192];
		for (y = 0; y < 192; y++)
		{
			int y210 = y & 0x07;
			int y543 = y & 0x38;
			int y76  = y & 0xC0;
			scanLineAddr[y] = (short)(0x4000 | (y210 << 10) | (y543 << 4) | (y76 >> 1) | (y76 >> 3));
		}
		//
		// Bounce ball around screen
		//
		ox = x = 140;
		oy = y = 86;
		xc = 1;
		yc = 1;
		w  = 2;
		h  = 7;
		w7 = w * 7;
		while (!AppleStuff.keyPressed())
		{
			bitBLT(ox, oy, ball, w, h, 0);
			bitBLT(x, y, ball, w, h, 1);
			ox = x;
			x += xc;
			if (x < 0)
			{
				xc = -xc;
				x  = 0;
			}	
			if (x > (279 - w7))
			{
				xc = -xc;
				x  = (279 - w7);
			}
			oy = y;
			y += yc;
			if (y < 0)
			{
				yc = -yc;
				y  = 0;
			}
			if (y > (191 - h))
			{
				yc = -yc;
				y  = (191 - h);
			}
		}
		AppleStuff.getKey();
		AppleStuff.text();
	}
}