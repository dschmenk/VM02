import apple2.*;

public class SimplePong
{
	public static void main(String[] args)
	{
		int xBall, yBall, ixBall, iyBall;
		int nxBall, nyBall, oxBall, oyBall, hPaddle, spin;
		
		/*
		 * Disable VBL interrupts.  Causes problems on IIc's.
		 */
		Mouse.disableIRQ();
		AppleStuff.loRes();
		/*
		 * Draw bounds.
		 */
		AppleStuff.lrHLine(0, 39, 0, 15);
		AppleStuff.lrVLine(0, 0, 47, 15);
		AppleStuff.lrVLine(39, 0, 47, 15);
		/*
		 * Initialize ball.
		 */
		xBall = 20*32;
		ixBall = 1;
		yBall = 24*32;
		iyBall = 1;
		while (!AppleStuff.button(0))
		{
			/*
			 * Update paddle.
			 */
			hPaddle = AppleStuff.paddle(0) >> 3;
			if (hPaddle > 31) hPaddle = 31;
			if (hPaddle > 0)
				AppleStuff.lrHLine(0, hPaddle - 1, 47, 0);
			AppleStuff.lrHLine(hPaddle, hPaddle + 8, 47, 15);
			if (hPaddle < 31)
				AppleStuff.lrHLine(hPaddle + 9, 39, 47, 0);
			/*
			 * Update ball.
			 */
			oxBall = xBall >> 5;
			oyBall = yBall >> 5;
			xBall += ixBall;
			if (xBall > (int)(38.9*32) || xBall < 1*32)
			{
				/*
				 * Bounce off left or right walls.
				 */
				AppleStuff.tone(20, 1);
				ixBall = -ixBall;
				xBall +=  ixBall;
			}
			nxBall = xBall >> 5;
			yBall += iyBall;
			if (yBall < 1*32)
			{
				/*
				 * Bounce off top wall.
				 */
				AppleStuff.tone(20, 1);
				iyBall = -iyBall;
				yBall +=  iyBall;
			}
			else if (yBall > 47*32)
			{
				if (nxBall >= hPaddle && nxBall <= hPaddle + 8)
				{
					/*
					 * Bounce off paddle.
					 */
					AppleStuff.tone(40, 1);			
					iyBall = -iyBall - 1;
					yBall +=  iyBall;
					spin   = (nxBall - hPaddle - 4) << 1;
					if (ixBall < 0)
						ixBall += spin;
					else
						ixBall -= spin;
				}
				else
				{
					/*
					 * Missed ball, restart.
					 */
					AppleStuff.tone(0,3);
					xBall = 20*32;
					nxBall = xBall >> 5;
					ixBall = (ixBall > 0) ? 1 : -1;
					yBall = 24*32;
					iyBall = 1;
				}
			}
			nyBall = yBall >> 5;
			if (oxBall != nxBall || oyBall != nyBall)
			{
				AppleStuff.lrPlot(oxBall, oyBall, 0);
				AppleStuff.lrPlot(nxBall, nyBall, 4);
			}
		}
		AppleStuff.text();
	}
}

