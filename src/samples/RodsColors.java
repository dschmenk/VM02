import apple2.*;

public class RodsColors {

	public static void main(String args[])
	{
		int i, j, k, w, fmi, fmk, color;
		
		AppleStuff.loResMix();
		conio.gotoXY(10,22);
		conio.print("PRESS ANY KEY TO EXIT.");
		while (true) {
			for (w = 3; w <= 50; ++w) {
				for (i = 1; i <= 19; ++i) {
					for (j = 0; j <= 19; ++j) {
						k = i + j;
						color = (j * 3) / (i + 3) + i * w / 12;
						fmi = 40 - i;
						fmk = 40 - k;
						AppleStuff.lrColor(color);
						AppleStuff.lrPlot(i, k);
						AppleStuff.lrPlot(k, i);
						AppleStuff.lrPlot(fmi, fmk);
						AppleStuff.lrPlot(fmk, fmi);
						AppleStuff.lrPlot(k, fmi);
						AppleStuff.lrPlot(fmi, k);
						AppleStuff.lrPlot(i, fmk);
						AppleStuff.lrPlot(fmk, i);
						if (AppleStuff.keyPressed()) {
							AppleStuff.text();
							return;
						}
					}
				}
			}
		}
	}
}