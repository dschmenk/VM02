import java.io.*;

public class StressMem {
	public static void main(String args[]) throws IOException
	{
		int i, j;
		byte stress[][] = new byte[256][256];
		
		for (i = 0; i < 256; i++)
		{
			for (j = 0; j < 256; j++)
				stress[i][j] = 69;
			System.out.println(i);
		}
	}
}
