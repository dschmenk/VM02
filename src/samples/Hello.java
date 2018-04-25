import apple2.*;
import java.io.*;

public class Hello {
	public static int getLine(byte line[]) throws IOException
	{
		int k,i = 0;
		
		while (i < line.length)
		{
			k = System.in.read();
			switch (k)
			{
				case -1:
				case '\r':
				case '\n':
					return i;
				case 0x08: // BS
				case 0x7F: // DEL
					if (i > 0)
					{
						i--;
						System.out.print((char)8); // BS
					}
					else
						System.out.print((char)7); // BELL
					break;
				default:
					line[i++] = (byte)k;
			}
		}
		return i;
	}
		
	public static void main(String args[]) throws IOException
	{
		int i;
		byte inputLine[] = new byte[80];
		
//		vm02.call(0, 0xC300);
		System.out.print("Hello. Enter your name:");
		System.out.flush();
		i = getLine(inputLine);
		System.out.print('\n');
		System.out.print("Nice to meet you, ");
		System.out.write(inputLine, 0, i);
		System.out.println('.');
	}
}
