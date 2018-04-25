import apple2.*;

public class List
{	
	private static char spinner[] = {'!', '/', '-', '\\'};
	public static void show_page(String line_buffer[], int topline)
	{
		//
		// Set text cursor home using direct calls into ROM
		//
		vm02.pokeByte(0x24, (byte)0);
		vm02.pokeByte(0x25, (byte)0);
		vm02.call(0, 0xFBC1);
		for (int row = 0; row < 23; row++)
			//
			// Call directly into VM02 to display a string
			//
			vm02.call(vm02.refAsBits(line_buffer[topline + row]), 0x52);
	}
	public static void main(String args[])
	{
		int io_buffer, ref_num, bytes_read, key, last_line, current_line, current_pos, spin;
		boolean carriage_return;
		String list_file;
		String line_buffer[] = new String[1000];
		byte data_buffer[]   = new byte[512];
		byte fill_buffer[]   = new byte[40];

		list_file = (args.length == 1) ? args[0] : "README.TXT";
		conio.home();
		conio.gotoXY(0,23);
		conio.print("Loading ");
		conio.print(list_file);
		io_buffer = ProDOS.allocIOBuffer();
		ref_num = ProDOS.open(list_file, io_buffer);
		if (ref_num < 0)
		{
			conio.print("Open file error: ");
			conio.println(-ref_num);
			return;
		}
		//
		// Read entire file into line buffer
		//
		last_line   = 0;
		current_pos = 0;
		spin        = 0;
		carriage_return = true;
		do
		{
			bytes_read = ProDOS.read(ref_num, data_buffer);
			if (bytes_read > 0)
			{
				conio.gotoXY(38, 23);
				conio.print(spinner[spin++ % 4]);
				for (int i = 0; i < bytes_read; i++)
				{
					byte b = (byte)(data_buffer[i] & 0x7F);
					if (b == '\n') b = '\r';
					else if (b == 9) b = ' ';
					fill_buffer[current_pos] = b;
					switch (current_pos)
					{
						case 39:
							int c, wrap;
							for (c = 39; c >= 0 && fill_buffer[c] > ' '; c--);
							if (c < 0)
							{
								wrap        = 40;
								c           = 39;
								current_pos = 0;
							}
							else
							{
								wrap = c + 1;
								current_pos = 39 - c;
								while (c >= 0 && fill_buffer[c] <= ' ')
									c--;
								c++;
							}
							if (c > 0)
								line_buffer[last_line] = new String(fill_buffer, 0, c);
							for (c = 0; wrap < 40; fill_buffer[c++] = fill_buffer[wrap++]);
							if (++last_line >= 1000)
								i = bytes_read;
							break;
						case 0:
							if (b <= ' ' && !carriage_return)
								//
								// Disregard wrapped white space
								//
								break;
							if (b == '\r')
							{
								carriage_return = true;
								if (++last_line >= 1000)
									i = bytes_read ;
								break;
							}
							carriage_return = false;
							//
							// Fall thru
							//
						default:
							if (b >= ' ')
								current_pos++;
							else if (b == '\r')
							{
								line_buffer[last_line] = new String(fill_buffer, 0, current_pos);
								current_pos = 0;
								carriage_return = true;
								//
								// Display the file as it gets loaded
								//
								if (++last_line < 24)
									show_page(line_buffer, 0);
								else if (last_line >= 1000)
									i = bytes_read ;
							}
					}
				}
			}
		} while (bytes_read == data_buffer.length && last_line < 1000);
		ProDOS.close(ref_num);
		ProDOS.freeIOBuffer(io_buffer);
		conio.gotoXY(0, 23);
		conio.inverse();
		conio.print("  PRESS ESC TO EXIT, ARROWS TO SCROLL  ");
		conio.normal();
		current_line = 0;
		do
		{
			show_page(line_buffer, current_line);
			key = conio.getKey();
			switch (key)
			{
				case 'A':
				case 'a':
				case 'I':
				case 'i':
				case 0x08:
				case 0x0B:
					if (--current_line < 0)
						current_line = 0;
					break;
				case 'Z':
				case 'z':
				case 'M':
				case 'm':
				case 0x15:
				case 0x0A:
					if (++current_line + 22 > last_line)
					{
						current_line = last_line - 22;
						if (current_line < 0)
							current_line = 0;
					}
					break;
			}
		} while (key != 27); // ESC key
		if (args.length == 0)
			ProDOS.destroy("STARTUP");
	}
}