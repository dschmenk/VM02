//
//  Catalog.java
//  
//
//  Created by Astro on 9/26/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//
import apple2.*;
import java.io.*;

public class Catalog
{
	public static void main(String args[]) throws IOException
	{
		int i, io_buffer, ref_num, bytes_read;
		int first_block, entry_length, entries_block, file_count, entry_offset;
		String pfx;
		byte data_buffer[] = new byte[512];
        
		if (args.length > 0)
		{
			if (ProDOS.setPrefix(args[0]) < 0)
			{
				System.err.println("Bad prefix: " + args[0]);
				return;
			}
		}
		pfx = ProDOS.getPrefix();
		System.out.println(pfx);
		io_buffer = ProDOS.allocIOBuffer();
		ref_num = ProDOS.open(pfx, io_buffer);
		if (ref_num < 0)
		{
			System.err.println("Open directory error: " + ref_num);
			return;
		}
		first_block   = 1;
		entry_length  = 0x26;
		entries_block = 14;
		file_count    = 0;
		do
		{
			bytes_read = ProDOS.read(ref_num, data_buffer);
			if (bytes_read == 512)
			{
				if (first_block == 1)
				{
					entry_length  =   (int)data_buffer[0x23] & 0xFF;
					entries_block =   (int)data_buffer[0x24] & 0xFF;
					file_count    =  ((int)data_buffer[0x25] & 0xFF)
						| (((int)data_buffer[0x26] & 0xFF) << 8);
				}
				for (i = first_block; i < entries_block; i++)
				{
					entry_offset = i * entry_length + 4;
					if (data_buffer[entry_offset] != 0)
					{
						int len = data_buffer[entry_offset] & 0x0F;
						System.out.write(data_buffer, entry_offset + 1, len);
						if ((data_buffer[entry_offset] & 0xF0) == 0xD0) // Is it a directory?
						{
							System.out.print('/');
							len++;
						}
						while (len++ < 40)
							System.out.print(' ');
						file_count--;
					}
				}
				first_block = 0;
			}
			else
				file_count = 0;
		} while (file_count > 0);
		ProDOS.close(ref_num);
		ProDOS.freeIOBuffer(io_buffer);
		System.out.print("\nPress RETURN key...");
		System.in.read();
	}
}
