import apple2.*;
import java.io.*;

public class P8 {
	public static void main(String args[]) throws IOException
	{
		int i, io_buffer, ref_num, bytes_read;
		int first_block, entry_length, entries_block, file_count, entry_offset;
		String pfx, vols[], dump_file;
		byte data_buffer[] = new byte[512];
	
		System.out.println("Online Volumes:");
		vols = ProDOS.online();
		for (i = 0; i < vols.length; i++)
			System.out.println(vols[i]);
		pfx = ProDOS.getPrefix();
		System.out.print('\n');
		System.out.print(pfx);
		System.out.println(':');
		io_buffer = ProDOS.allocIOBuffer();
		ref_num = ProDOS.open(pfx, io_buffer);
		if (ref_num < 0)
		{
			System.out.print("Open dir error: ");
			System.out.println(-ref_num);
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
						System.out.write(data_buffer, entry_offset + 1, data_buffer[entry_offset] & 0x0F);
						if ((data_buffer[entry_offset] & 0xF0) == 0xD0) // Is it a directory?
							System.out.println('/');
						else
							System.out.print('\n');
						file_count--;
					}
				}
				first_block = 0;
			}
			else
				file_count = 0;
		} while (file_count > 0);
		ProDOS.close(ref_num);
		dump_file = "P8.java";
		if (args.length == 1)
			dump_file = args[0];
		System.out.print('\n');
		System.out.print(pfx);
		System.out.println(dump_file);
		System.out.print('\n');
		ref_num = ProDOS.open(dump_file, io_buffer);
		if (ref_num < 0)
		{
			System.out.print("Open file error: ");
			System.out.println(-ref_num);
			return;
		}
		do
		{
			bytes_read = ProDOS.read(ref_num, data_buffer);
			if (bytes_read > 0)
				System.out.write(data_buffer, 0, bytes_read);
		} while (bytes_read == data_buffer.length);
		ProDOS.close(ref_num);
		ProDOS.freeIOBuffer(io_buffer);
	}
}
