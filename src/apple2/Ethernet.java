package apple2;
/*
 * This class interfaces to the ethernet device.
 */
public class Ethernet extends java.net.NetworkDevice
{
	static private int  ethSlot, ethOutputBuff, ethInputBuff, ethRecvPacket, ethSendPacket, ethHeaderLen, ethPacketDone, ethCtrl, ethWrite, ethRead;
	static private byte[] MAC = {(byte)0x00,(byte)0x0a,(byte)0x99,(byte)0x1e,(byte)0x02,(byte)0x00};
	static final int MIN_DATA_SIZE = 46;
	static final int MAX_DATA_SIZE = 1500;

	private static void copyBytes(byte[] src, int srcOfst, byte[] dst, int dstOfst, int count)
	{
		while (count-- != 0)
			dst[dstOfst++] = src[srcOfst++];
	}
	public static boolean enable()
	{
		//
		// Search for ethernet card.
		//
		for (int slot = 1; slot < 8; slot++)
		{
			//
			// ID device
			//
			if (((vm02.call((1 << 19), 0x90 + (slot << 1))) & 0x010000FF) == 0xE1) // CARRY clear == valid device IOCTL, 0xE1 == ethernet card ID
			{
				ethCtrl  = 0x90 + (slot << 1);
				ethWrite = 0x80 + (slot << 1);
				ethRead  = 0x70 + (slot << 1);
				ethSlot  = slot << 16;
				ethInputBuff  = ethSlot | ( 7 << 19);
				ethOutputBuff = ethSlot | ( 9 << 19);
				ethRecvPacket = ethSlot | (16 << 19);
				ethSendPacket = ethSlot | (17 << 19);
				ethPacketDone = ethSlot | (18 << 19);
				ethHeaderLen  = ethSlot | 14;
				for (int oct = 0; oct < 3; oct++)
					vm02.call(ethSlot
					| ((19 + oct) << 19)
					| ( MAC[oct*2]           & 0x00FF)
					| ((MAC[oct*2 + 1] << 8) & 0xFF00), ethCtrl); // set MAC address
				if ((vm02.call(ethSlot | (3 << 19), ethCtrl) & 0x01000000) == 0) // open port
					return true;
			}
		}
		return false;
	}
	public static void disable()
	{
		vm02.call(ethSlot | 4, ethCtrl); // close port
	}
	public static byte[] localAddr()
	{
		return MAC;
	}
	public static byte[] broadcastAddr()
	{
		byte[] addr = new byte[6];
		addr[0] = (byte)0xFF;
		addr[1] = (byte)0xFF;
		addr[2] = (byte)0xFF;
		addr[3] = (byte)0xFF;
		addr[4] = (byte)0xFF;
		addr[5] = (byte)0xFF;
		return addr;
	}
	public static byte[] newAddr()
	{
		return new byte[6];
	}
	public static boolean isEqualAddr(byte[] addr1, byte[] addr2)
	{
		return ((addr1[5] == addr2[5])
		     && (addr1[4] == addr2[4])
		     && (addr1[3] == addr2[3])
		     && (addr1[2] == addr2[2])
		     && (addr1[1] == addr2[1])
		     && (addr1[0] == addr2[0]));
	}
	public static void copyAddr(byte[] src, byte[] dst)
	{
		copyBytes(src, 0, dst, 0, 6);
	}
	public static byte[] newHeader()
	{
		return new byte[14];
	}
	public static void setHeaderSrcAddr(byte[] header, byte[] addr)
	{
		copyBytes(addr, 0, header, 6, 6);
	}
	public static void setHeaderDstAddr(byte[] header, byte[] addr)
	{
		copyBytes(addr, 0, header, 0, 6);
	}
	public static void copyHeaderSrcAddr(byte[] header, byte[] addr)
	{
		copyBytes(header, 6, addr, 0, 6);
	}
	public static void copyHeaderDstAddr(byte[] header, byte[] addr)
	{
		copyBytes(header, 0, addr, 0, 6);
	}
	public static byte[] getHeaderSrcAddr(byte[] header)
	{
		byte[] addr = new byte[6];
		copyBytes(header, 6, addr, 0, 6);
		return addr;
	}
	public static byte[] getHeaderDstAddr(byte[] header)
	{
		byte[] addr = new byte[6];
		copyBytes(header, 0, addr, 0, 6);
		return addr;
	}
	public static int getHeaderType(byte[] header)
	{
		if (header == null || header.length < 14)
		{
			System.out.println("Bad header in getHeaderType!");
			return 0;
		}
		return ((header[12] << 8) & 0xFF00) | (header[13] & 0x00FF);
	}
	public static void setHeaderType(byte[] header, int type)
	{
		header[12] = (byte)(type >> 8);
		header[13] = (byte)type;
	}
	public static boolean sendHeader(byte[] header, int dataSize)
	{
		boolean success;
		int hmem = vm02.refAsBits((Object)header) & 0xFFFF;
		vm02.call(ethOutputBuff | (vm02.call(hmem, 0x0E) & 0xFFFF) + 2, ethCtrl); // HMEM_LOCK header IOCTL_OUTBUFF
		success = (vm02.call(ethSendPacket | dataSize + 14, ethCtrl) & 0x01000000) == 0;
		if (success) // prepare to write packet of length len
			vm02.call(ethHeaderLen, ethWrite); // write 14 byte ethernet header
		vm02.call(hmem, 0x10); // HMEM_UNLOCK header
		return success;
	}
	public static boolean sendHeader(byte[] dst, int type, int dataSize)
	{
		byte[] header = new byte[14];
		copyBytes(dst, 0, header, 0, 6);
		copyBytes(MAC, 0, header, 6, 6);
		header[12] = (byte)(type >> 8);
		header[13] = (byte)type;
		return sendHeader(header, dataSize);
	}
	public static void sendData(byte[] data, int offset, int size)
	{
		int hmem = vm02.refAsBits((Object)data) & 0xFFFF;
		vm02.call(ethOutputBuff | (vm02.call(hmem, 0x0E) & 0xFFFF) + 2 + offset, ethCtrl); // HMEM_LOCK data IOCTL_OUTBUFF
		vm02.call(ethSlot | size,                                                ethWrite); // write data
		vm02.call(hmem,                                                          0x10); // HMEM_UNLOCK data
	}
	public static void sendData(byte[] data)
	{
		int hmem = vm02.refAsBits((Object)data) & 0xFFFF;
		vm02.call(ethOutputBuff | (vm02.call(hmem, 0x0E) & 0xFFFF) + 2, ethCtrl); // HMEM_LOCK data IOCTL_OUTBUFF
		vm02.call(ethSlot | data.length,                                ethWrite); // write data
		vm02.call(hmem,                                                 0x10); // HMEM_UNLOCK data
	}
	public static int recvHeader(byte[] header)
	{
		int len;

		if (header == null || header.length < 14)
		{
			System.out.println("Bad header in recvHeader!");
			return 0;
		}
		int hmem = vm02.refAsBits((Object)header) & 0xFFFF;
		vm02.call(ethInputBuff | (vm02.call(hmem, 0x0E) & 0xFFFF) + 2, ethCtrl); // HMEM_LOCK header IOCTL_INBUFF
		if ((len = vm02.call(ethRecvPacket, ethCtrl) & 0xFFFF) >= 14) //  ETHRCTRL_RECVPKT
		{
			vm02.call(ethHeaderLen, ethRead); // read 14 byte ethernet header
			vm02.call(hmem,         0x10); // HMEM_UNLOCK header
			return len - 14;
		}
		vm02.call(hmem, 0x10); // HMEM_UNLOCK header
		return 0;
	}
	public static void recvData(byte[] data, int offset, int size)
	{
		int hmem = vm02.refAsBits((Object)data) & 0xFFFF;
		vm02.call(ethInputBuff | (vm02.call(hmem, 0x0E) & 0xFFFF) + 2 + offset, ethCtrl); // HMEM_LOCK data IOCTL_OUTBUFF
		vm02.call(ethSlot | size,                                               ethRead); // write data
		vm02.call(hmem,                                                         0x10); // HMEM_UNLOCK data
	}
	public static void recvData(byte[] data)
	{
		int hmem = vm02.refAsBits((Object)data) & 0xFFFF;
		vm02.call(ethInputBuff | (vm02.call(hmem, 0x0E) & 0xFFFF) + 2, ethCtrl); // HMEM_LOCK data IOCTL_OUTBUFF
		vm02.call(ethSlot | data.length,                               ethRead); // write data
		vm02.call(hmem,                                                0x10); // HMEM_UNLOCK data
	}
	public static void xferComplete()
	{
		vm02.call(ethPacketDone, ethCtrl); // ETHRCRL_DONEPKT
	}
}