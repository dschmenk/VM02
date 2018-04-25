package java.net;
/*
 * This class implements the loopback device.
 */

public class Loopback extends NetworkDevice
{
	static private final int IDLE_OP  = 0;
	static private final int WRITE_OP = 1;
	static private final int READ_OP  = 2;
	static private int readOffset, writeOffset, xferOp, packetLen;
	static private Thread readThread;
	static private byte[] packet;

	public static boolean enable()
	{
		xferOp      = IDLE_OP;
		readOffset  = 0;
		writeOffset = 0;
		packetLen   = 0;
		packet      = null;
                return true;
	}
	public static void disable()
	{
		packet = null;
		if (readThread != null)
			readThread.notify();
	}
	public static int minDataSize()
	{
		return 0;
	}
	public static int maxDataSize()
	{
		return 2000;
	}
        public static byte[] localAddr()
        {
		return null;
        }
	public static byte[] broadcastAddr()
	{
		return null;
	}
        public static byte[] newAddr()
        {
		return null;
        }
        public static boolean isEqualAddr(byte[] addr1, byte[] addr2)
        {
		return true;
        }
        public static void copyAddr(byte[] src, byte[] dst)
        {
        }
        public static byte[] newHeader()
        {
		return new byte[2];
        }
        public static void setHeaderSrcAddr(byte[] header, byte[] addr)
        {
        }
        public static void setHeaderDstAddr(byte[] header, byte[] addr)
        {
        }
        public static void copyHeaderSrcAddr(byte[] header, byte[] addr)
        {
        }
        public static void copyHeaderDstAddr(byte[] header, byte[] addr)
        {
        }
        public static byte[] getHeaderSrcAddr(byte[] header)
        {
		return null;
        }
        public static byte[] getHeaderDstAddr(byte[] header)
        {
		return null;
        }
        public static int getHeaderType(byte[] header)
        {
		return ((char)header[0] << 8) | (char)header[1];
        }
        public static void setHeaderType(byte[] header, int type)
        {
		header[0] = (byte)(type >> 8);
		header[1] = (byte)type;
        }
	public static boolean sendHeader(byte[] header, int dataSize)
	{
		while (xferOp != 0)
			Thread.sleep(20);
		packetLen   = dataSize + 2;
		packet      = new byte[packetLen];
		packet[0]   = header[0];
		packet[1]   = header[1];
		writeOffset = 2;
		xferOp      = WRITE_OP;
		return true;
	}
	public static boolean sendHeader(byte[] dst, int type, int dataSize)
	{
		byte[] header = new byte[2];
		header[0] = (byte)(type >> 8);
		header[1] = (byte)type;
		return sendHeader(header, dataSize);
	}
	public static void sendData(byte[] data, int offset, int size)
	{
		int len = offset + size;
		for (int i = offset; i < len; i++)
			packet[writeOffset++] = data[i];
	}
	public static void sendData(byte[] data)
	{
		for (int i = 0; i < data.length; i++)
			packet[writeOffset++] = data[i];
	}
	public static int recvHeader(byte[] header)
	{
		if ((packet == null || packetLen == 0) || (xferOp != 0))
		{
			readThread = Thread.currentThread();
			try
			{
				readThread.wait();
			}
			catch (Exception e)
			{

			}
			readThread = null;
		}
		if (packet != null && packetLen > 2)
		{
			header[0]  = packet[0];
			header[1]  = packet[1];
			readOffset = 2;
			xferOp     = READ_OP;
			return packetLen - 2;
		}
		return 0;
	}
	public static void recvData(byte[] data, int offset, int size)
	{
		int len = offset + size;
		for (int i = offset; i < len; i++)
			data[i] = packet[readOffset++];
	}
	public static void recvData(byte[] data)
	{
		for (int i = 0; i < data.length; i++)
			data[i] = packet[readOffset++];
	}
	public static void xferComplete()
	{
		switch (xferOp)
		{
			case 1: // send
				xferOp = IDLE_OP;
				if (readThread != null)
					readThread.notify();
				break;
			case 2: // recv
				xferOp    = IDLE_OP;
				packet    = null;
				packetLen = 0;
		}
	}
}