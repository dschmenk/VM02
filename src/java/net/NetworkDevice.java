package java.net;
/*
 * This class is the base class for network devices.
 */
public class NetworkDevice
{
	public static boolean enable()
	{
                return false;
	}
	public static void disable()
	{
	}
	public static int minDataSize()
	{
		return 0;
	}
	public static int maxDataSize()
	{
		return 0;
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
		return false;
        }
        public static void copyAddr(byte[] src, byte[] dst)
        {
        }
        public static byte[] newHeader()
        {
		return null;
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
		return 0;
        }
        public static void setHeaderType(byte[] header, int type)
        {
        }
	public static boolean sendHeader(byte[] header, int dataSize)
	{
		return true;
	}
	public static boolean sendHeader(byte[] dst, int type, int dataSize)
	{
		return true;
	}
	public static void sendData(byte[] data, int offset, int size)
	{
	}
	public static void sendData(byte[] data)
	{
	}
	public static int recvHeader(byte[] header)
	{
		return 0;
	}
	public static void recvData(byte[] data, int offset, int size)
	{
	}
	public static void recvData(byte[] data)
	{
	}
	public static void xferComplete()
	{
	}
}