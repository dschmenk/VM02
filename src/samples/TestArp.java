import apple2.*;

public class TestArp extends Thread
{
	private static final int PRTYPE_IP            = 0x0800;
	private static final int PRTYPE_ARP           = 0x0806;
	private static final int PRTYPE_RARP          = 0x8035;
	private static final int ARP_HW_ETH           = 1; // ethernet hardware type code
	private static final int ARP_REQUEST          = 1;
	private static final int ARP_REPLY            = 2;
	private static final int ARP_HEADER_SIZE      = 8;
	private static final int ARP_HW_TYPE_OFS      = 0; // LEN = 2
	private static final int ARP_PR_TYPE_OFS      = 2; // LEN = 2
	private static final int ARP_HW_LEN_OFS       = 4; // LEN = 1
	private static final int ARP_PR_LEN_OFS       = 5; // LEN = 1
	private static final int ARP_OP_OFS           = 6; // LEN = 2
	private static final int ARP_SEND_HW_ADDR_OFS = 8; // LEN = ?
	private static       int ARP_SEND_PR_ADDR_OFS;     // LEN = ?
	private static       int ARP_TARG_HW_ADDR_OFS;     // LEN = ?
	private static       int ARP_TARG_PR_ADDR_OFS;     // LEN = ?
	private static       int ARP_PACKET_SIZE;
	private static    byte[] ipLocal  = {(byte)192,(byte)168,(byte)123,(byte)10};
	private static    byte[] ipServer = {(byte)192,(byte)168,(byte)123,(byte)1};
	private static   boolean done = false;

	public TestArp()
	{
	}
	public static void setDataField(byte[] packet, int offset, byte[] data)
	{
		for (int i = 0; i < data.length; i++)
			packet[offset + i] = data[i];
	}
	public static boolean keyPressed()
	{
		return ((vm02.call((5 << 19), 0x96) & 0xFF) > 0);
	}
	public void run()
	{
		byte[] arp = new byte[ARP_PACKET_SIZE];
		//
		// Fill in ARP packet
		//
		arp[ARP_HW_TYPE_OFS]     = (byte)(ARP_HW_ETH >> 8);
		arp[ARP_HW_TYPE_OFS + 1] = (byte) ARP_HW_ETH;
		arp[ARP_PR_TYPE_OFS]     = (byte)(PRTYPE_IP >> 8);
		arp[ARP_PR_TYPE_OFS + 1] = (byte) PRTYPE_IP;
		arp[ARP_HW_LEN_OFS]      = (byte) Ethernet.localAddr().length;
		arp[ARP_PR_LEN_OFS]      = (byte) ipLocal.length;
		arp[ARP_OP_OFS]          = (byte)(ARP_REQUEST >> 8);
		arp[ARP_OP_OFS + 1]      = (byte) ARP_REQUEST;
		setDataField(arp, ARP_SEND_HW_ADDR_OFS, Ethernet.localAddr());
		setDataField(arp, ARP_SEND_PR_ADDR_OFS, ipLocal);
		setDataField(arp, ARP_TARG_PR_ADDR_OFS, ipServer);
		while (!done)
		{
			System.out.println("Sending ARP ethernet header");
			System.out.print("Sending ARP for address:");
			System.out.print(ipServer[0] & 0xFF);
			System.out.print(".");
			System.out.print(ipServer[1] & 0xFF);
			System.out.print(".");
			System.out.print(ipServer[2] & 0xFF);
			System.out.print(".");
			System.out.println(ipServer[3] & 0xFF);
			if (!Ethernet.sendHeader(Ethernet.broadcastAddr(), PRTYPE_ARP, ARP_PACKET_SIZE))
			{
				System.out.println("Error sending header!");
	//			System.exit(-1);
			}
			else
			{
				System.out.println("Sending ARP packet");
				Ethernet.sendData(arp);
				Ethernet.xferComplete();
				System.out.println("Waiting for reply packet...");
			}
			sleep(10000); // sleep for 10 seconds
		}
	}
	public static void main(String[] args)
	{
		int len;
		byte[] header;
		byte[] arp;
		
		System.out.print('.');
		if (!Ethernet.enable())
		{
			System.out.println("Unable to open ethernet device");
			System.exit(-1);
		}
		System.out.print('.');
		//
		// Calculate offsets for remaining fields
		//
		ARP_SEND_PR_ADDR_OFS = ARP_SEND_HW_ADDR_OFS + Ethernet.localAddr().length;
		ARP_TARG_HW_ADDR_OFS = ARP_SEND_PR_ADDR_OFS + ipLocal.length;
		ARP_TARG_PR_ADDR_OFS = ARP_TARG_HW_ADDR_OFS + Ethernet.localAddr().length;
		ARP_PACKET_SIZE      = ARP_TARG_PR_ADDR_OFS + ipLocal.length;
		System.out.print('.');
		header = Ethernet.newHeader();
		System.out.print('.');
		arp = new byte[ARP_PACKET_SIZE];
		System.out.print('.');
		TestArp arpSend = new TestArp();
		System.out.print('.');
		Thread.currentThread().setPriority(6);
		System.out.print('.');
		arpSend.start(); // Start ARP send thread
		System.out.println('!');
		while (!keyPressed())
		{
			if ((len = Ethernet.recvHeader(header)) > 0)
			{
				System.out.print("Received packet of len:");
				System.out.println(len + header.length);
				if (Ethernet.getHeaderType(header) == PRTYPE_ARP)
				{
					if (len > ARP_PACKET_SIZE) len = ARP_PACKET_SIZE;
					Ethernet.recvData(arp, 0, len);
					System.out.print(" ARP packet type = 0x");
					System.out.println(Integer.toHexString(((arp[ARP_OP_OFS] & 0xFF) << 8) | (arp[ARP_OP_OFS + 1] & 0xFF)));
					System.out.print(" from:");
					System.out.print(arp[ARP_SEND_PR_ADDR_OFS] & 0xFF);
					System.out.print(".");
					System.out.print(arp[ARP_SEND_PR_ADDR_OFS+1] & 0xFF);
					System.out.print(".");
					System.out.print(arp[ARP_SEND_PR_ADDR_OFS+2] & 0xFF);
					System.out.print(".");
					System.out.println(arp[ARP_SEND_PR_ADDR_OFS+3] & 0xFF);
					System.out.print(" Hardware address:");
					System.out.print(Integer.toHexString(arp[ARP_SEND_HW_ADDR_OFS] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_SEND_HW_ADDR_OFS+1] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_SEND_HW_ADDR_OFS+2] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_SEND_HW_ADDR_OFS+3] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_SEND_HW_ADDR_OFS+4] & 0xFF));
					System.out.print(":");
					System.out.println(Integer.toHexString(arp[ARP_SEND_HW_ADDR_OFS+5] & 0xFF));
					System.out.print(" to:");
					System.out.print(arp[ARP_TARG_PR_ADDR_OFS] & 0xFF);
					System.out.print(".");
					System.out.print(arp[ARP_TARG_PR_ADDR_OFS+1] & 0xFF);
					System.out.print(".");
					System.out.print(arp[ARP_TARG_PR_ADDR_OFS+2] & 0xFF);
					System.out.print(".");
					System.out.println(arp[ARP_TARG_PR_ADDR_OFS+3] & 0xFF);
					System.out.print(" Hardware address:");
					System.out.print(Integer.toHexString(arp[ARP_TARG_HW_ADDR_OFS] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_TARG_HW_ADDR_OFS+1] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_TARG_HW_ADDR_OFS+2] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_TARG_HW_ADDR_OFS+3] & 0xFF));
					System.out.print(":");
					System.out.print(Integer.toHexString(arp[ARP_TARG_HW_ADDR_OFS+4] & 0xFF));
					System.out.print(":");
					System.out.println(Integer.toHexString(arp[ARP_TARG_HW_ADDR_OFS+5] & 0xFF));
				}
				else
				{
					System.out.print(" Non-ARP packet type = 0x");
					System.out.println(Integer.toHexString(Ethernet.getHeaderType(header)));
				}
			}
			else
				System.out.println("Received bad packet");
			Ethernet.xferComplete();
		}
		System.out.println("Exiting...");
		done = true;
		sleep(10500);
	}
}
