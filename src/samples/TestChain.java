import apple2.*;

public class TestChain
{
        public static void main(String args[])
        {
        	String ChainCmd = "samples/NextChain Hello";
        	System.out.println("Test chain command..");
        	System.out.print("Sending chain: ");
        	System.out.println(ChainCmd);
	int stringptr = vm02.call(vm02.refAsBits((Object)ChainCmd), 0x0E) & 0xFFFF;	// HMEM_LOCK
	vm02.call(stringptr+1, 0x3E);	// MEMSRC
	vm02.call(0x0200, 0x40);	// MEMDST
	vm02.call(ChainCmd.length(), 0x42);	// MEMCPY
	vm02.call(vm02.refAsBits((Object)ChainCmd), 0x10);	// HMEM_UNLOCK
	vm02.pokeByte(0x03EC, (byte)ChainCmd.length());	// CHAIN_CMD
	System.exit(1234);
        }
}
