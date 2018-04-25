import apple2.*;

public class NextChain
{
        public static void main(String args[])
        {
        	System.out.println("Passed arguments:");
        	for (int i = 0; i < args.length; i++)
	        	System.out.println(args[i]);
        	System.out.print("Exit status:");
	System.out.println(vm02.peekWord(0x03E8));	// EXIT_STATUS
        }
}
