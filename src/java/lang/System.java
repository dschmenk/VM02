package java.lang;
import apple2.*;

public final class System extends Object {
	public static java.io.PrintStream err = new apple2.PrintConsole();
	public static java.io.InputStream in  = new apple2.InputConsole();
	public static java.io.PrintStream out = new apple2.PrintConsole();
	
	public static void arraycopy(Object src, int srcpos,
	                             Object dst, int dstpos,
	                             int len){};
	public static long currentTimeMillis()
	{
		int msh, msl;
		do
		{
			msh = vm02.call(0, 0x3C) << 16;	// THREAD_GETTICH
			msl = vm02.call(0, 0x3A) & 0xFFFF;	// THREAD_GETTICL
		} while (msh != (vm02.call(0, 0x3C) << 16));		// THREAD_GETTICH
		return (msl | msh);
	}
	public static void exit(int status)
	{
		vm02.call(status, 0xEE);
	}
	public static void gc()
	{
		vm02.call(256, 0x64);
		vm02.call(0, 0x00);	// THREAD_YIELD
	}
	public static void load(String filename)
	{
		vm02.call(vm02.refAsBits(filename), 0x62);	
	}
	public static void runFinalization()
	{
		gc();
	}
}