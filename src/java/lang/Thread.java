package java.lang;
import apple2.*;

public class Thread implements Runnable
{
	private int tid;
	private String tname;
	private Runnable ttarget;
	public final static int MAX_PRIORITY  = 10;
	public final static int MIN_PRIORITY  = 5;
	public final static int NORM_PRIORITY = 1;
	
	public Thread()
	{
		int tparam;
		ttarget = this;
		tid = vm02.call(vm02.refAsBits(this), 0x12) & 0x00FF0000; // THREAD_NEW
		tparam = (vm02.refAsBits(this) >> 16) & 0xFFFF;
		vm02.call(tid | tparam, 0x2C);	// THREAD_SETCLASS
//		vm02.call(vm02.refAsBits(this), 0x56);	// HMEM_REF_INC

	}
	public Thread(Runnable target){}
	public Thread(Runnable target, String name){}
	public Thread(String name){}
	public Thread(ThreadGroup group, Runnable target){}
	public Thread(ThreadGroup group, Runnable target, String name){}
	public Thread(ThreadGroup group, String name){}
	private Thread(int forcetid)
	{
		int tparam;
		tid = forcetid;
		tparam = vm02.refAsBits(this) & 0xFFFF;
		vm02.call(forcetid | tparam, 0x2E); // THREAD_SETREF
		tparam = (vm02.refAsBits(this) >> 16) & 0xFFFF;
		vm02.call(forcetid | tparam, 0x2C);	// THREAD_SETCLASS
		vm02.call(vm02.refAsBits(this), 0x56);	// HMEM_REF_INC
	}
	public static int activeCount(){return 0;}
	public void checkAccess(){}
	public int countStackFrames(){return 0;}
	public static Thread currentThread()
	{
		Thread curobj;
		int curtid = vm02.call(0, 0x1C) & 0x00FF0000; // THREAD_GET_CURRENT
		int tref = vm02.call(curtid, 0x32) & 0xFFFF;	// THREAD_GETREF
		tref    |= vm02.call(curtid, 0x30) << 16;	// THREAD_GETCLASS
		if (tref == 0)
			curobj = new Thread(curtid);	// Force Thread object for current thread which doesn't have one
		else
			curobj = (Thread)vm02.bitsAsRef(tref);
		return curobj;
	}
	public void destroy(){}
	public static void dumpStack()
	{
		vm02.call(0, 0xE4);	// THREAD_DUMPSTACK
	}
	public static int enumerate(Thread tarray[]){return 0;}
	public final String getName(){return null;}
	public final int getPriority(){return 0;}
	public final ThreadGroup getThreadGroup(){return null;}
	public void interrupt(){}
	public static boolean interrupted(){return false;}
	public final boolean isAlive(){return true;}
	public final boolean isDaemon(){return true;}
	public boolean isInterrupted(){return true;}
	public final void join(){}
	public final void join(long millis){}
	public final void join(long millis, int nanos){}
	public final void resume(){}
	public void run(){}
	public final void setDaemon(boolean on){}
	public final void setName(String name){}
	public final void setPriority(int newPriority)
	{
		int curtid = vm02.call(0, 0x1C) & 0x00FF0000; // THREAD_GET_CURRENT
		vm02.call(curtid | newPriority, 0x28); // THREAD_SETPRIORITY
	}
	public static void sleep(long millis)
	{
		int tparam = (int)millis;
		int curtid = vm02.call(0, 0x1C) & 0x00FF0000; // THREAD_GET_CURRENT
		vm02.call(curtid | (tparam & 0xFFFF), 0x1E);  // THREAD_SETTIMEOUTL
		vm02.call(curtid | (tparam >> 16   ), 0x20);  // THREAD_SETTIMEOUTH
		vm02.call(curtid | 0x03,              0x24);  // THREAD_SETSTATE = S_SLEEP
	}
	public static void sleep(long millis, int nanos){}
	public void start()
	{
		int tparam = vm02.refAsBits(ttarget);
		int pushparamh, pushparaml;
		
 		ttarget = null;	// Unref self
		pushparamh = ((tparam >> 24) & 0x00FF)
		           | ((tparam >> 16) & 0xFF00);
		pushparaml = ((tparam >> 8)  & 0x00FF)
		           | ((tparam << 8)  & 0xFF00);
		vm02.call(tid | pushparamh, 0x14); // THREAD_PUSH
		vm02.call(tid | pushparaml, 0x14); // THREAD_PUSH
		vm02.call(tid | ((tparam >> 16) & 0xFF), 0x16); // THREAD_START
	}
	public final void stop(){}
	public final void stop(Throwable obj){}
	public final void suspend(){}
	public String toString(){return null;}
	public static void yield()
	{
		vm02.call(0, 0x00);	// THREAD_YIELD
	}
}
