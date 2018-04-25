package java.lang;

import apple2.*;

public class Throwable
{
	private String msg;
	
	public Throwable(){}
	public Throwable(String message)
	{
		msg = message;
	}

	public String getMessage()
	{
		return msg;
	}
	public void printStackTrace()
	{
		vm02.call(0, 0xEC);	// THREAD_DUMPSTACK
	}
}