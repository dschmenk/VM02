package java.lang;

public class IllegalThreadStateException extends IllegalArgumentException
{
	public IllegalThreadStateException(){}
	public IllegalThreadStateException(String message)
	{
		super(message);
	}
}