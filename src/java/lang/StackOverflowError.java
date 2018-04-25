package java.lang;

public class StackOverflowError extends VirtualMachineError
{
	public StackOverflowError(){}
	public StackOverflowError(String message)
	{
		super(message);
	}
}