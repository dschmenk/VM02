package java.lang;

public class OutOfMemoryError extends VirtualMachineError
{
	public OutOfMemoryError(){}
	public OutOfMemoryError(String message)
	{
		super(message);
	}
}