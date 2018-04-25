package java.lang;

public class InternalError extends VirtualMachineError
{
	public InternalError(){}
	public InternalError(String message)
	{
		super(message);
	}
}