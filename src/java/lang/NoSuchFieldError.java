package java.lang;

public class NoSuchFieldError extends IncompatibleClassChangeError
{
	public NoSuchFieldError(){}
	public NoSuchFieldError(String message)
	{
		super(message);
	}
}