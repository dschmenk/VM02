package apple2;

public class SystemException
{
	private static void throwSystemException(int exception) throws Exception
	{
		switch (exception)
		{
			case 1:  throw new Error();
			case 2:  throw new ThreadDeath();
			case 3:  throw new InternalError();
			case 4:  throw new OutOfMemoryError();
			case 5:  throw new StackOverflowError();
			case 6:  throw new NoClassDefFoundError();
			case 7:  throw new ClassFormatError();
			case 8:  throw new IncompatibleClassChangeError();
			case 9:  throw new NoSuchMethodError();
			case 10: throw new NoSuchFieldError();
			case 11: throw new Exception();
			case 12: throw new IllegalMonitorStateException();
			case 13: throw new IllegalThreadStateException();
			case 14: throw new InterruptedException();
			case 15: throw new ClassNotFoundException();
			case 16: throw new RuntimeException();
			case 17: throw new NullPointerException();
			case 18: throw new IndexOutOfBoundsException();
			case 19: throw new ArrayIndexOutOfBoundsException();
			case 20: throw new StringIndexOutOfBoundsException();
			case 21: throw new NegativeArraySizeException();
			case 22: throw new ArrayStoreException();
			case 23: throw new ArithmeticException();
			case 24: throw new ClassCastException();
		}
	}
}