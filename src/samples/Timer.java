public class Timer {
	public static void main(String args[])
	{
		int i;
		while (System.in.available() == 0)
		{
			i = (int)System.currentMillis();
			System.out.println(i);
			Thread.currentThread().yield();
		}
	}
}