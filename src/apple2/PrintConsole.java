package apple2;

public class PrintConsole extends java.io.PrintStream {

	public PrintConsole()
	{
		super(new OutputConsole());
		vm02.call(20 << 19, 0x96); // CONCTL_TEXT80 on slot #3
	}
	
	public void print(String s)
	{
		vm02.call(vm02.refAsBits(s), 0x50);
	}
	public void println(String s)
	{
		vm02.call(vm02.refAsBits(s), 0x52);
	}
}