package java.lang;
import apple2.*;

public class StringBuilder
{
	private short bufLen;
	private byte  buffer[];
	
	public StringBuilder()
	{
		newBuffer(16);
	}
	public StringBuilder(int len)
	{
		newBuffer(len);
	}
	public StringBuilder(String str)
	{
		int bufSize;
		if (str.length() > 240)
			bufSize = 255;
		else
			bufSize = str.length() + 16;
		newBuffer(bufSize);
		append(str);
	}
	private void newBuffer(int size)
	{
		buffer = new byte[size];
		bufLen = 0;	// Set working string length
	}
	private void expandBuffer(int newSize)
	{
		if (newSize > 255)
			newSize = 255;
		if (newSize > buffer.length)
		{
			byte bigBuff[] = new byte[newSize];
			for (int i = buffer.length - 1; i >= 0 ; i--)
				bigBuff[i] = buffer[i];
			buffer = bigBuff;
		}
	}
	public StringBuilder append(boolean b)
	{
		append(b ? " true " : " false ");
		return this;
        
	}
	public StringBuilder append(char c)
	{
		if (buffer.length == bufLen)
			expandBuffer(buffer.length + 16);
		if (bufLen < 255)
			buffer[bufLen++] = (byte)c;
		return this;
	}
	public StringBuilder append(float f)
	{
		return append(Float.toString(f));
	}
	public StringBuilder append(int i)
	{
		return append(Integer.toString(i));
	}
	public StringBuilder append(Object obj)
	{
		// return append(Object.toString(obj));
		return this;
	}
	public StringBuilder append(String str)
	{
		int i;
		if (str != null)
		{
			if (buffer.length < (bufLen + str.length()))
				expandBuffer(buffer.length + str.length() + 16);
			for (i = 0; i < str.length(); i++)
				if (bufLen < 255)
					buffer[bufLen++] = (byte)str.charAt(i);
		}
		return this;
	}
	public int capacity()
	{
		return buffer.length;
	}
	public int length()
	{
		return bufLen;
	}
	public String toString()
	{
		//
		// Create a String object ref from buffer array.
		//
		return new String(buffer, 0, bufLen);
	}
}