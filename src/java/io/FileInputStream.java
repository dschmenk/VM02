package java.io;
import apple2.*;

public class FileInputStream extends InputStream
{
    byte[] datumBuffer;
    int ioBuffer = 0;
    int refNum   = 0;
    
    public FileInputStream(String name) throws FileNotFoundException, IOException
    {
        ioBuffer = ProDOS.allocIOBuffer();
        if (ioBuffer == 0)
            throw new IOException();
        refNum = ProDOS.open(name, ioBuffer);
        if (refNum <= 0)
        {
            ProDOS.freeIOBuffer(ioBuffer);
            ioBuffer = 0;
            refNum   = 0;
            throw new FileNotFoundException();
        }
        datumBuffer = new byte[1];
    }
    
    public void close()
    {
        if (ioBuffer != 0)
        {
            if (refNum != 0)
            {
                ProDOS.close(refNum);
                refNum = 0;
            }
            ProDOS.freeIOBuffer(ioBuffer);
            ioBuffer = 0;
        }
        datumBuffer = null;
    }
    public int read() throws IOException
    {
        if (ProDOS.read(refNum, datumBuffer, 0, 1) < 0)
            throw new IOException();
        return datumBuffer[0];
    }
    public int read(byte dataBuffer[]) throws IOException
    {
        return ProDOS.read(refNum, dataBuffer, 0, dataBuffer.length);
    }
    public int read(byte dataBuffer[], int off, int len) throws IOException
    {
        if (dataBuffer == null)
            return (int)skip(len);
        else
            return ProDOS.read(refNum, dataBuffer, off, len);
    }
}