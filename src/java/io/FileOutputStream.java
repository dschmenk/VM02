package java.io;
import apple2.*;

public class FileOutputStream extends OutputStream
{
    byte[] datumBuffer;
    int ioBuffer = 0;
    int refNum   = 0;
    
    public FileOutputStream(String name) throws FileNotFoundException, IOException
    {
        open(name, false);
    }
    public FileOutputStream(String name, boolean append) throws FileNotFoundException, IOException
    {
        open(name, append);
    }
    private void open(String name, boolean append) throws FileNotFoundException, IOException
    {
        byte[] fileInfo = ProDOS.getFileInfo(name);
        if (fileInfo[0] == 0)
        {
            if (fileInfo[7] > 0x03)
                //
                // Can't write to a directory
                //
                throw new FileNotFoundException();
            if ((fileInfo[3] & 0x02) == 0x00)
                //
                // Write disabled
                //
                throw new IOException();
        }
        else
        {
            if (fileInfo[0] == 0x46)
                //
                // File not found, so create it.
                //
                if (ProDOS.create(name, 0xC3, 0x06, 0x0000, 0x01) != 0)
                    throw new IOException();
            else
                throw new IOException();
        }
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
        if (append)
            ProDOS.setMark(refNum, ProDOS.getEOF(refNum));
        else
            ProDOS.setEOF(refNum, 0);
    }
    protected void finalize()
    {
        if (refNum != 0)
            close();
    }
    public void close()
    {
        if (ioBuffer != 0)
        {
            if (refNum != 0)
            {
                //
                // Truncate file at current position
                //
                ProDOS.setEOF(refNum, ProDOS.getMark(refNum));
                ProDOS.close(refNum);
	refNum = 0;
            }
            ProDOS.freeIOBuffer(ioBuffer);
            ioBuffer = 0;
        }
        datumBuffer = null;
    }
    public void write(int b) throws IOException
    {
        datumBuffer[0] = (byte)b;
        if (ProDOS.write(refNum, datumBuffer, 0, 1) < 0)
            throw new IOException();
    }
    public void write(byte dataBuffer[]) throws IOException
    {
        ProDOS.write(refNum, dataBuffer, 0, dataBuffer.length);
    }
    public void write(byte dataBuffer[], int off, int len) throws IOException
    {
        ProDOS.write(refNum, dataBuffer, off, len);
    }
}