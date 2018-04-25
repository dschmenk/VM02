package java.io;

public class FileNotFoundException extends IOException
{
    public FileNotFoundException(){}
    public FileNotFoundException(String message)
    {
        super(message);
    }
}