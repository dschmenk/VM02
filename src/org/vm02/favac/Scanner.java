//
//  SLACKER - Simple Lexical Analyzer with Constant and Keyword EvaluatoR
//
package org.vm02.favac;
import java.io.*;

public class Scanner 
{
    public final static int TOK_ID          = 1;
    public final static int TOK_HEXCONST    = 2;
    public final static int TOK_INTCONST    = 3;
    public final static int TOK_FLOATCONST  = 4;
    public final static int TOK_CHARCONST   = 5;
    public final static int TOK_STRINGCONST = 6;
    public final static int TOK_COMMENT     = 14;
    public final static int TOK_SCANNING    = 15;
    public final static int TOK_KEYWORD     = 16;
    public final static int TOK_TOKEN       = 128;
    public final static int TOK_ENDOFINPUT  = 0;
    public final static int TOK_ERR         = -1;
    public final static int TOK_HEXERR      = -2;
    public final static int TOK_INTERR      = -3;
    public final static int TOK_FLOATERR    = -4;
    public final static int TOK_CHARERR     = -5;
    public final static int TOK_STRINGERR   = -6;
    public final static int TOK_COMMENTERR  = -7;
    private final static float[] twoPow10   = {1E+1F,1E+2F,1E+4F,1E+8F,1E+16F,1E+32F,0F,0F};   
    private boolean ignoreCase, inComment;
    private String keywords[], tokens[];
    private byte inputBlock[];
    private int nextChar;
    private InputStream inputFile;
    byte   inputLine[];
    int    inputLen, matchPos;
    int    intValue;
    float  floatValue;
    String stringValue;
    
    /*
     * Create new inpt scanner of tokens and optional keywords.
     */
    public Scanner(InputStream in, String[] tok, String[] kw, boolean kwCase)
    {
        tokens     = tok;
        keywords   = kw;
        ignoreCase = kwCase;
        inputFile  = in;
        inputLine  = new byte[256];
        inputBlock = new byte[512];
        nextChar   = inputBlock.length;
		matchPos   = 0;
		inputLen   = 0;
		inComment  = false;
    }
    public Scanner(InputStream in, String[] tok)
    {
        tokens     = tok;
        keywords   = null;
        ignoreCase = false;
        inputFile  = in;
        inputLine  = new byte[256];
        inputBlock = new byte[512];
        nextChar   = inputBlock.length;
		matchPos   = 0;
		inputLen   = 0;
		inComment  = false;
    }
    public Scanner(byte[] input, String[] tok, String[] kw, boolean kwCase)
    {
        tokens     = tok;
        keywords   = kw;
        ignoreCase = kwCase;
        inputFile  = null;
        inputLine  = input;
        inputBlock = inputLine;
        nextChar   = 0;
		matchPos   = 0;
		inputLen   = 0;
		inComment  = false;
    }
    private void cleanup()
    {
        //
        // Dereference resources
        //
        inputLine  = null;
        inputBlock = null;
        tokens     = null;
        keywords   = null;
        inputFile  = null;
    }
    protected void finalize()
    {
        cleanup();
    }
    public void finished()
    {
        cleanup();
    }
	private boolean readBlock()
	{
		/*
		 * Fetch next block of input.
		 */
		if (inputFile != null)
		{
			try
			{
				int len = inputFile.read(inputBlock);
				if (len < inputBlock.length)
				{
					if (len < 0)
					{
						nextChar = -1;
						return false;
					}
					else
					{
						/*
						 * Shift data to end of inputBlock.
						 */
						nextChar = inputBlock.length - len;
						for (int i = 1; i <= len; i++)
							inputBlock[inputBlock.length - i] = inputBlock[len - i];
					}
				}
				else
					nextChar = 0;
			}
			catch (IOException e)
			{
				nextChar = -1;
				return false;
			}
		}
		else
			return false;
		return true;
	}
    /*
     * Return escaped character value.
     */
    private static byte escapeChar(int esc)
    {
        switch (esc)
        {
            case 'n':
                return (byte)'\n';
            case 'r':
                return (byte)'\r';
            case 't':
                return (byte)'\t';
            case 'b':
                return (byte)'\b';
            case '\\':
                return (byte)'\\';
            case '\'':
                return (byte)'\'';
            case '\"':
                return (byte)'\"';
        }
        return (byte)esc;
    }
    /*
     * Get next token from input stream.
     */
    public int getToken()
    {
		byte scanLine[] = inputLine;
        int c, p, C, tt;
		int matchEnd, matchLen, tokenLen;
        String matchToken;
		do
		{
			if (matchPos >= inputLen)
			{
				/*
				 * Read until EOL.
				 */
				if (nextChar < 0)
					return TOK_ENDOFINPUT;
				/*
				 * Read until a non-blank line or EOF.
				 */
				do
				{
					/*
					 * Skip leading whitespace.
					 */
					if (nextChar == inputBlock.length)
					{
						if (!readBlock())
							return TOK_ENDOFINPUT;
					}

				} while (inputBlock[nextChar++] <= ' ');
				scanLine[0] = inputBlock[nextChar - 1];
				p           = 1;
				do
				{
					/*
					 * Read until EOL.
					 */
					if (nextChar == inputBlock.length)
					{
						if (!readBlock())
						{
							if (p++ == 0)
								return TOK_ENDOFINPUT;
							break;
						}
					}
					c = scanLine[p++] = inputBlock[nextChar++];
				} while (c != '\r' && c != '\n' && p < scanLine.length);
				scanLine[p--] = 0;
				inputLen = p;
				matchPos = 0;
			}
			/*
			 * Scan the input line.
			 */
			C = (c = scanLine[matchPos]) & 0x5F;
			/*
			 * Check for comments
			 */
			if (inComment)
			{
				if ((c == '*') && (scanLine[matchPos + 1] == '/'))
				{
					/*
					 * Return to scanning for tokens.
					 */
					inComment = false;
					matchPos += 2;
				}
				else
				{
					/*
					 * Eat characters.
					 */
					while ((++matchPos < (inputLen - 1))
						&& (scanLine[matchPos] != '*')
						&& (scanLine[matchPos + 1] != '/'));
				}
			}
			else if ((c == '/') && (scanLine[matchPos + 1] == '*'))
			{
				/*
				 * Inside of comment block.
				 */
				inComment = true;
				matchPos += 2;
			}
			else if ((c == '/') && (scanLine[matchPos + 1] == '/'))
				/*
				 * Eat rest of line.
				 */
				inputLen = 0;
			else if (c <= ' ')
				/*
				 * Eat whitespace characters.
				 */
				matchPos++;
			/*
			 * Look for token matches.
			 */
			else if ((C >= 'A' && C <= 'Z') || (c == '_'))
			{
				/*
				 * Match identifier.
				 */
				matchEnd  = matchPos;
				do
				{
					C = (c = scanLine[++matchEnd]) & 0x5F;
				} while ((C >= 'A' && C <= 'Z')
						 || (c >= '0' && c <= '9')
						 || (c == '_'));
				matchLen = matchEnd - matchPos;
				if (keywords != null)
				{
					/*
					 * Check for keyword match.
					 */
					for (tt = 0; tt <keywords.length; tt++)
					{
						matchToken = keywords[tt];
						if ((tokenLen = matchToken.length()) > matchLen)
							/*
							 * No match - tokens in acsending length order.
							 */
							break;
						if (matchLen == tokenLen)
						{
							if (ignoreCase)
								for (p = 0; (p < tokenLen) && ((scanLine[matchPos + p] & 0x5F) == matchToken.charAt(p)); p++);
							else
								for (p = 0; (p < tokenLen) && (scanLine[matchPos + p] == matchToken.charAt(p)); p++);
							if (p == tokenLen)
							{
								/*
								 * Update current token match.
								 */
								stringValue = matchToken;
								matchPos    = matchEnd;
								return tt + TOK_KEYWORD;
							}
						}
					}
				}
				/*
				 * Create string from token chars.
				 */
				stringValue = new String(scanLine, matchPos, matchLen);
				matchPos    = matchEnd;
				return TOK_ID;
			}
			else if ((c == '0') && ((scanLine[matchPos + 1] & 0x5F) == 'X'))
			{
				/*
				 * Match hex number constant.
				 */
				int hex   = 0;
				matchPos += 2;
				while (((c = scanLine[matchPos++]) >= '0' && c <= '9')
					||  (c                         >= 'A' && c <= 'F')
					||  (c                         >= 'a' && c <= 'f'))
				{
					C = c & 0x5F;
					hex = hex * 16 + (C >= 'A' ? C - ('A' - 10) : c - '0');
				}
				intValue = hex;
				return TOK_HEXCONST;
			}
			else if ((c >= '0' && c <= '9')
				  || (c == '.' && (((p = scanLine[matchPos + 1]) >= '0' && p <= '9'))))
			{
				/*
				 * Match number constant.
				 */
				int     mant      = 0;
				int     dp        = 0;
				boolean negExp    = false;
				int     tokenType = TOK_INTCONST;
				while (c >= '0' && c <= '9')
				{
					mant = mant * 10 + (c - '0');
					c = scanLine[++matchPos];
				}
				if (c == '.')
				{
					/*
					 * Float fraction.
					 */
					tokenType = TOK_FLOATCONST;
					while (((c = scanLine[++matchPos]) >= '0') && (c <= '9'))
					{
						dp--;
						mant = mant * 10 + (c - '0');
					}
				}
				if ((c & 0x5F) == 'E')
				{
					/*
					 * Float exponent.
					 */
					int   exp = 0;
					tokenType = TOK_FLOATCONST;
					c = scanLine[++matchPos];
					if (c == '-')
					{
						negExp = true;
						c = scanLine[++matchPos];
					}
					else if (c == '+')
						c = scanLine[++matchPos];
					if (c >= '0' && c <= '9')
					{
						exp = c - '0';
						if ((c = scanLine[++matchPos]) >= '0' && c <= '9')
						{
							exp = exp * 10 + (c - '0');
							c = scanLine[++matchPos];
						}
					}
					else
						return TOK_FLOATERR;   // Malformed exponent
					if (negExp)
						exp = -exp;
					dp += exp;
				}
				if ((c & 0x5F) == 'F')
				{
					/*
					 * Force float type.
					 */
					tokenType = TOK_FLOATCONST;
					matchPos++;
				}
				if (tokenType == TOK_INTCONST)
					intValue = mant;
				else
				{
					int place;
					float scale;

					if (dp < 0)
					{
						dp = -dp;
						negExp = true;
					}
					else
						negExp = false;
					//
					// Apply scale
					//
					for (place = 0, scale = 1.0F; dp != 0; dp >>= 1, place++)
						if ((dp & 1) != 0)
							scale *= twoPow10[place];
					if (negExp)
						floatValue = (float)mant / scale;
					else
						floatValue = (float)mant * scale;
				}
				return tokenType;
			}
			else if (c == '\'')
			{
				/*
				 * Match character constant.
				 */
				tokenLen  = 0;
				if ((c = scanLine[++matchPos]) == '\\')
					intValue = escapeChar(scanLine[++matchPos]);
				else
					intValue = c;
				if (scanLine[++matchPos] != '\'')
					return TOK_CHARERR; // Malformed char literal
				matchPos++;
				return TOK_CHARCONST;
			}
			else if (c == '\"')
			{
				/*
				 * Match string literal.
				 */
				matchEnd = ++matchPos;
				do
				{
					if ((c = scanLine[matchEnd++]) == '\\')
					{
						scanLine[matchEnd - 1] = escapeChar(scanLine[matchEnd]);
						/*
						 * Move input down.
						 */
						for (p = matchEnd; p < inputLen; p++)
							scanLine[p] = scanLine[p + 1];
						inputLen--;
					}
				} while (c != '\"');
				/*
				 * Create string from token chars.
				 */
				stringValue = new String(scanLine, matchPos, matchEnd - matchPos - 1);
				matchPos = matchEnd;
				return TOK_STRINGCONST;
			}
			else
			{
				/*
				 * Match token.
				 */
				for (tt = 0; tt <tokens.length; tt++)
				{
					if (scanLine[matchPos] == tokens[tt].charAt(0))
					{
						matchToken = tokens[tt];
						tokenLen   = matchToken.length();
						for (p = 1; (p < tokenLen) && (scanLine[matchPos + p] == matchToken.charAt(p)); p++);
						if (p == tokenLen)
						{
							/*
							 * Update current token match.
							 */
							stringValue = matchToken;
							matchPos   += tokenLen;
							return tt + TOK_TOKEN;
						}
					}
				}
				/*
				 * Unknown token.
				 */
				return TOK_ERR;
			}
		} while (true);
	}
}