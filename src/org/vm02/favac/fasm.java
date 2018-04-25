//
// fasm 6502 natic class assembler
//
package org.vm02.favac;
import java.io.*;

class fasm
{
    private static String fasmKeywords[] = {
        "A",
        "X",
        "Y",
        "LDA",
        "LDY",
        "LDX",
        "STA",
        "STX",
        "STY",
        "BEQ",
        "BNE",
        "BCC",
        "BCS",
        "BPL",
        "BMI",
        "BVS",
        "BVC",
        "JMP",
        "JSR",
        "RTS",
        "SEC",
        "CLC",
        "SEI",
        "CLI",
        "BIT",
        "CMP",
        "CPX",
        "CPY",
        "ADC",
        "SBC",
        "ORA",
        "AND",
        "EOR",
        "PHA",
        "PLA",
        "PHP",
        "PLP",
        "TAX",
        "TXA",
        "TAY",
        "TYA",
        "TSX",
        "TXS",
        "BRK",
        "EQU",
        "DFB",
        "DFW",
        "HEX",
        "FIELD",
        "CLASS",
        "METHOD"
    };
    private static String fasmTokens[] = {
        "(",                                // Left paren
        ")",                                // Right paren
        ",",                                // Comma
        "#",                                // Pound sign
        ":"
    };
    public static void main(String args[])
    {
        int tt, prevToken = -1, indent = 0;
        InputStream input = System.in;        
        if (args.length > 0)
        {
            try
            {
                input = new FileInputStream(args[0]);
            }
            catch (FileNotFoundException e)
            {
                System.err.println("Unable to open file: " + args[0]);
                System.exit(-1);
            }
            catch (IOException e)
            {
                System.err.println("Unable to access file: " + args[0]);
                System.exit(-1);
            }
        }
        Scanner lex = new Scanner(input, fasmTokens, fasmKeywords, true);
        while ((tt = lex.getToken()) >= 0)
        {
            switch (tt)
            {
                case Scanner.TOK_ID:
                    System.out.println(lex.stringValue);
                    break;
                case Scanner.TOK_HEXCONST:
                case Scanner.TOK_INTCONST:
                    System.out.println(lex.intValue);
                    break;
                case Scanner.TOK_FLOATCONST:
                    System.out.println(lex.floatValue);
                    break;
                case Scanner.TOK_CHARCONST:
                    System.out.println("\'" + (char)lex.intValue + "\'");
                    break;
                case Scanner.TOK_STRINGCONST:
                    System.out.println("\"" + lex.stringValue + "\"");
                    break;
                default:
                    System.out.println(lex.stringValue);
            }
        }
        lex.finished();
    }
}
