//
// favac class
//
package org.vm02.favac;
import java.io.*;

class favac
{
    private static String favaKeywords[] = {
        "if",
        "do",
        "int",
        "new",
        "for",
        "try",
        "void",
        "char",
        "long",
        "else",
        "case",
        "byte",
        "true",
        "null",
        "float",
        "short",
        "while",
        "break",
        "false",
        "catch",
        "class",
        "return",
        "static",
        "public",
        "switch",
        "double",
        "import",
        "default",
        "finally",
        "private",
        "boolean",
        "package",
        "protected"
    };
    private static String favaTokens[] = {
        ";",                                // Statement terminator
        "{",                                // Left brace
        "}",                                // Right brace
        "(",                                // Left paren
        ")",                                // Right paren
        ",",                                // Comma
        ".",                                // Period
        "[",                                // Left bracket
        "]",                                // Right bracket
        "==",                               // Is equal
        "=",                                // Set
        "<<",                               // Left shift
        "<=",                               // Less than or equal
        "<",                                // Less than
        ">>>",                              // Unsigned right shift
        ">>",                               // Right shift
        ">=",                               // Greater than or equal
        ">",                                // Greater than
        "++",                               // Increment operator
        "+=",                               // Plus equals
        "+",                                // Add operator
        "--",                               // Decrement operator
        "-=",                               // Minus equals
        "-",                                // Subract and negate operator
        "*=",                               // Times equals
        "*",                                // Multiply operator
        "/=",                               // Divide equals
        "/",                                // Divide operator
        "!=",                               // Not equal
        "!",                                // Logical not
        "%=",                               // Mod equals
        "%",                                // Mod operator
        "||",                               // Logical OR
        "|=",                               // Bitwise OR equals
        "|",                                // Bitwise OR
        "&&",                               // Logical AND
        "&=",                               // Bitwise AND equals
        "&",                                // Bitwise AND
        "^=",                               // Bitwise XOR equals
        "^",                                // Bitwise XOR
        "~=",                               // Bitwise Compliment equals
        "~",                                // Bitwise compliment
        "?",                                // Question mark - Tertiary if-then-else
        ":",                                // Colon
        "\\",                               // Backslash
        "@",                                // At sign
        "#",                                // Pound sign
        "$"                                 // Dollar sign
    };
        
    static void printIndent(int indent)
    {
        for (int i = 0; i < indent; i++)
            System.out.print("    ");
    }
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
            System.out.println(args[0]);
        }
        Scanner lex = new Scanner(input, favaTokens, favaKeywords, false);
        int start = (int)System.currentTimeMillis();
        while ((tt = lex.getToken()) > 0)
        {
            switch (tt)
            {
                case Scanner.TOK_ID:
                    if (prevToken == Scanner.TOK_ID)
                        System.out.print(' ');
                    System.out.print(lex.stringValue);
                    break;
                case Scanner.TOK_HEXCONST:
                    System.out.print("0x");
                    System.out.print(Integer.toHexString(lex.intValue));
                    break;
                case Scanner.TOK_INTCONST:
                    System.out.print(lex.intValue);
                    break;
                case Scanner.TOK_FLOATCONST:
                    System.out.print(lex.floatValue);
                    break;
                case Scanner.TOK_CHARCONST:
                    System.out.print("\'" + (char)lex.intValue + "\'");
                    break;
                case Scanner.TOK_STRINGCONST:
                    System.out.print("\"" + lex.stringValue + "\"");
                    break;
                case Scanner.TOK_TOKEN+0:
                    System.out.println(";");
                    printIndent(indent);
                    break;
                case Scanner.TOK_TOKEN+1:
                    System.out.println("");
                    printIndent(indent);
                    System.out.println("{");
                    printIndent(++indent);
                    break;
                case Scanner.TOK_TOKEN+2:
                    System.out.print("\b\b\b\b");
                    System.out.println("}");
                    printIndent(--indent);
                    break;
                case Scanner.TOK_TOKEN+5:
                    System.out.print(", ");
                    break;
                default:
                    if (tt >= Scanner.TOK_KEYWORD && tt < Scanner.TOK_TOKEN)
                        System.out.print(lex.stringValue + " ");
                    else
                        if (tt > 136 && tt != 146 && tt != 149)
                            System.out.print(" " + lex.stringValue + " ");
                        else
                            System.out.print(lex.stringValue);
            }
            prevToken = tt;
        }
        int stop = (int)System.currentTimeMillis();
        lex.finished();
        System.out.println("Elapsed time = " + (stop - start) + " milliseconds");
    }
}
