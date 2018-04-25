#include <stdio.h>
#include "tokens.h"

char *statement, *scanpos, *tokenstr;
t_token scantoken, prevtoken;
int tokenlen;
long constval;
int lineno = 0;
t_token keywords[] = {
                        IF_TOKEN,               'I', 'F',
                        ELSE_TOKEN,             'E', 'L', 'S', 'E',
                        ELSEIF_TOKEN,           'E', 'L', 'S', 'I', 'F',
                        FIN_TOKEN,              'F', 'I', 'N',
                        WHILE_TOKEN,            'W', 'H', 'I', 'L', 'E',
                        LOOP_TOKEN,             'L', 'O', 'O', 'P',
                        CASE_TOKEN,             'W', 'H', 'E', 'N',
                        OF_TOKEN,               'I', 'S',
                        DEFAULT_TOKEN,          'O', 'T', 'H', 'E', 'R', 'W', 'I', 'S', 'E',
                        ENDCASE_TOKEN,          'W', 'E', 'N', 'D',
                        FOR_TOKEN,              'F', 'O', 'R',
                        TO_TOKEN,	        'T', 'O',
                        DOWNTO_TOKEN,	        'D', 'O', 'W', 'N', 'T', 'O',
                        STEP_TOKEN,	        'S', 'T', 'E', 'P',
                        NEXT_TOKEN,             'N', 'E', 'X', 'T',
                        REPEAT_TOKEN,           'R', 'E', 'P', 'E', 'A', 'T',
                        UNTIL_TOKEN,	        'U', 'N', 'T', 'I', 'L',
                        BREAK_TOKEN,	        'B', 'R', 'E', 'A', 'K',
                        ASM_TOKEN,	        'A', 'S', 'M',
                        IFUNC_TOKEN,	        'D', 'E', 'F',
                        NFUNC_TOKEN,	        'D', 'E', 'F', 'O', 'P', 'T',
                        DROP_TOKEN,	        'D', 'R', 'O', 'P',
                        RETURN_TOKEN,           'R', 'E', 'T', 'U', 'R', 'N',
                        END_TOKEN,              'E', 'N', 'D',
                        START_TOKEN,	        'S', 'T', 'A', 'R', 'T',
                        DONE_TOKEN,	        'D', 'O', 'N', 'E',
                        LOGIC_NOT_TOKEN,        'N', 'O', 'T',
                        LOGIC_AND_TOKEN,        'A', 'N', 'D',
                        LOGIC_OR_TOKEN,	        'O', 'R',
                        BYTE_TOKEN,             'B', 'Y', 'T', 'E',
                        WORD_TOKEN,	        'W', 'O', 'R', 'D',
                        CONST_TOKEN,            'C', 'O', 'N', 'S', 'T',
                        FUNC_TOKEN,             'F', 'U', 'N', 'C',
                        EOL_TOKEN
                };

void parse_error(char *errormsg)
{
	char *error_carrot = statement;

	fprintf(stderr, "\n%4d: %s\n      ", lineno, statement);
	for (error_carrot = statement; error_carrot != tokenstr; error_carrot++)
		putc(*error_carrot == '\t' ? '\t' : ' ', stderr);
	fprintf(stderr, "^\nError: %s\n", errormsg);
}
t_token scan(void)
{
	prevtoken = scantoken;
	/*
	 * Skip whitespace.
	 */
	while (*scanpos && (*scanpos == ' ' || *scanpos == '\t')) scanpos++;
	tokenstr = scanpos;
	/*
	 * Scan for token based on first character.
	 */
	if (*scanpos == '\0' || *scanpos == '\n' ||*scanpos == ';')
		scantoken = EOL_TOKEN;
	else if ((scanpos[0] >= 'a' && scanpos[0] <= 'z')
	 || (scanpos[0] >= 'A' && scanpos[0] <= 'Z')
	 || (scanpos[0] == '_'))
	{
		/*
		 * ID,  either variable name or reserved word.
		 */
		int keypos = 0, matchpos = 0;

		do
		{
			scanpos++;
		} while ((*scanpos >= 'a' && *scanpos <= 'z')
			  || (*scanpos >= 'A' && *scanpos <= 'Z')
			  || (*scanpos == '_')
			  || (*scanpos >= '0' && *scanpos <= '9'));
		scantoken = ID_TOKEN;
		tokenlen = scanpos - tokenstr;
		/*
		 * Search for matching keyword.
		 */
		while (keywords[keypos] != EOL_TOKEN)
		{
			while (keywords[keypos + 1 + matchpos] == toupper(tokenstr[matchpos]))
				matchpos++;
			if (IS_TOKEN(keywords[keypos + 1 + matchpos]) && (matchpos == tokenlen))
			{
				/*
				 * A match.
				 */
				scantoken = keywords[keypos];
				break;
			}
			else
			{
				/*
				 * Find next keyword.
				 */
				keypos  += matchpos + 1;
				matchpos = 0;
				while (!IS_TOKEN(keywords[keypos])) keypos++;
			}
		}
	}
	else if (scanpos[0] >= '0' && scanpos[0] <= '9')
	{
		/*
		 * Number constant.
		 */
		for (constval = 0; *scanpos >= '0' && *scanpos <= '9'; scanpos++)
			constval = constval * 10 + *scanpos - '0';
		scantoken = INT_TOKEN;
	}
	else if (scanpos[0] == '$')
	{
	        /*
	         * Hexadecimal constant.
	         */
		constval = 0;
	        while (scanpos++)
		{
		        if (*scanpos >= '0' && *scanpos <= '9')
        			constval = constval * 16 + *scanpos - '0';
		        else if (*scanpos >= 'A' && *scanpos <= 'F')
        			constval = constval * 16 + *scanpos - 'A' + 10;
		        else if (*scanpos >= 'a' && *scanpos <= 'f')
        			constval = constval * 16 + *scanpos - 'a' + 10;
        		else
        		        break;
        	}
		scantoken = INT_TOKEN;
	}
	else if (scanpos[0] == '\'')
	{
		/*
		 * Character constant.
		 */
		scantoken = CHAR_TOKEN;
		if (scanpos[1] != '\\')
		{
            constval =  scanpos[1];
            if (scanpos[2] != '\'')
            {

                    parse_error("Bad character constant");
                    return (-1);
            }
            scanpos += 3;
    }
    else
    {
            switch (scanpos[2])
            {
                    case 'n':
                            constval = '\n';
                            break;
                    case 'r':
                            constval = '\r';
                            break;
                    case 't':
                            constval = '\t';
                            break;
                    case '\'':
                            constval = '\'';
                    case '\\':
                            constval = '\\';
                    case '0':
                            constval = '\0';
                            break;
                    default:
                            parse_error("Bad character constant");
                            return (-1);
            }
            if (scanpos[3] != '\'')
            {

                    parse_error("Bad character constant");
                    return (-1);
            }
            scanpos += 4;
    }
	}
	else if (scanpos[0] == '\"')
	{
		/*
		 * String constant.
		 */
		scantoken = STRING_TOKEN;
		constval = (long)++scanpos;
		while (*scanpos &&  *scanpos != '\"')
			scanpos++;
		if (!*scanpos++)
		{
			parse_error("Unterminated string");
			return (-1);
		}
	}
	else switch (scanpos[0])
	{
		/*
		 * Potential two and three character tokens.
		 */
		case '>':
			if (scanpos[1] == '>')
			{
				scantoken = SHR_TOKEN;
				scanpos += 2;
			}
			else if (scanpos[1] == '=')
			{
				scantoken = GE_TOKEN;
				scanpos += 2;
			}
			else
			{
				scantoken = GT_TOKEN;
				scanpos++;
			}
			break;
		case '<':
			if (scanpos[1] == '<')
			{
				scantoken = SHL_TOKEN;
				scanpos += 2;
			}
			else if (scanpos[1] == '=')
			{
				scantoken = LE_TOKEN;
				scanpos += 2;
			}
			else if (scanpos[1] == '>')
			{
				scantoken = NE_TOKEN;
				scanpos += 2;
			}
			else
			{
				scantoken = LT_TOKEN;
				scanpos++;
			}
			break;
		case '=':
			if (scanpos[1] == '=')
			{
				scantoken = EQ_TOKEN;
				scanpos += 2;
			}
                        else if (scanpos[1] == ',')
			{
				scantoken = SETLIST_TOKEN;
				scanpos += 2;
			}
			else
			{
				scantoken = SET_TOKEN;
				scanpos++;
			}
			break;
		case '+':
			if (scanpos[1] == '+')
			{
				scantoken = INC_TOKEN;
				scanpos += 2;
			}
			else
			{
				scantoken = ADD_TOKEN;
				scanpos++;
			}
			break;
		case '-':
			if (scanpos[1] == '-')
			{
				scantoken = DEC_TOKEN;
				scanpos += 2;
			}
			else
			{
				scantoken = SUB_TOKEN;
				scanpos++;
			}
			break;
		default:
			/*
			 * Simple single character tokens.
			 */
			scantoken = TOKEN(*scanpos++);
	}
	tokenlen = scanpos - tokenstr;
	return (scantoken);
}
void scan_rewind(char *backptr)
{
        scanpos = backptr;
}
char inputline[128];
int next_line(void)
{
        gets(inputline);
        lineno++;
        statement = inputline;
        scanpos   = inputline;
        scantoken = EOL_TOKEN;
        scan();
        printf("; %4d: %s\n", lineno, inputline);
        return (1);
}
