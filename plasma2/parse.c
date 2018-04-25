#include <stdio.h>
#include "tokens.h"
#include "lex.h"
#include "codegen.h"

int infunc = 0, retfunc_tag = 0, break_tag = 0, stack_loop = 0;
t_token prevstmnt;

t_token binary_ops_table[] = {
                        /* Highest precedence */
                        MUL_TOKEN, DIV_TOKEN, DIVMOD_TOKEN,
                        ADD_TOKEN, SUB_TOKEN,
                        SHR_TOKEN, SHL_TOKEN,
                        AND_TOKEN,
                        EOR_TOKEN,
                        OR_TOKEN,
                        GT_TOKEN, GE_TOKEN, LT_TOKEN, LE_TOKEN,
                        EQ_TOKEN, NE_TOKEN,
                        LOGIC_AND_TOKEN,
                        LOGIC_OR_TOKEN,
                        COMMA_TOKEN
                        /* Lowest precedence  */
                };
t_token binary_ops_precedence[] = {
                        /* Highest precedence */
                        1, 1, 1,
                        2, 2,
                        3, 3,
                        4,
                        5,
                        6,
                        7, 7, 7, 7,
                        8, 8,
                        9,
                        10,
                        11
                        /* Lowest precedence  */
                };

t_token opstack[16];
int precstack[16];
int opsptr = -1;
void push_op(t_token op, int prec)
{
        if (++opsptr == 16)
        {
                parse_error("Stack overflow\n");
                return;
        }
        opstack[opsptr]   = op;
        precstack[opsptr] = prec;
}
t_token pop_op(void)
{
        if (opsptr < 0)
        {
                parse_error("Stack underflow\n");
                return (0);
        }
        return opstack[opsptr--];
}
t_token tos_op(void)
{
        return opsptr < 0 ? 0 : opstack[opsptr];
}
int tos_op_prec(int tos)
{
        return opsptr <= tos ? 100 : precstack[opsptr];
}
int  consts = 0;
char idconst_name[1024][17];
int  idconst_value[1024];
int  globals = 0;
int  globalsize = 0;
char idglobal_name[1024][17];
int  idglobal_type[1024];
int  idglobal_tag[1024];
int  locals = 0;
int  localsize = 0;
char idlocal_name[128][17];
int  idlocal_type[128];
int  idlocal_offset[128];
int id_match(char *name, int len, char *id)
{
        if (len == id[0])
        {
                if (len > 16) len = 16;
                while (len--)
                {
                        if (name[len] != id[1 + len])
                                return (0);
                }
                return (1);
        }
        return (0);
}
int idlocal_lookup(char *name, int len)
{
        int i;
        for (i = 0; i < locals; i++)
                if (id_match(name, len, &(idlocal_name[i][0])))
                        return (i);
        return (-1);
}
int idglobal_lookup(char *name, int len)
{
        int i;
        for (i = 0; i < globals; i++)
                if (id_match(name, len, &(idglobal_name[i][0])))
                        return (i);
        return (-1);
}
int idconst_lookup(char *name, int len)
{
        int i;
        for (i = 0; i < consts; i++)
                if (id_match(name, len, &(idconst_name[i][0])))
                        return (i);
        return (-1);
}
int idlocal_add(char *name, int len, int type, int size)
{
        if (localsize > 255)
        {
                printf("Local variable size overflow\n");
                return (0);
        }
        char c = name[len];
        name[len] = '\0';
        emit_idlocal(name, localsize);
        name[len] = c;
        idlocal_name[locals][0] = len;
        if (len > 16) len = 16;
        while (len--)
                idlocal_name[locals][1 + len] = name[len];
        idlocal_type[locals]   = type;
        idlocal_offset[locals] = localsize;
        localsize += size;
        locals++;
        return (1);
}
int idglobal_add(char *name, int len, int type, int size)
{
        if (globals > 1024)
        {
                printf("Global variable count overflow\n");
                return (0);
        }
        char c = name[len];
        name[len] = '\0';
        emit_idglobal(globalsize, size, name);
        name[len] = c;
        idglobal_name[globals][0] = len;
        if (len > 16) len = 16;
        while (len--)
                idglobal_name[globals][1 + len] = name[len];
        idglobal_type[globals] = type;
        idglobal_tag[globals]  = globalsize;
        globalsize += size;
        globals++;
        return (1);
}
void idglobal_size(int type, int size, int constsize)
{
        if (size > constsize)
                globalsize += emit_data(0, 0, 0, size - constsize);
        else
                globalsize += constsize;
}
int idfunc_add(char *name, int len, int tag)
{
        if (globals > 1024)
        {
                printf("Global variable count overflow\n");
                return (0);
        }
        idglobal_name[globals][0] = len;
        if (len > 16) len = 16;
        while (len--)
                idglobal_name[globals][1 + len] = name[len];
        idglobal_type[globals] = FUNC_TYPE;
        idglobal_tag[globals]  = tag;
        globals++;
        return (1);
}
int idconst_add(char *name, int len, int value)
{
        if (consts > 1024)
        {
                printf("Constant count overflow\n");
                return (0);
        }
        char c = name[len];
        name[len] = '\0';
        emit_idconst(name, value);
        name[len] = c;
        idconst_name[consts][0] = len;
        if (len > 16) len = 16;
        while (len--)
                idconst_name[consts][1 + len] = name[len];
        idconst_value[consts] = value;
        consts++;
        return (1);
}
int id_addr(char *name, int len)
{
        int i;
        if ((i = idlocal_lookup(name, len)) >= 0)
                return (idlocal_offset[i]);
        if ((i = idglobal_lookup(name, len)) >= 0)
                return (idglobal_tag[i]);
        parse_error("Undeclared identifier");
        return (-1);
}
int id_const(char *name, int len)
{
        int i;
        if ((i = idconst_lookup(name, len)) >= 0)
                return (idconst_value[i]);
        parse_error("Undeclared constant");
        return (0);
}
int id_type(char *name, int len)
{
        int i;
        if ((i = idconst_lookup(name, len)) >= 0)
                return (CONST_TYPE);
        if ((i = idlocal_lookup(name, len)) >= 0)
                return (idlocal_type[i] | LOCAL_TYPE);
        if ((i = idglobal_lookup(name, len)) >= 0)
                return (idglobal_type[i]);
        parse_error("Undeclared identifier");
        return (0);
}
void idlocal_reset(void)
{
        locals = localsize = 0;
}
int tag_new(void)
{
        static int codetag = 0;
        return (codetag++);
}
int parse_expr(void);
int parse_term(void)
{
        /*
         * Parse terminal tokens.
         */
        switch (scan())
        {
                case CHAR_TOKEN:
                case INT_TOKEN:
                case FLOAT_TOKEN:
                case ID_TOKEN:
                case STRING_TOKEN:
                        break;
                case OPEN_PAREN_TOKEN:
                        if (!parse_expr())
                        {
                                parse_error("Bad expression in parenthesis");
                                return (0);
                        }
                        if (scantoken != CLOSE_PAREN_TOKEN)
                        {
                                parse_error("Missing closing parenthesis");
                                return (0);
                        }
                        break;
                default:
                        /*
                         * Non-terminal token.
                         */
                        return (0);
        }
        return (1);
}
int parse_constval(long *value, int *size)
{
        int mod = 0, type = 0;
        *value = 0;
        while (!parse_term())
        {
                switch (scantoken)
                {
                        case ADD_TOKEN:
                                /*
                                 * Just ignore unary plus, it is a no-op.
                                 */
                                 break;
                        case SUB_TOKEN: // Really NEG_TOKEN
                                mod |= 1;
                                break;
                        case COMP_TOKEN:
                                mod |= 2;
                                break;
                        case LOGIC_NOT_TOKEN:
                                mod |= 4;
                                break;
                        case AT_TOKEN:
                                mod |= 8;
                                break;
                        default:
                                return (0);
                }
        }
        /*
         * Determine which terminal type.
         */
        if (scantoken == STRING_TOKEN)
        {
                *value = constval;
                *size  = tokenlen - 1;
                type   = STRING_TYPE;
                if (mod)
                {
                        parse_error("Invalid string modifiers");
                        return (0);
                }
        }
        else if (scantoken == CHAR_TOKEN)
        {
                *value = constval;
                *size  = 1;
                type   = BYTE_TYPE;
        }
        else if (scantoken == INT_TOKEN)
        {
                *value = constval;
                *size  = 2;
                type   = WORD_TYPE;
        }
        else if (scantoken == ID_TOKEN)
        {
                type = id_type(tokenstr, tokenlen);
                if (type & CONST_TYPE)
                        *value = id_const(tokenstr, tokenlen);
                else if ((type & VAR_TYPE) && (mod & 8))
                {
                        type = TAG_TYPE;
                        *value = id_addr(tokenstr, tokenlen);
                }
                else if (type & FUNC_TYPE)
                {
                        type = TAG_TYPE;
                        *value = id_addr(tokenstr, tokenlen) | 0x8000;
                }
                else
                {
                        parse_error("Invalid constant");
                        return (0);
                }
        }
        else
        {
                parse_error("Invalid constant");
                return (0);
        }
        if (mod & 1)
                *value = -*value;
        if (mod & 2)
                *value = ~*value;
        if (mod & 4)
                *value = *value ? 0 : -1;
        return (type);
}
int parse_value(int rvalue)
{
        int cparams;
        int deref = rvalue;
        int optos = opsptr;
        int type = 0, value = 0, emit_value = 0;
        /*
         * Parse pre operand operators.
         */
        while (!parse_term())
        {
                switch (scantoken)
                {
                        case ADD_TOKEN:
                                /*
                                 * Just ignore unary plus, it is a no-op.
                                 */
                                 break;
                        case BPTR_TOKEN:
                                if (deref)
                                        push_op(scantoken, 0);
                                else
                                {
                                        type |= BPTR_TYPE;
                                        deref++;
                                }
                                break;
                        case WPTR_TOKEN:
                                if (deref)
                                        push_op(scantoken, 0);
                                else
                                {
                                        type |= WPTR_TYPE;
                                        deref++;
                                }
                                break;
                        case AT_TOKEN:
                                deref--;
                                break;
                        case SUB_TOKEN: // Really NEG_TOKEN
                        case COMP_TOKEN:
                        case LOGIC_NOT_TOKEN:
                                push_op(scantoken, 0);
                                break;
                        default:
                                return (0);
                }
        }
        /*
         * Determine which terminal type.
         */
        if (scantoken == INT_TOKEN || scantoken == CHAR_TOKEN)
        {
                value = constval;
                type |= CONST_TYPE;
        }
        else if (scantoken == ID_TOKEN)
        {
                if ((type |= id_type(tokenstr, tokenlen)) & CONST_TYPE)
                        value = id_const(tokenstr, tokenlen);
                else if (type & VAR_TYPE)
                        value = id_addr(tokenstr, tokenlen);
                else if (type & FUNC_TYPE)
                        value = id_addr(tokenstr, tokenlen);
                else
                {
                        printf("Bad ID type\n");
                        return (0);
                }
        }
        else if (scantoken == CLOSE_PAREN_TOKEN)
        {
//                type |= WORD_TYPE;
                emit_value = 1;
        }
        else
                 return (0);
        if (type & CONST_TYPE)
        {
                /*
                 * Quick optimizations
                 */
                while ((optos < opsptr)
                   && ((tos_op() == NEG_TOKEN) || (tos_op() == COMP_TOKEN) || (tos_op() == LOGIC_NOT_TOKEN)))
                {
                        switch (pop_op())
                        {
                                case NEG_TOKEN:
                                        value = -value;
                                        break;
                                case COMP_TOKEN:
                                        value = ~value;
                                        break;
                                case LOGIC_NOT_TOKEN:
                                        value = value ? 0 : -1;
                                        break;
                        }
                }
        }
        /*
         * Parse post operand operators.
         */
		scan();
        while (scantoken == OPEN_PAREN_TOKEN
            || scantoken == OPEN_BRACKET_TOKEN
            || scantoken == DOT_TOKEN
            || scantoken == COLON_TOKEN)
        {
				/*
				 * Parse post operand operators.
				 */
				if (scantoken == OPEN_BRACKET_TOKEN)
				{
						/*
						 * Array
						 */
						if (!emit_value)
						{
								if (type & ADDR_TYPE)
								{
										if (type & LOCAL_TYPE)
												emit_localaddr(value);
										else
												emit_globaladdr(value, type);
								}
								else if (type & CONST_TYPE)
								{
										emit_const(value);
								}
								emit_value = 1;
						}
						if (type & PTR_TYPE)
								emit_lw();
						if (!parse_expr())
						{
								parse_error("Bad expression");
								return (0);
						}
						if (scantoken != CLOSE_BRACKET_TOKEN)
						{
								parse_error("Missing closing bracket");
								return (0);
						}
						if (type & WORD_TYPE)
						{
								//type |= WPTR_TYPE;
								type = WPTR_TYPE;
								emit_indexword();
						}
						else
						{
								//type |= BPTR_TYPE;
								type = BPTR_TYPE;
								emit_indexbyte();
						}
						//type &= ~(ADDR_TYPE | CONST_TYPE);
						scan();
				}
				else if (scantoken == DOT_TOKEN || scantoken == COLON_TOKEN)
				{
						/*
						 * Structure member offset or array of arrays
						 */
						int elem_size;
						int elem_type   = (scantoken == DOT_TOKEN) ? BPTR_TYPE : WPTR_TYPE;
						long elem_offset = 0;
						if (parse_constval(&elem_offset, &elem_size))
						{
								/*
								 * Constant member offset
								 */
								if (!emit_value)
								{
										if (type & VAR_TYPE)
										{
												if (type & LOCAL_TYPE)
														emit_localaddr(value + elem_offset);
												else
														emit_globaladdrofst(value, elem_offset, type);
										}
										else if (type & CONST_TYPE)
										{
												value += elem_offset;
												emit_const(value);
										}
										else // FUNC_TYPE
										{
												emit_globaladdr(value, type);
												emit_const(elem_offset);
												emit_op(ADD_TOKEN);
										}
										emit_value = 1;
								}
								else
								{
										if (elem_offset != 0)
										{
												emit_const(elem_offset);
												emit_op(ADD_TOKEN);
										}
								}
								scan();
						}
						else if (scantoken == OPEN_BRACKET_TOKEN)
						{
								/*
								 * Array of arrays
								 */
								if (!emit_value)
								{
										if (type & ADDR_TYPE)
										{
												if (type & LOCAL_TYPE)
														emit_localaddr(value);
												else
														emit_globaladdr(value, type);
										}
										else if (type & CONST_TYPE)
										{
												emit_const(value);
										}
										emit_value = 1;
								}
								do
								{
										if (emit_value++ > 1)
										{
												emit_indexword();
												emit_lw();
										}
										if (!parse_expr())
										{
												parse_error("Bad expression");
												return (0);
										}
										if (scantoken != CLOSE_BRACKET_TOKEN)
										{
												parse_error("Missing closing bracket");
												return (0);
										}
								} while (scan() == OPEN_BRACKET_TOKEN);
								if (elem_type & WPTR_TYPE)
										emit_indexword();
								else
										emit_indexbyte();
						}
						else
						{
								parse_error("Invalid member offset");
								return (0);
						}
						type = elem_type; //(type & ~(ADDR_TYPE | CONST_TYPE)) | elem_type;
				}
				else if (scantoken == OPEN_PAREN_TOKEN)
				{
						/*
						 * Function call
						 */
						if (!emit_value && (type & VAR_TYPE))
						{
								if (type & LOCAL_TYPE)
										emit_localaddr(value);
								else
										emit_globaladdr(value, type);
						}
						if (type & (VAR_TYPE | PTR_TYPE))
								emit_lw();
						if (!(type & (FUNC_TYPE | CONST_TYPE)))
								emit_push();
						parse_expr();
						if (scantoken != CLOSE_PAREN_TOKEN)
						{
								parse_error("Missing closing parenthesis");
								return (0);
						}
						if (type & (FUNC_TYPE | CONST_TYPE))
								emit_call(value);
						else
						{
								emit_pull();
								emit_ical();
						}
						emit_value = 1;
						type = WORD_TYPE; //(type & ~(FUNC_TYPE | CONST_TYPE)) | WORD_TYPE;
						scan();
				}
        }
        if (emit_value)
        {
                if (rvalue && deref && (type & PTR_TYPE))
                        (type & BPTR_TYPE) ? emit_lb() : emit_lw();
        }
        else
        {
                if (type & CONST_TYPE)
                        emit_const(value);
                else if (deref)
                {
                        if (type & FUNC_TYPE)
                                emit_call(value);
                        else if (type & VAR_TYPE)
                        {
                                if (type & LOCAL_TYPE)
                                        (type & BYTE_TYPE) ? emit_llb(value) : emit_llw(value);
                                else
                                        (type & BYTE_TYPE) ? emit_lab(value) : emit_law(value);
                        }
                        else if (type & PTR_TYPE)
                                (type & BPTR_TYPE) ? emit_lb() : emit_lw();
                }
                else
                {
                        if (type & LOCAL_TYPE)
                                emit_localaddr(value);
                        else
                                emit_globaladdr(value, type);
                }
        }
        while (optos < opsptr)
        {
                if (!emit_unaryop(pop_op()))
                {
                        parse_error(": Invalid unary operation");
                        return (0);
                }
        }
        return (type ? type : WORD_TYPE);
}
int parse_constexpr(long *value, int *size)
{
        long val1, val2;
        int type, size1, size2 = 0;

        if (!(type = parse_constval(&val1, &size1)))
                return (0);
        if (scan() == ADD_TOKEN)
        {
                if (!parse_constval(&val2, &size2))
                        return (0);
                *value = val1 + val2;
        }
        else if (scantoken == SUB_TOKEN)
        {
                if (!parse_constval(&val2, &size2))
                        return (0);
                *value = val1 - val2;
        }
        else if (scantoken == MUL_TOKEN)
        {
                if (!parse_constval(&val2, &size2))
                        return (0);
                *value = val1 * val2;
        }
        else if (scantoken == DIV_TOKEN)
        {
                if (!parse_constval(&val2, &size2))
                        return (0);
                *value = val1 / val2;
        }
        else if (scantoken == AND_TOKEN)
        {
                if (!parse_constval(&val2, &size2))
                        return (0);
                *value = val1 & val2;
        }
        else if (scantoken == OR_TOKEN)
        {
                if (!parse_constval(&val2, &size2))
                        return (0);
                *value = val1 | val2;
        }
        else if (scantoken == EOR_TOKEN)
        {
                if (!parse_constval(&val2, &size2))
                        return (0);
                *value = val1 ^ val2;
        }
        else
                *value = val1;
        *size = size1 > size2 ? size1 : size2;
        return (type);
}
int parse_expr()
{
        int prevmatch;
        int matchop = 0;
        int optos = opsptr;
        int i;
        int prevtype, type = 0;
        do
        {
                /*
                 * Parse sequence of double operand operations.
                 */
                prevmatch = matchop;
                matchop   = 0;
                if (parse_value(1))
                {
                        matchop = 1;
                        for (i = 0; i < sizeof(binary_ops_table); i++)
                                if (scantoken == binary_ops_table[i])
                                {
                                        matchop = 2;
                                        if (binary_ops_precedence[i] >= tos_op_prec(optos))
                                                if (!emit_op(pop_op()))
                                                {
                                                        parse_error(": Invalid binary operation");
                                                        return (0);
                                                }
                                        push_op(scantoken, binary_ops_precedence[i]);
                                        break;
                                }
                }
        } while (matchop == 2);
        if (matchop == 0 && prevmatch == 2)
        {
                parse_error("Missing operand");
                return (0);
        }
        while (optos < opsptr)
                if (!emit_op(pop_op()))
                {
                        parse_error(": Invalid binary operation");
                        return (0);
                }
        return (matchop || prevmatch);
}
int parse_setlist(int addr, int type)
{
        int nexttype, nextaddr;
        char *idptr;

        if (!(type & VAR_TYPE))
                emit_push();
        if (scan() == ID_TOKEN)
        {
                nexttype = id_type(tokenstr, tokenlen);
                nextaddr = id_addr(tokenstr, tokenlen);
        }
        else
        {
                nexttype = 0;
                nextaddr = 0;
        }
        idptr = tokenstr;
        scan();
        if (nexttype & VAR_TYPE && scantoken == SET_TOKEN)
        {
                parse_expr();
                if (type & LOCAL_TYPE)
                        (type & BYTE_TYPE) ? emit_slb(nextaddr) : emit_slw(nextaddr);
                else
                        (type & BYTE_TYPE) ? emit_sab(nextaddr) : emit_saw(nextaddr);
        }
        else if (nexttype & VAR_TYPE && scantoken == SETLIST_TOKEN)
        {
                parse_setlist(nextaddr, nexttype);
        }
        else
        {
                tokenstr = idptr;
                scan_rewind(tokenstr);
                if ((nexttype = parse_value(0)) != 0)
                {
                        if (scantoken == SET_TOKEN)
                        {
                                emit_push();
                                parse_expr();
                                emit_pull();
                                emit_swap();
                                (nexttype & (BYTE_TYPE | BPTR_TYPE)) ? emit_sb() : emit_sw();
                        }
                        else if (scantoken == SETLIST_TOKEN)
                                parse_setlist(0, nexttype);
                }
                else
                {
                        parse_error("Syntax error");
                        return (0);
                }
        }
        if (type & VAR_TYPE)
        {
                if (type & LOCAL_TYPE)
                        (type & BYTE_TYPE) ? emit_slb(addr) : emit_slw(addr);
                else
                        (type & BYTE_TYPE) ? emit_sab(addr) : emit_saw(addr);
        }
        else
        {
                emit_pull();
                emit_swap();
                (type & (BYTE_TYPE | BPTR_TYPE)) ? emit_sb() : emit_sw();
        }
        return (1);
}
int parse_stmnt(void)
{
        int tag_prevbrk, tag_else, tag_endif, tag_while, tag_wend, tag_repeat, tag_for, tag_choice, type, addr, step;
        char *idptr;

        /*
         * Optimizationf for last function LEAVE
         */
        if (scantoken != END_TOKEN && scantoken != DONE_TOKEN)
                prevstmnt = scantoken;

        switch (scantoken)
        {
                case IF_TOKEN:
                        parse_expr();
                        tag_else  = tag_new();
                        tag_endif = tag_new();
                        emit_skpfls(tag_else);
                        scan();
                        do {
                                while (parse_stmnt()) next_line();
                                if (scantoken != ELSEIF_TOKEN)
                                        break;
                                emit_skip(tag_endif);
                                emit_codetag(tag_else);
                                if (!parse_expr())
                                {
                                        parse_error("Bad expression");
                                        return (0);
                                }
                                tag_else = tag_new();
                                emit_skpfls(tag_else);
                        } while (1);
                        if (scantoken == ELSE_TOKEN)
                        {
                                emit_skip(tag_endif);
                                emit_codetag(tag_else);
                                scan();
                                while (parse_stmnt()) next_line();
                                emit_codetag(tag_endif);
                        }
                        else
                        {
                                emit_codetag(tag_else);
                                emit_codetag(tag_endif);
                        }
                        if (scantoken != FIN_TOKEN)
                        {
                                parse_error("Missing IF/FIN");
                                return (0);
                        }
                        break;
                case WHILE_TOKEN:
                        tag_while   = tag_new();
                        tag_wend    = tag_new();
                        tag_prevbrk = break_tag;
                        break_tag   = tag_wend;
                        emit_codetag(tag_while);
                        parse_expr();
                        emit_skpfls(tag_wend);
                        while (parse_stmnt()) next_line();
                        if (scantoken != LOOP_TOKEN)
                        {
                                parse_error("Missing WHILE/END");
                                return (0);
                        }
                        emit_skip(tag_while);
                        emit_codetag(tag_wend);
                        break_tag = tag_prevbrk;
                        break;
                case REPEAT_TOKEN:
                        tag_prevbrk = break_tag;
                        break_tag   = tag_new();
                        tag_repeat  = tag_new();
                        emit_codetag(tag_repeat);
                        scan();
                        while (parse_stmnt()) next_line();
                        if (scantoken != UNTIL_TOKEN)
                        {
                                parse_error("Missing REPEAT/UNTIL");
                                return (0);
                        }
                        parse_expr();
                        emit_skpfls(tag_repeat);
                        emit_codetag(break_tag);
                        break_tag = tag_prevbrk;
                        break;
                case FOR_TOKEN:
                        stack_loop++;
                        tag_prevbrk = break_tag;
                        break_tag   = tag_new();
                        tag_for     = tag_new();
                        if (scan() != ID_TOKEN)
                        {
                                parse_error("Missing FOR variable");
                                return (0);
                        }
                        type = id_type(tokenstr, tokenlen);
                        addr = id_addr(tokenstr, tokenlen);
                        if (scan() != SET_TOKEN)
                        {
                                parse_error("Missing FOR =");
                                return (0);
                        }
                        parse_expr();
                        emit_codetag(tag_for);
                        if (type & LOCAL_TYPE)
                                type & BYTE_TYPE ? emit_dlb(addr) : emit_dlw(addr);
                        else
                                type & BYTE_TYPE ? emit_dab(addr) : emit_daw(addr);
                        step = 1;
                        if (scantoken == TO_TOKEN)
                        {
                                parse_expr();
                        }
                        else if (scantoken == DOWNTO_TOKEN)
                        {
                                step = -1;
                                parse_expr();
                        }
                        step > 0 ? emit_skpgt(break_tag) : emit_skplt(break_tag);
                        if (scantoken == STEP_TOKEN)
                        {
                                parse_expr();
                                emit_op(step > 0 ? ADD_TOKEN : SUB_TOKEN);
                        }
                        else
                                emit_unaryop(step > 0 ? INC_TOKEN : DEC_TOKEN);
                        while (parse_stmnt()) next_line();
                        if (scantoken != NEXT_TOKEN)
                        {
                                parse_error("Missing FOR/NEXT ");
                                return (0);
                        }
                        emit_skip(tag_for);
                        emit_codetag(break_tag);
                        emit_drop();
                        break_tag = tag_prevbrk;
                        stack_loop--;
                        break;
                case CASE_TOKEN:
                        stack_loop++;
                        tag_prevbrk = break_tag;
                        break_tag   = tag_new();
                        tag_choice  = tag_new();
                        parse_expr();
                        next_line();
                        while (scantoken != ENDCASE_TOKEN)
                        {
                                if (scantoken == OF_TOKEN)
                                {
                                        if (!parse_expr())
                                        {
                                                parse_error("Bad CASE OF expression");
                                                return (0);
                                        }
                                        emit_skpne(tag_choice);
                                        while (parse_stmnt()) next_line();
                                        emit_skip(break_tag);
                                        emit_codetag(tag_choice);
                                        tag_choice = tag_new();
                                }
                                else if (scantoken == DEFAULT_TOKEN)
                                {
                                        scan();
                                        while (parse_stmnt()) next_line();
                                        if (scantoken != ENDCASE_TOKEN)
                                        {
                                                parse_error("Bad CASE DEFAULT clause");
                                                return (0);
                                        }
                                }
                                else
                                {
                                        parse_error("Bad CASE clause");
                                        return (0);
                                }
                        }
                        emit_codetag(break_tag);
                        emit_drop();
                        break_tag = tag_prevbrk;
                        stack_loop--;
                        break;
                case BREAK_TOKEN:
                        if (break_tag)
                                emit_skip(break_tag);
                        else
                        {
                                parse_error("BREAK without loop");
                                return (0);
                        }
                        break;
                case RETURN_TOKEN:
                        if (infunc)
                        {
                                int i;
                                for (i = 0; i < stack_loop; i++)
                                        emit_drop();
                                parse_expr();
                                emit_leave(localsize);
                        }
                        else
                        {
                                parse_error("RETURN outside of function");
                                return (0);
                        }
                        break;
                case DROP_TOKEN:
                        parse_expr();
                        emit_drop();
                        break;
                case EOL_TOKEN:
                case COMMENT_TOKEN:
                        return (1);
                case ELSE_TOKEN:
                case ELSEIF_TOKEN:
                case FIN_TOKEN:
                case LOOP_TOKEN:
                case UNTIL_TOKEN:
                case NEXT_TOKEN:
                case OF_TOKEN:
                case DEFAULT_TOKEN:
                case ENDCASE_TOKEN:
                case END_TOKEN:
                case DONE_TOKEN:
                case IFUNC_TOKEN:
                case NFUNC_TOKEN:
                        return (0);
                case ID_TOKEN:
                        idptr = tokenstr;
                        type = id_type(tokenstr, tokenlen);
                        if (type & (VAR_TYPE | FUNC_TYPE))
                        {
                                addr = id_addr(tokenstr, tokenlen);
                                if (scan() == SET_TOKEN)
                                {
                                        if (type & VAR_TYPE)
                                        {
                                                parse_expr();
                                                if (type & LOCAL_TYPE)
                                                        (type & BYTE_TYPE) ? emit_slb(addr) : emit_slw(addr);
                                                else
                                                        (type & BYTE_TYPE) ? emit_sab(addr) : emit_saw(addr);
                                                break;
                                        }
                                }
                                else if (type & VAR_TYPE && scantoken == SETLIST_TOKEN)
                                {
                                        parse_setlist(addr, type);
                                        break;
                                }
                                else if ((scantoken == EOL_TOKEN) && (type & FUNC_TYPE))
                                {
                                        emit_call(addr);
                                        break;
                                }
                        }
                        tokenstr = idptr;
                default:
                        scan_rewind(tokenstr);
                        if ((type = parse_value(0)) != 0)
                        {
                                if (scantoken == SET_TOKEN)
                                {
                                        parse_expr();
                                        if (type & LOCAL_TYPE)
                                                (type & (BYTE_TYPE | BPTR_TYPE)) ? emit_sb() : emit_sw();
                                        else
                                                (type & (BYTE_TYPE | BPTR_TYPE)) ? emit_sb() : emit_sw();
                                }
                                else if (scantoken == SETLIST_TOKEN)
                                {
                                        parse_setlist(0, type);
                                }
                                else
                                {
                                        if (type & BPTR_TYPE)
                                                emit_lb();
                                        else if (type & WPTR_TYPE)
                                                emit_lw();
                                }
                        }
                        else
                        {
                                parse_error("Syntax error");
                                return (0);
                        }
        }
        if (scan() != EOL_TOKEN && scantoken != COMMENT_TOKEN)
        {
                parse_error("Extraneous characters");
                return (0);
        }
        return (1);
}
int parse_var(int type)
{
        char *idstr;
        long constval;
        int  consttype, constsize, arraysize, idlen = 0;
        long size = 1;

        if (scan() == ID_TOKEN)
        {
                idstr = tokenstr;
                idlen = tokenlen;
                if (scan() == OPEN_BRACKET_TOKEN)
                {
                        size = 0;
                        parse_constexpr(&size, &constsize);
                        if (scantoken != CLOSE_BRACKET_TOKEN)
                        {
                                parse_error("Missing closing bracket");
                                return (0);
                        }
                        scan();
                }
        }
        if (type == WORD_TYPE)
                size *= 2;
        if (scantoken == SET_TOKEN)
        {
                if (infunc)
                {
                        parse_error("Cannot initiallize local variables");
                        return (0);
                }
                if (idlen)
                        idglobal_add(idstr, idlen, type, 0);
                if ((consttype = parse_constexpr(&constval, &constsize)))
                {
                        /*
                         * Variable initialization.
                         */
                        arraysize = emit_data(type, consttype, constval, constsize);
                        while (scantoken == COMMA_TOKEN)
                        {
                                if ((consttype = parse_constexpr(&constval, &constsize)))
                                        arraysize += emit_data(type, consttype, constval, constsize);
                                else
                                {
                                        parse_error("Bad array declaration");
                                        return (0);
                                }
                        }
                        idglobal_size(PTR_TYPE, size, arraysize);
                }
                else
                {
                        parse_error("Bad variable initializer");
                        return (0);
                }
        }
        else if (idlen)
        {
                infunc ? idlocal_add(idstr, idlen, type, size)
                       : idglobal_add(idstr, idlen, type, size);
        }
        return (1);
}
int parse_vars(void)
{
        long value;
        int type, idlen, size;
        char *idstr;

        switch (scantoken)
        {
                case CONST_TOKEN:
                        if (scan() != ID_TOKEN)
                        {
                                parse_error("Missing variable");
                                return (0);
                        }
                        idstr = tokenstr;
                        idlen = tokenlen;
                        if (scan() != SET_TOKEN)
                        {
                                parse_error("Bad LValue");
                                return (0);
                        }
                        if (!parse_constexpr(&value, &size))
                        {
                                parse_error("Bad constant");
                                return (0);
                        }
                        idconst_add(idstr, idlen, value);
                        break;
                case BYTE_TOKEN:
                case WORD_TOKEN:
                        type = (scantoken == BYTE_TOKEN) ? BYTE_TYPE : WORD_TYPE;
                        if (!parse_var(type))
                                return (0);
                        while (scantoken == COMMA_TOKEN)
                        {
                                if (!parse_var(type))
                                        return (0);
                        }
                        break;
                case FUNC_TOKEN:
                        if (scan() == ID_TOKEN)
                        {
                                idstr = tokenstr;
                                idlen = tokenlen;
                                idfunc_add(tokenstr, tokenlen, tag_new());
                                while (scan() == COMMA_TOKEN)
                                {
                                        if (scan() == ID_TOKEN)
                                        {
                                                idstr = tokenstr;
                                                idlen = tokenlen;
                                                idfunc_add(tokenstr, tokenlen, tag_new());
                                        }
                                        else
                                        {
                                                parse_error("Bad function pre-declaration");
                                                return (0);
                                        }
                                }
                        }
                        else
                        {
                                parse_error("Bad function pre-declaration");
                                return (0);
                        }
                case EOL_TOKEN:
                case COMMENT_TOKEN:
                        return (1);
                default:
                        return (0);
        }
        return (1);
}
int parse_func(void)
{
        char c;
        int func_tag, defopt, cfnparms;
        optimization(0);
        switch (scantoken)
        {
                case IFUNC_TOKEN:
                case NFUNC_TOKEN:
                        defopt = scantoken - IFUNC_TOKEN;
                        if (scan() != ID_TOKEN)
                        {
                                parse_error("Missing function name");
                                return (0);
                        }
                        cfnparms    = 0;
                        infunc      = 1;
                        if (idglobal_lookup(tokenstr, tokenlen) >= 0)
                                func_tag = id_addr(tokenstr, tokenlen);
                        else
                        {
                                func_tag = tag_new();
                                idfunc_add(tokenstr, tokenlen, func_tag);
                        }
                        c = tokenstr[tokenlen];
                        tokenstr[tokenlen] = '\0';
                        emit_idfunc(func_tag, tokenstr);
                        tokenstr[tokenlen] = c;
                        retfunc_tag = tag_new();
                        idlocal_reset();
                        localsize = 2;
                        if (scan() == OPEN_PAREN_TOKEN)
                        {
                                do
                                {
                                        if (scan() == ID_TOKEN)
                                        {
                                                cfnparms++;
                                                idlocal_add(tokenstr, tokenlen, WORD_TYPE, 2);
                                                scan();
                                        }
                                } while (scantoken == COMMA_TOKEN);
                                if (scantoken != CLOSE_PAREN_TOKEN)
                                {
                                        parse_error("Bad function parameter list");
                                        return (0);
                                }
                                scan();
                        }
                        while (parse_vars()) next_line();
                        emit_def(defopt);
                        emit_enter(localsize, cfnparms);
                        prevstmnt = 0;
                        while (parse_stmnt()) next_line();
                        infunc = 0;
                        if (scantoken != END_TOKEN)
                        {
                                parse_error("Syntax error");
                                return (0);
                        }
                        if (scan() != EOL_TOKEN && scantoken != COMMENT_TOKEN)
                        {
                                parse_error("Extraneous characters");
                                return (0);
                        }
                        if (prevstmnt != RETURN_TOKEN)
                                emit_leave(localsize);
                        return (1);
                case ASM_TOKEN:
                        if (scan() != ID_TOKEN)
                        {
                                parse_error("Missing function name");
                                return (0);
                        }
                        cfnparms    = 0;
                        infunc      = 1;
                        if (idglobal_lookup(tokenstr, tokenlen) >= 0)
                                func_tag = id_addr(tokenstr, tokenlen);
                        else
                        {
                                func_tag = tag_new();
                                idfunc_add(tokenstr, tokenlen, func_tag);
                        }
                        c = tokenstr[tokenlen];
                        tokenstr[tokenlen] = '\0';
                        emit_idfunc(func_tag, tokenstr);
                        tokenstr[tokenlen] = c;
                        retfunc_tag = tag_new();
                        idlocal_reset();
                        localsize = 2;
                        if (scan() == OPEN_PAREN_TOKEN)
                        {
                                do
                                {
                                        if (scan() == ID_TOKEN)
                                        {
                                                cfnparms++;
                                                idlocal_add(tokenstr, tokenlen, WORD_TYPE, 2);
                                                scan();
                                        }
                                } while (scantoken == COMMA_TOKEN);
                                if (scantoken != CLOSE_PAREN_TOKEN)
                                {
                                        parse_error("Bad function parameter list");
                                        return (0);
                                }
                                scan();
                        }
                        while (parse_vars()) next_line();
                        emit_def(1);
                        emit_enter(localsize, cfnparms);
                        prevstmnt = 0;
                        do
                        {
                                if (scantoken == EOL_TOKEN)
                                    next_line();
                                else if (scantoken != END_TOKEN)
                                {
                                    emit_asm(inputline);
                                    next_line();
                                }
                        } while (scantoken != END_TOKEN);
                        emit_leave(localsize);
                        infunc = 0;
                        return (1);
                case EOL_TOKEN:
                case COMMENT_TOKEN:
                        return (1);
                }
        return (0);
}
int parse_module(void)
{
        if (next_line())
        {
                while (parse_vars())  next_line();
                while (parse_func()) next_line();
                if (scantoken != DONE_TOKEN && scantoken != EOF_TOKEN)
                {
                        optimization(0);
                        emit_start();
                        prevstmnt = 0;
                        while (parse_stmnt())  next_line();
                        if (scantoken != DONE_TOKEN)
                                parse_error("Missing DONE statement");
			emit_ret();
                }
                return (1);
        }
        return (0);
}
int main(int argc, char **argv)
{
	int optc = 1;
	if (argc > optc && argv[optc][0] == '-')
	{
		emit_flags(EDASM);
		optc++;
	}
	if (argc > optc && argv[optc][0] == '$')
		optc++;
	else
		emit_header();
	if (parse_module())
	{
                fprintf(stderr, "Compilation complete.\n");
		emit_trailer();
	}
        return (0);
}
