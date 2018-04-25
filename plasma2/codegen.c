#include <stdio.h>
#include "tokens.h"
#include "codegen.h"

#define DEX         if (xreg < 1){printf("\tDEX\n");}else{xreg--;}
#define INX         if (xreg > 1){printf("\tINX\n");}else{xreg++;}
#define SYNC_X      while (xreg != 0){if (xreg > 0){printf("\tINX\n");xreg--;}else{printf("\tDEX\n");xreg++;}}
#define INVALID_Y   {y_valid = 0;}
#define VALID_Y     {y_valid = 1;}
#define VALIDATE_Y  if (!y_valid){printf("\tLDY\t#$00\n");y_valid=1;}

int xreg = 0;
int y_valid = 0;
int opt = 0;
int outflags = 0;

int optimization(int newopt)
{
    int oldopt = opt;
    opt = newopt ? 1 : 0;
    return (oldopt);
}
static int vsp_saved;
void emit_flags(int flags)
{
    outflags = flags;
}
void emit_header(void)
{
	if (outflags & EDASM)
		printf("; EDASM COMPATIBLE OUTPUT\n");
	else
		printf("\t.INCLUDE\t\"plstub.s\"\n");
}
void emit_trailer(void)
{
//        if (!(outflags & EDASM))
//                printf("\t.INCLUDE\t\"vmcore.s\"\n");
}
void emit_comment(char *s)
{
	if (!(outflags & EDASM))
		printf("\t\t\t\t\t; %s\n", s);
}
void emit_asm(char *s)
{
    printf("%s\n", s);
}
void emit_idlocal(char *name, int value)
{
	if (!(outflags & EDASM))
		printf("\t\t\t\t\t; %s = %d\n", name, value);
}
void emit_idglobal(int value, int size, char *name)
{
    if (size == 0)
	{
		if (outflags & EDASM)
			printf("D%04d: EQU * ; %s\n", value, name);
		else
			printf("D%04d:\t\t\t\t\t; %s\n", value, name);
	}
    else
	{
		if (outflags & EDASM)
			printf("D%04d: DS %d ; %s\n", value, size, name);
		else
			printf("D%04d:\tDS\t%d\t\t\t; %s\n", value, size, name);
	}
}
void emit_idfunc(int value, char *name)
{
	if (outflags & EDASM)
		printf("C%04d: EQU * ; %s()\n", value, name);
	else
		printf("C%04d:\t\t\t\t\t; %s()\n", value, name);
}
void emit_idconst(char *name, int value)
{
	if (!(outflags & EDASM))
		printf("\t\t\t\t\t; %s = %d\n", name, value);
}
int emit_data(int vartype, int consttype, long constval, int constsize)
{
    int datasize, i;
    char *str;
    if (consttype == 0)
    {
        datasize = constsize;
        printf("\tDS\t$%02X\n", constsize);
    }
    else if (consttype == STRING_TYPE)
    {
        datasize = constsize;
        str = (char *)constval;
        printf("\tDB\t$%02X\n", --constsize);
        while (constsize-- > 0)
        {
            printf("\tDB\t$%02X", *str++);
            for (i = 0; i < 7; i++)
            {
                if (constsize-- > 0)
                    printf(",$%02X", *str++);
                else
                    break;
            }
            printf("\n");
        }
    }
    else if (consttype == TAG_TYPE)
    {
        datasize = 2;
        if (constval & 0x8000)
            printf("\tDW\tC%04ld\t; C%04ld\n", constval & 0x7FFF, constval & 0x7FFF);
        else
            printf("\tDW\tD%04ld\t; D%04ld\n", constval, constval);
    }
    else
    {
        if (vartype == WORD_TYPE)
        {
            datasize = 2;
            printf("\tDW\t$%04lX\n", constval & 0xFFFF);
        }
        else
        {
            datasize = 1;
            printf("\tDB\t$%02lX\n", constval & 0xFF);
        }
    }
    return (datasize);
}
void emit_codetag(int tag)
{
    if (opt)
    {
        SYNC_X;
    }
    if (outflags & EDASM)
        printf("C%04d: EQU *\n", tag);
    else
        printf("C%04d:\n", tag);
    if (opt)
        INVALID_Y;
}
void emit_const(int cval)
{
    if (opt)
    {
        int lo = cval & 0xFF;
        int hi = (cval >> 8) & 0xFF;
        DEX;//printf("\tDEX\n");
        if (lo == 0)
        {
            VALIDATE_Y;
            printf("\tSTY\tESTKL,X\n");
        }
        else
        {
            printf("\tLDA\t#$%02X\n", lo);
            printf("\tSTA\tESTKL,X\n");
        }
        if (hi == 0)
        {
            VALIDATE_Y;
            printf("\tSTY\tESTKH,X\n");
        }
        else
        {
            if (hi != lo)
                printf("\tLDA\t#$%02X\n", hi);
            printf("\tSTA\tESTKH,X\n");
        }
    }
    else
    {
        if (cval == 0)
            printf("\tDB\t$00\t\t\t; ZERO\n");
        else if (cval > 0 && cval < 256)
            printf("\tDB\t$2A,$%02X\t\t\t; CB\t%d\n", cval, cval);
        else
            printf("\tDB\t$2C,$%02X,$%02X\t\t; CW\t%d\n", cval&0xFF,(cval>>8)&0xFF, cval);
    }
}
void emit_lb(void)
{
    if (opt)
    {
        VALIDATE_Y;
        SYNC_X;
        printf("\tJSR\tLB\n");
    }
    else
        printf("\tDB\t$60\t\t\t; LB\n");
}
void emit_lw(void)
{
    if (opt)
    {
        VALIDATE_Y;
        SYNC_X;
        printf("\tJSR\tLW\n");
	INVALID_Y;
    }
    else
        printf("\tDB\t$62\t\t\t; LW\n");
}
void emit_llb(int index)
{
    if (opt)
    {
        DEX;//printf("\tDEX\n");
        printf("\tLDY\t#$%02X\n", index);
        printf("\tLDA\t(FRMP),Y\n");
        printf("\tSTA\tESTKL,X\n");
        printf("\tLDY\t#$00\n");
        printf("\tSTY\tESTKH,X\n");
        VALID_Y;
    }
    else
        printf("\tDB\t$64,$%02X\t\t\t; LLB\t%d\n", index, index);
}
void emit_llw(int index)
{
        if (opt)
    {
        DEX;//printf("\tDEX\n");
        printf("\tLDY\t#$%02X\n", index);
        printf("\tLDA\t(FRMP),Y\n");
        printf("\tSTA\tESTKL,X\n");
        printf("\tINY\n");
        printf("\tLDA\t(FRMP),Y\n");
        printf("\tSTA\tESTKH,X\n");
        INVALID_Y;
    }
    else
        printf("\tDB\t$66,$%02X\t\t\t; LLW\t%d\n", index, index);
}
void emit_lab(int tag)
{
    if (opt)
    {
        DEX;//printf("\tDEX\n");
        printf("\tLDA\tD%04d\n", tag);
        printf("\tSTA\tESTKL,X\n");
        VALIDATE_Y;
        printf("\tSTY\tESTKH,X\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$68,>D%04d,<D%04d\t; LAB\tD%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$68,<D%04d,>D%04d\t; LAB\tD%04d\n", tag, tag, tag);
    }
}
void emit_law(int tag)
{
    if (opt)
    {
        DEX;//printf("\tDEX\n");
        printf("\tLDA\tD%04d\n",   tag);
        printf("\tSTA\tESTKL,X\n");
        printf("\tLDA\tD%04d+1\n", tag);
        printf("\tSTA\tESTKH,X\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$6A,>D%04d,<D%04d\t; LAW\tD%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$6A,<D%04d,>D%04d\t; LAW\tD%04d\n", tag, tag, tag);
    }
}
void emit_sb(void)
{
    if (opt)
    {
        VALIDATE_Y;
        SYNC_X;
        printf("\tJSR\tSB\n");
    }
    else
        printf("\tDB\t$70\t\t\t; SB\n");
}
void emit_sw(void)
{
    if (opt)
    {
        VALIDATE_Y;
        SYNC_X;
        printf("\tJSR\tSW\n");
	INVALID_Y;
    }
    else
        printf("\tDB\t$72\t\t\t; SW\n");
}
void emit_slb(int index)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDY\t#$%02X\n", index);
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\t(FRMP),Y\n");
        INX;//printf("\tINX\n");
        INVALID_Y;
    }
    else
        printf("\tDB\t$74,$%02X\t\t\t; SLB\t%d\n", index, index);
}
void emit_slw(int index)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDY\t#$%02X\n", index);
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\t(FRMP),Y\n");
        printf("\tINY\n");
        printf("\tLDA\tESTKH,X\n");
        printf("\tSTA\t(FRMP),Y\n");
        INX;//printf("\tINX\n");
        INVALID_Y;
    }
    else
        printf("\tDB\t$76,$%02X\t\t\t; SLW\t%d\n", index, index);
}
void emit_dlb(int index)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDY\t#$%02X\n", index);
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\t(FRMP),Y\n");
        INVALID_Y;
    }
    else
        printf("\tDB\t$6C,$%02X\t\t\t; DLB\t%d\n", index, index);
}
void emit_dlw(int index)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDY\t#$%02X\n", index);
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\t(FRMP),Y\n");
        printf("\tINY\n");
        printf("\tLDA\tESTKH,X\n");
        printf("\tSTA\t(FRMP),Y\n");
        INVALID_Y;
    }
    else
        printf("\tDB\t$6E,$%02X\t\t\t; DLW\t%d\n", index, index);
}
void emit_sab(int tag)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\tD%04d\n", tag);
        INX;//printf("\tINX\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$78,>D%04d,<D%04d\t; SAB\tD%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$78,<D%04d,>D%04d\t; SAB\tD%04d\n", tag, tag, tag);
    }
}
void emit_saw(int tag)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\tD%04d\n", tag);
        printf("\tLDA\tESTKH,X\n");
        printf("\tSTA\tD%04d+1\n", tag);
        INX;//printf("\tINX\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$7A,>D%04d,<D%04d\t; SAW\tD%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$7A,<D%04d,>D%04d\t; SAW\tD%04d\n", tag, tag, tag);
    }
}
void emit_dab(int tag)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\tD%04d\n", tag);
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$7C,>D%04d,<D%04d\t; DAB\tD%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$7C,<D%04d,>D%04d\t; DAB\tD%04d\n", tag, tag, tag);
    }
}
void emit_daw(int tag)
{
    if (opt)
    {
        SYNC_X;
        printf("\tLDA\tESTKL,X\n");
        printf("\tSTA\tD%04d\n", tag);
        printf("\tLDA\tESTKH,X\n");
        printf("\tSTA\tD%04d+1\n", tag);
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$7E,>D%04d,<D%04d\t; DAW\tD%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$7E,<D%04d,>D%04d\t; DAW\tD%04d\n", tag, tag, tag);
    }
}
void emit_localaddr(int index)
{
    if (opt)
    {
		VALIDATE_Y;
        DEX;//printf("\tDEX\n");
        printf("\tLDA\t#$%02X\n", index);
        printf("\tCLC\n");
        printf("\tADC\tFRMPL\n");
        printf("\tSTA\tESTKL,X\n");
        printf("\tTYA\n");
        printf("\tADC\tFRMPH\n");
        printf("\tSTA\tESTKH,X\n");
    }
    else
        printf("\tDB\t$28,$%02X\t\t\t; LLA\t%d\n", index, index);
}
void emit_globaladdr(int tag, int type)
{
    if (opt)
    {
        DEX;//printf("\tDEX\n");
        if (type & FUNC_TYPE)
        {
            if (outflags & EDASM)
                printf("\tLDA\t#>C%04d\n", tag);
            else
                printf("\tLDA\t#<C%04d\n", tag);
            printf("\tSTA\tESTKL,X\n");
            if (outflags & EDASM)
                printf("\tLDA\t#<C%04d\n", tag);
            else
                printf("\tLDA\t#>C%04d\n", tag);
            printf("\tSTA\tESTKH,X\n");
        }
        else
        {
            if (outflags & EDASM)
                printf("\tLDA\t#>D%04d\n", tag);
            else
                printf("\tLDA\t#<D%04d\n", tag);
            printf("\tSTA\tESTKL,X\n");
            if (outflags & EDASM)
                printf("\tLDA\t#<D%04d\n", tag);
            else
                printf("\tLDA\t#>D%04d\n", tag);
            printf("\tSTA\tESTKH,X\n");
        }
    }
    else
    {
        if (type & FUNC_TYPE)
        {
            if (outflags & EDASM)
                printf("\tDB\t$26,>C%04d,<C%04d\t; LA\tC%04d\n", tag, tag, tag);
            else
                printf("\tDB\t$26,<C%04d,>C%04d\t; LA\tC%04d\n", tag, tag, tag);
        }
        else
        {
            if (outflags & EDASM)
                printf("\tDB\t$26,>D%04d,<D%04d\t; LA\tD%04d\n", tag, tag, tag);
            else
                printf("\tDB\t$26,<D%04d,>D%04d\t; LA\tD%04d\n", tag, tag, tag);
        }
    }
}
void emit_globaladdrofst(int tag, int ofst, int type)
{
    if (opt)
    {
        DEX;//printf("\tDEX\n");
        if (type & FUNC_TYPE)
        {
            if (outflags & EDASM)
                printf("\tLDA\t#>C%04d+%d\n", tag, ofst);
            else
                printf("\tLDA\t#<C%04d+%d\n", tag, ofst);
            printf("\tSTA\tESTKL,X\n");
            if (outflags & EDASM)
                printf("\tLDA\t#<C%04d+%d\n", tag, ofst);
            else
                printf("\tLDA\t#>C%04d+%d\n", tag, ofst);
            printf("\tSTA\tESTKH,X\n");
        }
        else
        {
            if (outflags & EDASM)
                printf("\tLDA\t#>D%04d+%d\n", tag, ofst);
            else
                printf("\tLDA\t#<D%04d+%d\n", tag, ofst);
            printf("\tSTA\tESTKL,X\n");
            if (outflags & EDASM)
                printf("\tLDA\t#<D%04d+%d\n", tag, ofst);
            else
                printf("\tLDA\t#>D%04d+%d\n", tag, ofst);
            printf("\tSTA\tESTKH,X\n");
        }
    }
    else
    {
        if (type & FUNC_TYPE)
        {
            if (outflags & EDASM)
                printf("\tDB\t$26,>C%04d+%d,<C%04d+%d\t; LA\tC%04d+%d\n", tag, ofst, tag, ofst, tag, ofst);
            else
                printf("\tDB\t$26,<(C%04d+%d),>(C%04d+%d)\t; LA\tC%04d+%d\n", tag, ofst, tag, ofst, tag, ofst);
        }
        else
        {
            if (outflags & EDASM)
                printf("\tDB\t$26,>D%04d+%d,<D%04d+%d\t; LA\tD%04d+%d\n", tag, ofst, tag, ofst, tag, ofst);
            else
                printf("\tDB\t$26,<(D%04d+%d),>(D%04d+%d)\t; LA\tD%04d+%d\n", tag, ofst, tag, ofst, tag, ofst);
        }
    }
}
void emit_indexbyte(void)
{
    if (opt)
    {
        SYNC_X;
        printf("\tJSR\tADD\n");
    }
    else
        printf("\tDB\t$02\t\t\t; IDXB\n");
}
void emit_indexword(void)
{
    if (opt)
    {
        SYNC_X;
        printf("\tJSR\tIDXW\n");
    }
    else
        printf("\tDB\t$1E\t\t\t; IDXW\n");
}
void emit_skpfls(int tag)
{
    if (opt)
    {
        INX;//printf("\tINX\n");
        SYNC_X;
        printf("\tLDA\tESTKL-1,X\n");
        printf("\tORA\tESTKH-1,X\n");
        printf("\tBNE\t:+\n");
        printf("\tJMP\tC%04d\n", tag);
        printf(":\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$4C,>C%04d,<C%04d\t; SKPFLS\tC%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$4C,<C%04d,>C%04d\t; SKPFLS\tC%04d\n", tag, tag, tag);
    }
}
void emit_skptru(int tag)
{
    if (opt)
    {
        INX;//printf("\tINX\n");
        SYNC_X;
        printf("\tLDA\tESTKL-1,X\n");
        printf("\tORA\tESTKH-1,X\n");
        printf("\tBEQ\t:+\n");
        printf("\tJMP\tC%04d\n", tag);
        printf(":\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$4E,>C%04d,<C%04d\t; SKPTRU\tC%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$4E,<C%04d,>C%04d\t; SKPTRU\tC%04d\n", tag, tag, tag);
    }
}
void emit_skip(int tag)
{
    if (opt > 0)
    {
        SYNC_X;
        printf("\tJMP\tC%04d\n", tag);
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$50,>C%04d,<C%04d\t; SKIP\tC%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$50,<C%04d,>C%04d\t; SKIP\tC%04d\n", tag, tag, tag);
    }
}
void emit_skpeq(int tag)
{
    if (opt)
    {
        INX;//printf("\tINX\n");
        SYNC_X;
        printf("\tLDA\tESTKL-1,X\n");
        printf("\tCMP\tESTKL,X\n");
        printf("\tBNE\t:+\n");
        printf("\tLDA\tESTKH-1,X\n");
        printf("\tCMP\tESTKH,X\n");
        printf("\tBNE\t:+\n");
        printf("\tJMP\tC%04d\n", tag);
        printf(":\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$3C,>C%04d,<C%04d\t; SKPEQ\tC%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$3C,<C%04d,>C%04d\t; SKPEQ\tC%04d\n", tag, tag, tag);
    }
}
void emit_skpne(int tag)
{
    if (opt)
    {
        INX;//printf("\tINX\n");
        SYNC_X;
        printf("\tLDA\tESTKL-1,X\n");
        printf("\tCMP\tESTKL,X\n");
        printf("\tBNE\t:+\n");
        printf("\tLDA\tESTKH-1,X\n");
        printf("\tCMP\tESTKH,X\n");
        printf("\tBEQ\t:++\n");
        printf(":\tJMP\tC%04d\n", tag);
        printf(":\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$3E,>C%04d,<C%04d\t; SKPNE\tC%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$3E,<C%04d,>C%04d\t; SKPNE\tC%04d\n", tag, tag, tag);
    }
}
void emit_skplt(int tag)
{
    if (opt)
    {
        INX;//printf("\tINX\n");
        SYNC_X;
        printf("\tLDA\tESTKL,X\n");
        printf("\tCMP\tESTKL-1,X\n");
        printf("\tLDA\tESTKH,X\n");
        printf("\tSBC\tESTKH-1,X\n");
        printf("\tBPL\t:+\n");
        printf("\tJMP\tC%04d\n", tag);
        printf(":\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$38,>C%04d,<C%04d\t; SKPLT\tC%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$38,<C%04d,>C%04d\t; SKPLT\tC%04d\n", tag, tag, tag);
    }
}
void emit_skpgt(int tag)
{
    if (opt)
    {
        INX;//printf("\tINX\n");
        SYNC_X;
        printf("\tLDA\tESTKL-1,X\n");
        printf("\tCMP\tESTKL,X\n");
        printf("\tLDA\tESTKH-1,X\n");
        printf("\tSBC\tESTKH,X\n");
        printf("\tBPL\t:+\n");
        printf("\tJMP\tC%04d\n", tag);
        printf(":\n");
    }
    else
    {
        if (outflags & EDASM)
            printf("\tDB\t$3A,>C%04d,<C%04d\t; SKPGT\tC%04d\n", tag, tag, tag);
        else
            printf("\tDB\t$3A,<C%04d,>C%04d\t; SKPGT\tC%04d\n", tag, tag, tag);
    }
}
void emit_call(int tag)
{
    if (opt)
    {
        SYNC_X;
        printf("\tJSR\tC%04d\n", tag);
        INVALID_Y;
    }
    else
    {
        if (outflags & EDASM)
            (tag & 0x8000) ? printf("\tDB\t$54,>D%04d,<D%04d\t; CALL\tD%04d\n", tag&0x7FFF, tag&0x7FFF, tag&0x7FFF)
                           : printf("\tDB\t$54,>C%04d,<C%04d\t; CALL\tC%04d\n", tag, tag, tag);
        else
            (tag & 0x8000) ? printf("\tDB\t$54,<D%04d,>D%04d\t; CALL\tD%04d\n", tag&0x7FFF, tag&0x7FFF, tag&0x7FFF)
                           : printf("\tDB\t$54,<C%04d,>C%04d\t; CALL\tC%04d\n", tag, tag, tag);
    }
}
void emit_ical(void)
{
    if (opt)
    {
        SYNC_X;
        printf("\tJSR\tICAL\n");
        INVALID_Y;
    }
    else
        printf("\tDB\t$56\t\t\t; ICAL\n");
}
void emit_leave(int framesize)
{
    if (framesize > 2)
    {
        if (opt)
        {
            SYNC_X;
            printf("\tJMP\tLEAVE\n");
        }
        else
            printf("\tDB\t$5A\t\t\t; LEAVE\n");
    }
    else
    {
        if (opt)
            printf("\tRTS\n");
        else
            printf("\tDB\t$5C\t\t\t; RET\n");
    }
}
void emit_ret(void)
{
	if (opt)
	{
        SYNC_X;
		printf("\tRTS\n");
	}
	else
		printf("\tDB\t$5C\t\t\t; RET\n");
}
void emit_def(int defopt)
{
    opt = defopt;
    if (!opt)
        printf("\tJSR\t_INTERP\n");
}
void emit_enter(int framesize, int cparams)
{
    if (framesize > 2)
    {
        if (opt)
        {
            printf("\tLDY\t#%d\n", framesize);
            printf("\tLDA\t#%d\n", cparams);
            printf("\tJSR\tENTER\n");
            VALID_Y;
        }
        else
            printf("\tDB\t$58,$%02X,$%02X\t\t; ENTER\t%d,%d\n", framesize, cparams, framesize, cparams);
    }
}
void emit_start(void)
{
    printf("START:\t; JSR\tINTERP\n");
}
void emit_dup(void)
{
    if (opt)
    {
        DEX;//printf("\tDEX\n");
        printf("\tLDA\tESTKL+1,X\n");
        printf("\tSTA\tESTKL,X\n");
        printf("\tLDA\tESTKH+1,X\n");
        printf("\tSTA\tESTKH,X\n");
    }
    else
        printf("\tDB\t$32\t\t\t; DUP\n");
}
void emit_push(void)
{
        if (opt)
    {
        printf("\tLDA\tESTKL,X\n");
        printf("\tPHA\n");
        printf("\tLDA\tESTKH,X\n");
        printf("\tPHA\n");
        INX;//printf("\tINX\n");
    }
    else
        printf("\tDB\t$34\t\t\t; PUSH\n");
}
void emit_pull(void)
{
        if (opt)
    {
        DEX;//printf("\tDEX\n");
        printf("\tPLA\n");
        printf("\tSTA\tESTKH,X\n");
        printf("\tPLA\n");
        printf("\tSTA\tESTKL,X\n");
    }
    else
        printf("\tDB\t$36\t\t\t; PULL\n");
}
void emit_swap(void)
{
    if (opt)
    {
        SYNC_X;
        printf("\tJSR\tSWAP\n");
        INVALID_Y;
    }
    else
        printf("\tDB\t$2E\t\t\t; SWAP\n");
}
void emit_drop(void)
{
    if (opt)
    {
        INX;//printf("\tINX\t\t\t\t; DROP\n");
    }
    else
        printf("\tDB\t$30\t\t\t; DROP\n");
}
int emit_unaryop(int op)
{
    if (opt)
        SYNC_X;
    switch (op)
    {
        case NEG_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tNEG\n");
            }
            else
                printf("\tDB\t$10\t\t\t; NEG\n");
            break;
        case COMP_TOKEN:
            if (opt)
                printf("\tJSR\tCOMP\n");
            else
                printf("\tDB\t$12\t\t\t; COMP\n");
            break;
        case LOGIC_NOT_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tNOT\n");
            }
            else
                printf("\tDB\t$20\t\t\t; NOT\n");
            break;
        case INC_TOKEN:
            if (opt)
            {
                printf("\tINC\tESTKL,X\n");
                printf("\tBNE\t:+\n");
                printf("\tINC\tESTKH,X\n");
                printf(":\n");
            }
            else
                printf("\tDB\t$0C\t\t\t; INCR\n");
            break;
        case DEC_TOKEN:
            if (opt)
            {
                printf("\tLDA\tESTKL,X\n");
                printf("\tBNE\t:+\n");
                printf("\tDEC\tESTKH,X\n");
                printf(":\tDEC\tESTKL,X\n");
            }
            else
                printf("\tDB\t$0E\t\t\t; DECR\n");
            break;
        case BPTR_TOKEN:
            emit_lb();
            break;
        case WPTR_TOKEN:
            emit_lw();
            break;
        default:
            printf("emit_unaryop(%c) ? \n", op  & 0x7F);
            return (0);
    }
    return (1);
}
int emit_op(t_token op)
{
    if (opt)
        SYNC_X;
    switch (op)
    {
        case MUL_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tMUL\n");
            }
            else
                printf("\tDB\t$06\t\t\t; MUL\n");
            break;
        case DIV_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tDIV\n");
		INVALID_Y;
            }
            else
                printf("\tDB\t$08\t\t\t; DIV\n");
            break;
        case DIVMOD_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tDIVMOD\n");
		INVALID_Y;
            }
            else
                printf("\tDB\t$0A\t\t\t; DIV,MOD\n");
            break;
        case ADD_TOKEN:
            if (opt)
			{
                printf("\tJSR\tADD\n");
//                printf("\tLDA\tESTKL,X\n");
//                printf("\tCLC\n");
//                printf("\tADC\tESTKL+1,X\n");
//                printf("\tSTA\tESTKL+1,X\n");
//                printf("\tLDA\tESTKH,X\n");
//                printf("\tADC\tESTKH+1,X\n");
//                printf("\tSTA\tESTKH+1,X\n");
//				INX;
			}
            else
                printf("\tDB\t$02\t\t\t; ADD\n");
            break;
        case SUB_TOKEN:
            if (opt)
			{
                printf("\tJSR\tSUB\n");
//                printf("\tLDA\tESTKL+1,X\n");
//                printf("\tSEC\n");
//                printf("\tSBC\tESTKL,X\n");
//                printf("\tSTA\tESTKL+1,X\n");
//                printf("\tLDA\tESTKH+1,X\n");
//                printf("\tSBC\tESTKH,X\n");
//                printf("\tSTA\tESTKH+1,X\n");
//				INX;
			}
            else
                printf("\tDB\t$04\t\t\t; SUB\n");
            break;
        case SHL_TOKEN:
            if (opt)
            {
                printf("\tJSR\tSHL\n");
                VALID_Y;
            }
            else
                printf("\tDB\t$1A\t\t\t; SHL\n");
            break;
        case SHR_TOKEN:
            if (opt)
            {
                printf("\tJSR\tSHR\n");
                VALID_Y;
            }
            else
                printf("\tDB\t$1C\t\t\t; SHR\n");
            break;
        case AND_TOKEN:
            if (opt)
                printf("\tJSR\tBAND\n");
            else
                printf("\tDB\t$14\t\t\t; BAND\n");
            break;
        case OR_TOKEN:
            if (opt)
                printf("\tJSR\tIOR\n");
            else
                printf("\tDB\t$16\t\t\t; IOR\n");
            break;
        case EOR_TOKEN:
            if (opt)
                printf("\tJSR\tXOR\n");
            else
                printf("\tDB\t$18\t\t\t; XOR\n");
            break;
        case EQ_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tISEQ\n");
				INVALID_Y;
            }
            else
                printf("\tDB\t$40\t\t\t; ISEQ\n");
            break;
        case NE_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tISNE\n");
				INVALID_Y;
            }
            else
                printf("\tDB\t$42\t\t\t; ISNE\n");
            break;
        case GE_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tISGE\n");
				INVALID_Y;
            }
            else
                printf("\tDB\t$48\t\t\t; ISGE\n");
            break;
        case LT_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tISLT\n");
				INVALID_Y;
            }
            else
                printf("\tDB\t$46\t\t\t; ISLT\n");
            break;
        case GT_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tISGT\n");
				INVALID_Y;
            }
            else
                printf("\tDB\t$44\t\t\t; ISGT\n");
            break;
        case LE_TOKEN:
            if (opt)
            {
                VALIDATE_Y;
                printf("\tJSR\tISLE\n");
				INVALID_Y;
            }
            else
                printf("\tDB\t$4A\t\t\t; ISLE\n");
            break;
        case LOGIC_OR_TOKEN:
            if (opt)
                printf("\tJSR\tLOR\n");
            else
                printf("\tDB\t$22\t\t\t; LOR\n");
            break;
        case LOGIC_AND_TOKEN:
            if (opt)
                printf("\tJSR\tLAND\n");
            else
                printf("\tDB\t$24\t\t\t; LAND\n");
            break;
        case COMMA_TOKEN:
            break;
        default:
            return (0);
    }
    return (1);
}
