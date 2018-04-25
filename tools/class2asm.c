#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

char *opcode[256] = {  "nop", "aconst_null", "iconst_m1", "iconst_0",
                        "iconst_1", "iconst_2", "iconst_3", "iconst_4",
                        "iconst_5", "lconst_0", "lconst_1", "fconst_0",
                        "fconst_1", "fconst_2", "dconst_0", "dconst_1",
                        "bipush", "sipush", "ldc", "ldc_w",
                        "ldc2_w", "iload", "lload", "fload",
                        "dload", "aload", "iload_0", "iload_1",
                        "iload_2", "iload_3", "lload_0", "lload_1",
                        "lload_2", "lload_3", "fload_0", "fload_1",
                        "fload_2", "fload_3", "dload_0", "dload_1",
                        "dload_2", "dload_3", "aload_0", "aload_1",
                        "aload_2", "aload_3", "iaload", "laload",
                        "faload", "daload", "aaload", "baload",
                        "caload", "saload", "istore", "lstore",
                        "fstore", "dstore", "astore", "istore_0",
                        "istore_1", "istore_2", "istore_3", "lstore_0",
                        "lstore_1", "lstore_2", "lstore_3", "fstore_0",
                        "fstore_1", "fstore_2", "fstore_3", "dstore_0",
                        "dstore_1", "dstore_2", "dstore_3", "astore_0",
                        "astore_1", "astore_2", "astore_3", "iastore",
                        "lastore", "fastore", "dastore", "aastore",
                        "bastore", "castore", "sastore", "pop",
                        "pop2", "dup", "dup_x1", "dup_x2",
                        "dup2", "dup2_x1", "dup2_x2", "swap",
                        "iadd", "ladd", "fadd", "dadd",
                        "isub", "lsub", "fsub", "dsub", 
                        "imul", "lmul", "fmul", "dmul",
                        "idiv", "ldiv", "fdiv", "ddiv",
                        "irem", "lrem", "frem", "drem",
                        "ineg", "lneg", "fneg", "dneg",
                        "ishl", "lshl", "ishr", "lshr",
                        "iushr", "lushr", "iand", "land",
                        "ior", "lor", "ixor", "lxor",
                        "iinc", "i2l", "i2f", "i2d",
                        "l2i", "l2f", "l2d", "f2i",
                        "f2l", "f2d", "d2i", "d2l",
                        "d2f", "i2b", "i2c", "i2s",
                        "lcmp", "fcmpl", "fcmpg", "dcmpl",
                        "dcmpg", "ifeq", "ifne", "iflt",
                        "ifge", "ifgt", "ifle", "if_icmpeq",
                        "if_icmpne", "if_icmplt", "if_icmpge", "if_icmpgt",
                        "if_icmple", "if_acmpeq", "if_acmpne", "goto",
                        "jsr", "ret", "tableswitch", "lookupswitch",
                        "ireturn", "lreturn", "freturn", "dreturn",
                        "areturn", "return", "getstatic", "putstatic",
                        "getfield", "putfield", "invokevirtual", "invokespecial",
                        "invokestatic", "invokeinterface", "xxx_unused_xxx", "new",
                        "newarray", "anewarray", "arraylength", "athrow",
                        "checkcast", "instanceof", "monitorenter", "monitorexit",
                        "wide", "multianewarray", "ifnull", "ifnonnull",
                        "goto_w", "jsr_w", "breakpoint", "XXX",
                        "XXX","XXX", "XXX", "XXX",
                        "XXX","XXX", "XXX", "XXX", "XXX", "XXX", "XXX", "XXX",
                        "XXX","XXX", "XXX", "XXX", "XXX", "XXX", "XXX", "XXX",
                        "XXX","XXX", "XXX", "XXX", "XXX", "XXX", "XXX", "XXX",
                        "XXX","XXX", "XXX", "XXX", "XXX", "XXX", "XXX", "XXX",
                        "XXX","XXX", "XXX", "XXX", "XXX", "XXX", "XXX", "XXX",
                        "XXX","XXX", "XXX", "XXX", "XXX", "XXX", "impdep1", "impdep2"
                   };
char *atype[] = {       "???", "???", "???", "???",
                        "T_BOOLEAN", "T_CHAR", "T_FLOAT", "T_DOUBLE",
                        "T_BYTE", "T_SHORT", "T_INT", "T_LONG"
                    };
char *as65str[] = {
                        "\t.BYTE\t",
                        "\t.ORG\t",
                        "\t.BYTE\t",
                        ";*",
                        ":",
                        "\t",
                        "\"",
                        "(",
                        ")"
                    };
char *merlinstr[] = {
                        " DFB ",
                        " ORG ",
                        " ASC ",
                        "*",
                        "",
                        " ",
                        "\'",
                        "",
                        ""
                    };
#define DFB  0
#define ORG  1
#define ASC  2
#define CMNT 3
#define LABL 4
#define TAB  5
#define QUOT 6
#define OPAREN 7
#define CPAREN 8
char **asmstring = NULL;
	
#define TO_32(b)    ((b[0]<<24)|(b[1]<<16)|(b[2]<<8)|(b[3]))
#define TO_16(b)    ((b[0]<<8)|(b[1]))

int CodeAttrName, ConstantAttrName, ExceptAttrName, InnerClassAttrName,
    SyntheticAttrName, SourceAttrName, LineNumAttrName, LocalVarAttrName;
unsigned char buf[256];

void DumpAttr(int fd, int name, int len, char blktype, int blknum, int attrnum)
{
    char attrbgn[10], attrend[10];
    sprintf(attrbgn, "%c%dA%dBGN", blktype, blknum, attrnum);
    sprintf(attrend, "%c%dA%dEND", blktype, blknum, attrnum);
    printf("%s$%02X,$%02X%s; NAME #%d\n", asmstring[DFB], (name >> 8) & 0xFF, name & 0xFF, asmstring[TAB], name);
    printf("%s$%02X,$%02X,>%s%s-%s%s,<%s%s-%s%s\n", asmstring[DFB], (len >> 24) & 0xFF,(len >> 16) & 0xFF, asmstring[OPAREN], attrend, attrbgn, asmstring[CPAREN], asmstring[OPAREN], attrend, attrbgn, asmstring[CPAREN]);
    printf("%s%s\n", attrbgn, asmstring[LABL]);
    if (name == CodeAttrName)
    {
        unsigned short max_stack, max_locals, cnt, val;
        signed short sval;
        int ival, defval, lval, hval, pad;
        unsigned long code_len, pc;
        char opstr[40], codebgn[10], codeend[10];
        
        printf("%s CODE:\n", asmstring[CMNT]);
        sprintf(codebgn, "%c%dC%dBGN", blktype, blknum, attrnum);
        sprintf(codeend, "%c%dC%dEND", blktype, blknum, attrnum);
        read(fd, buf, 2); max_stack = TO_16(buf);
        printf("%s$%02X,$%02X%s; MAX STACK %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], max_stack);
        read(fd, buf, 2); max_locals = TO_16(buf);
        printf("%s$%02X,$%02X%s; MAX LOCALS %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], max_locals);
        read(fd, buf, 4); code_len = TO_32(buf);
        printf("%s$%02X,$%02X,>%s%s-%s%s,<%s%s-%s%s\n", asmstring[DFB], (len >> 24) & 0xFF,(len >> 16) & 0xFF, asmstring[OPAREN], codeend, codebgn, asmstring[CPAREN], asmstring[OPAREN], codeend, codebgn, asmstring[CPAREN]);
        printf("%s%s\n", codebgn, asmstring[LABL]);
        
        for (pc = 0; pc < code_len; pc++)
        {
            read(fd, buf, 1);
            sprintf(opstr, "%05lu: %s", pc, opcode[buf[0]]);
            printf("%s$%02X", asmstring[DFB], buf[0]);
            switch (buf[0])
            {
                case 0x19: // local var index8
                case 0x3A:
                case 0x18:
                case 0x39:
                case 0x17:
                case 0x38:
                case 0x84:
                case 0x15:
                case 0x36:
                case 0x16:
                case 0x37:
                case 0xA9:
                    read(fd, buf+1, 1);
                    printf(",$%02X", buf[1]);
                    sprintf(opstr, "%s %u", opstr, buf[1]);
                    if (buf[0] == 0x84) // , const
                    {
                        read(fd, buf, 1);
                        printf(",$%02X", buf[0]);
                        sprintf(opstr, "%s %d", opstr, (signed char)buf[0]);
                        pc++;
                    }
                    pc++;
                    break;
                case 0xC4: // wide local var index16
                    read(fd, buf, 1);
                    if (buf[0] == 0x84)
                    {
                        read(fd, buf, 2); val = TO_16(buf);
                        printf(",$%02X,$%02X", buf[0], buf[1]);
                        sprintf(opstr, "%s iinc %u", opstr, val);            
                        read(fd, buf, 2); val = TO_16(buf);
                        printf(",$%02X,$%02X", buf[0], buf[1]);
                        sprintf(opstr, "%s %d", opstr, val);            
                        pc += 5;
                    }
                    else
                    {
                        sprintf(opstr, "%s %s", opstr, opcode[buf[0]]);            
                        read(fd, buf, 2); val = TO_16(buf);
                        printf(",$%02X,$%02X", buf[0], buf[1]);
                        sprintf(opstr, "%s %u", opstr, val);            
                        pc += 3;
                    }
                    break;
                case 0x12: // class const pool index8
                    read(fd, buf+1, 1);
                    printf(",$%02X", buf[1]);
                    sprintf(opstr, "%s #%u", opstr, buf[1]);
                    pc++;
                    break;
                case 0xBD: // class const pool index16
                case 0xC0:
                case 0xB4:
                case 0xB2:
                case 0xC1:
                case 0xB7:
                case 0xB8:
                case 0xB6:
                case 0x13:
                case 0x14:
                case 0xBB:
                case 0xB5:
                case 0xB3:
                case 0xB9:
                case 0xC5:
                    buf[2] = buf[0];
                    read(fd, buf, 2); val = TO_16(buf);
                    printf(",$%02X,$%02X", buf[0], buf[1]);
                    sprintf(opstr, "%s #%u", opstr, val);            
                    if (buf[2] == 0xC5) // , dimensions
                    {
                        read(fd, buf, 1);
                        printf(",$%02X", buf[0]);
                        sprintf(opstr, "%s %d", opstr, buf[0]);
                        pc++;
                    }
                   if (buf[2] == 0xB9) // , count, 0
                    {
                        read(fd, buf, 2);
                        printf(",$%02X,$%02X", buf[0], buf[1]);
                        sprintf(opstr, "%s %d",  opstr, buf[0]);
                        pc += 2;
                    }
                    pc += 2;
                    break;
                case 0x10: // byte
                    read(fd, buf, 1);
                    printf(",$%02X", buf[0]);
                    sprintf(opstr, "%s %d", opstr, buf[0]);            
                    pc++;
                    break;
                case 0xBC: // atype
                    read(fd, buf, 1);
                    sprintf(opstr, "%s %d (%s)", opstr, buf[0], atype[buf[0]]);            
                    pc++;
                    break;
                case 0x11: // short
                    read(fd, buf, 2); val = TO_16(buf);
                    printf(",$%02X,$%02X", buf[0], buf[1]);
                    sprintf(opstr, "%s %d", opstr, val);            
                    pc += 2;
                    break;
                case 0xA7: // branch16
                case 0xA5:
                case 0xA6:
                case 0x9F:
                case 0xA0:
                case 0xA1:
                case 0xA2:
                case 0xA3:
                case 0xA4:
                case 0x99:
                case 0x9A:
                case 0x9B:
                case 0x9C:
                case 0x9D:
                case 0x9E:
                case 0xC7:
                case 0xC6:
                case 0xA8:
                    read(fd, buf, 2); sval = TO_16(buf);
                    printf(",$%02X,$%02X", buf[0], buf[1]);
                    sprintf(opstr, "%s %+d (%06lu)", opstr, sval, pc + sval);            
                    pc += 2;
                    break;
                case 0xC9: // branch32
                case 0xC8:
                    read(fd, buf, 4); ival = TO_32(buf);
                    printf(",$%02X,$%02X,$%02X,$%02X", buf[0], buf[1], buf[2], buf[3]);
                    sprintf(opstr, "%s %+d (%06lu)", opstr, ival, pc + ival);            
                    pc += 4;
                    break;
                case 0xAB: // lookupswitch
                    pad = ((pc + 1) & 0x03);
                    if (pad)
                    {
                        pad = 4 - pad;
                        read(fd, buf, pad); // padding
                        for (cnt=0; cnt < pad; cnt++)
                         printf(",$00");
                    }
                    read(fd, buf, 4); defval = TO_32(buf); // default
                    printf("%s$%02X,$%02X,$%02X,$%02X%s; DEFAULT %+d (%06lu)", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], defval, pc + defval);
                    read(fd, buf, 4); hval = TO_32(buf); // num pairs
                    printf("%s$%02X,$%02X,$%02X,$%02X%s; NUM PAIRS %d", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], hval);
                    for (cnt = 0; cnt < hval; cnt++)
                    {
                        read(fd, buf, 4); ival = TO_32(buf); // match
                        printf("%s$%02X,$%02X,$%02X,$%02X%s; MATCH %d", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], ival);
                        read(fd, buf, 4); lval = TO_32(buf); // offsets
                        printf("%s$%02X,$%02X,$%02X,$%02X%s; OFFSET %d: %+d (%06lu)\n", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], ival, lval, pc + lval);
                    }
                    pc += pad + 8 + 8 * hval;
                    opstr[0] = '\0';
                    break;
                case 0xAA: // tableswitch
                    pad = ((pc + 1) & 0x03);
                    if (pad)
                    {
                        pad = 4 - pad;
                        read(fd, buf, pad); // padding
                        for (cnt=0; cnt < pad; cnt++)
                         printf(",$00");
                    }
                 printf("%s; %s\n", asmstring[TAB], opstr);
                    read(fd, buf, 4); defval = TO_32(buf); // default
                    printf("%s$%02X,$%02X,$%02X,$%02X%s; DEFAULT %+d (%06lu)", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], defval, pc + defval);
                    read(fd, buf, 4); lval = TO_32(buf); // low value
                    printf("%s$%02X,$%02X,$%02X,$%02X%s; LO VAL %d", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], lval);
                    read(fd, buf, 4); hval = TO_32(buf); // high value
                    printf("%s$%02X,$%02X,$%02X,$%02X%s; HI VAL %d", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], hval);
                    for (cnt = lval; cnt <= hval; cnt++)
                    {
                        read(fd, buf, 4); ival = TO_32(buf); // offsets
                        printf("%s$%02X,$%02X,$%02X,$%02X%s; %d: %+d (%06u)\n", asmstring[DFB], buf[0], buf[1], buf[2], buf[3], asmstring[TAB], hval, cnt, ival, pc + ival);
                    }
                    pc += pad + 12 + 4 * (hval - lval + 1);
                    opstr[0] = '\0';
                    break;
            }
            if (opstr[0])
             printf("%s; %s\n", asmstring[TAB], opstr);
        }
        printf("%s%s\n", codeend, asmstring[LABL]);
        read(fd, buf, 2); cnt = TO_16(buf);
        printf("%s EXCEPTION TABLE\n", asmstring[CMNT]);
        printf("%s$%02X,$%02X%s; COUNT %d\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], cnt);
        while (cnt--)
        {
            read(fd, buf, 2); val = TO_16(buf);
            printf("%s$%02X,$%02X%s; FROM    %05u\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], val);
            read(fd, buf, 2); val = TO_16(buf);
            printf("%s$%02X,$%02X%s; TO      %05u\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], val);
            read(fd, buf, 2); val = TO_16(buf);
            printf("%s$%02X,$%02X%s; HANDLER %05u\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], val);
            read(fd, buf, 2); val = TO_16(buf);
            printf("%s$%02X,$%02X%s; TYPE    %d\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], val);
        }
        printf("%s CODE ATTRIB\n", asmstring[CMNT]);
        read(fd, buf, 2); cnt = TO_16(buf);
        printf("%s$%02X,$%02X%s; ATTRIB COUNT %d\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], cnt);
        while (cnt--)
        {
            read(fd, buf, 2); name = TO_16(buf);                
            read(fd, buf, 4); len = TO_32(buf);
            DumpAttr(fd, name, len, 'C', blknum, cnt);
        }
    }
    else if (name == ConstantAttrName)
    {
        unsigned short idx;
        if (len != 2)
        {
            fprintf(stderr, "\nError: Bad attribute length for ConstantValue.\n");
            exit(1);
        }
        read(fd, buf, 2); idx = TO_16(buf);
        printf("%s$%02X,$%02X%s; CONST ATTR NAME #%d\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], idx);
    }
    else if (name == ExceptAttrName)
    {
        unsigned short except_cnt, except_idx;
        
        read(fd, buf, 2); except_cnt = TO_16(buf);                
        printf("%s$%02X,$%02X%s; EXCEPTIONS COUNT %d\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], except_cnt);
        while (except_cnt--)
        {
            read(fd, buf, 2); except_idx = TO_16(buf);                
            printf("%s$%02X,$%02X%s; EXCEPTIONS INDEX %d\n", asmstring[DFB], buf[0], buf[1], asmstring[TAB], except_idx);
        }
    }
    else
    {
        if (name == InnerClassAttrName)
            printf("%s INNER CLASSES: REMOVEABLE\n", asmstring[CMNT]);
        else if (name == SyntheticAttrName)
            printf("%s SYNTHETIC: REMOVEABLE\n", asmstring[CMNT]);
        else if (name ==  SourceAttrName)
            printf("%s SOURCE FILE: REMOVEABLE\n", asmstring[CMNT]);
        else if (name == LineNumAttrName)
            printf("%s LINE NUMBER TABLE: REMOVEABLE\n", asmstring[CMNT]);
        else if (name == LocalVarAttrName)
            printf("%s LOCAL VARIABLE TABLE: REMOVEABLE\n", asmstring[CMNT]);
        else
        while (len > 8)
        {
            read(fd, buf, 8);
            len -= 8;
            printf("%s$%02X,$%02X,$%02X,$%02X,$%02X,$%02X,$%02X,$%02X\n", asmstring[DFB], buf[0],buf[1],buf[2],buf[3],buf[4],buf[5],buf[6],buf[7]);
        }
        if (len)
        {
            printf("%s", asmstring[DFB]);
            while (len--)
            {
                read(fd, buf, 1);
                printf("$%02X", buf[0]);
	if (len)
	     printf(",");
            }
            printf("\n");
        }
    }
    printf("%s%s\n", attrend, asmstring[LABL]);
}

int main(int argc, char **argv)
{
    int cf, fd, cc, ii, cnt, idx;
    long long long_val;
    float float_val;
    double double_val;
    unsigned int magic;
    unsigned short minor_version;
    unsigned short major_version;
    unsigned short const_pool_count, iface_count, fields_count, methods_count, attribs_count;
    unsigned short access_flags;
    unsigned short this_class, super_class;
        
    if (argc < 3)
    {
        fprintf(stderr, "Usage: %s <prodos|ca65> <classfile1> [classfile2] ...\n", argv[0]);
        return (1);
    }
    if (*argv[1] == 'p' || *argv[1] == 'P')
    	asmstring = merlinstr;                    
    if (*argv[1] == 'c' || *argv[1] == 'C')
    	asmstring = as65str;                    
    if (!asmstring)
    {
        fprintf(stderr, "Must select <P>rodos or <C>a65 output format\n");
        return (1);
    }
    for (cf = 2; cf < argc; cf++)
    {
        if ((fd = open(argv[cf], O_RDONLY, 0)) > 0)
        {
            printf("%s\n%s CLASS FILE %s\n%s\n", asmstring[CMNT], asmstring[CMNT], argv[cf], asmstring[CMNT]);
            printf("%s$1000%s; DUMMY ADDRESS\n", asmstring[ORG], asmstring[TAB]);
            read(fd, buf, 4); // magic = TO_32(buf);
            printf("%s$%02X,$%02X,$%02X,$%02X%s; MAGIC\n", asmstring[DFB], buf[0],buf[1],buf[2],buf[3], asmstring[TAB]);
            read(fd, buf, 2); minor_version = TO_16(buf);
            printf("%s$%02X,$%02X%s; MINOR %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], minor_version);
            read(fd, buf, 2); major_version = TO_16(buf);
            printf("%s$%02X,$%02X%s; MAJOR %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], major_version);
            read(fd, buf, 2); const_pool_count = TO_16(buf);
            printf("%s\n%s CONSTANT POOL\n%s\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT]);
            printf("%s$%02X,$%02X%s; CONST POOL COUNT %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], const_pool_count);
            for (cc = 1; cc < const_pool_count; cc++)
            {
             printf("%s CONST POOL INDEX %d\n", asmstring[CMNT], cc);
                read(fd, buf, 1);
                switch (buf[0])
                {
                    case 0: // BAD
                        printf("Bad tag 0");
                        read(fd, buf, 1);
                        exit(1);
                        break;
                    case 1: // CONSTANT_Utf8
                        printf("%s$01%s; UTF8\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; STRLEN\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB]);
                        read(fd, buf, idx);
                        buf[idx] = '\0';
                        printf("%s%s%s%s\n", asmstring[ASC], asmstring[QUOT], buf, asmstring[QUOT]);
                        if (strcmp(buf, "Code") == 0)
                            CodeAttrName = cc;
                        else if (strcmp(buf, "ConstantValue") == 0)
                            ConstantAttrName = cc;
                        else if (strcmp(buf, "Exceptions") == 0)
                            ExceptAttrName = cc;
                        else if (strcmp(buf, "InnerClasses") == 0)
                            InnerClassAttrName = cc;
                        else if (strcmp(buf, "Synthetic") == 0)
                            SyntheticAttrName = cc;
                        else if (strcmp(buf, "SourceFile") == 0)
                            SourceAttrName = cc;
                        else if (strcmp(buf, "LineNumberTable") == 0)
                            LineNumAttrName = cc;
                        else if (strcmp(buf, "LocalVariableTable") == 0)
                            LocalVarAttrName = cc;
                        break;
                    case 3: // CONSTANT_Integer
                        printf("%s$03%s; INT\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 4); idx = TO_32(buf);
                        printf("%s$%02X,$%02X,$%02X,$%02X%s; %d\n", asmstring[DFB], buf[0],buf[1],buf[2],buf[3], asmstring[TAB], idx);
                        break;
                    case 4: // CONSTANT_Float
                        printf("%s$04%s; FLOAT\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 4); idx = TO_32(buf);
                        float_val = *(float *)(&idx);
                        printf("%s$%02X,$%02X,$%02X,$%02X%s; %f\n", asmstring[DFB], buf[0],buf[1],buf[2],buf[3], asmstring[TAB], float_val);
                        break;
                    case 5: // CONSTANT_Long
                        printf("%s$05%s; LONG\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 8);
                        printf("%s$%02X,$%02X,$%02X,$%02X,$%02X,$%02X,$%02X,$%02X\n", asmstring[DFB], buf[0],buf[1],buf[2],buf[3],buf[4],buf[5],buf[6],buf[7]);
                        break;
                    case 6: // CONSTANT_Double
                        printf("%s$06%s; DOUBLE\n", asmstring[DFB], asmstring[TAB]);
                        printf("Double: ");
                        read(fd, buf, 8);
                        printf("%s$%02X,$%02X,$%02X,$%02X,$%02X,$%02X,$%02X,$%02Xn", asmstring[DFB], buf[0],buf[1],buf[2],buf[3],buf[4],buf[5],buf[6],buf[7]);
                        break;
                    case 7: // CONSTANT_Class
                        printf("%s07%s; CLASS\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        break;
                    case 8: // CONSTANT_String
                        printf("%s$08%s; STRING\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        break;
                    case 9: // CONSTANT_Fieldref
                        printf("%s$09%s; FIELDREF\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; CLASS #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; NAME AND TYPE #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        break;
                    case 10: // CONSTANT_Methodref
                        printf("%s$0A%s; METHODREF\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; CLASS #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; NAME AND TYPE #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        break;
                    case 11: // CONSTANT_InterfaceMethodref
                        printf("%s$0B%s; IFACE METHODREF\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; CLASS #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; NAME AND TYPE #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        break;
                    case 12: // CONSTANT_NameAndType
                        printf("%s$0C%s; NAME AND TYPE\n", asmstring[DFB], asmstring[TAB]);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; NAME #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("%s$%02X,$%02X%s; DESC #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                        break;
                    default:
                        printf("Bad tag %d", buf[0]);
                        read(fd, buf, 1);
                        break;
                }
            }
            read(fd, buf, 2); access_flags = TO_16(buf);
            printf("%s\n%s ACCESS FLAGS\n%s\n%s$%02X,$%02X%s; 0x%04X\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT], asmstring[DFB], buf[0],buf[1], asmstring[TAB], access_flags);
            read(fd, buf, 2); this_class = TO_16(buf);
            printf("%s\n%s THIS CLASS\n%s\n%s$%02X,$%02X%s; #%d\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT], asmstring[DFB], buf[0],buf[1], asmstring[TAB], this_class);
            read(fd, buf, 2); super_class = TO_16(buf);
            printf("%s\n%s SUPER CLASS\n%s\n%s$%02X,$%02X%s; #%d\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT], asmstring[DFB], buf[0],buf[1], asmstring[TAB], super_class);
            read(fd, buf, 2); iface_count = TO_16(buf);
            printf("%s\n%s INTERFACES\n%s\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT]);
            printf("%s$%02X,$%02X%s; IFACE COUNT %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], iface_count);
            for (cc = 0; cc < iface_count; cc++)
            {
                read(fd, buf, 2); idx = TO_16(buf);
                printf("%s$%02X,$%02X%s; #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
            }
            read(fd, buf, 2); fields_count = TO_16(buf);
            printf("%s\n%s FIELDS\n%s\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT]);
            printf("%s$%02X,$%02X%s; FIELD COUNT %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], fields_count);
            for (cc = 0; cc < fields_count; cc++)
            {
                printf("%s****** FIELD INDEX %d ********\n", asmstring[CMNT], cc);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("%s$%02X,$%02X%s; ACCESS FLAGS 0x%04X\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("%s$%02X,$%02X%s; NAME #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("%s$%02X,$%02X%s; DESC #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                read(fd, buf, 2); cnt = TO_16(buf);
                printf("%s$%02X,$%02X%s; ATTRIB COUNT %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], cnt);
                for (ii = 0; ii < cnt; ii++)
                {
                    int nm, len;
                    
                    printf("%s ATTRIB INDEX %d\n", asmstring[CMNT], ii);
                    read(fd, buf, 2); nm = TO_16(buf);                
                    read(fd, buf, 4); len = TO_32(buf);                
                    DumpAttr(fd, nm, len, 'F', cc, ii);
                }
            }
            read(fd, buf, 2); methods_count = TO_16(buf);
            printf("%s\n%s METHODS\n%s\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT]);
            printf("%s$%02X,$%02X%s; METHOD COUNT %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], methods_count);
            for (cc = 0; cc < methods_count; cc++)
            {
                printf("%s****** METHOD INDEX %d ********\n", asmstring[CMNT], cc);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("%s$%02X,$%02X%s; ACCESS FLAGS 0x%04X\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("%s$%02X,$%02X%s; NAME #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("%s$%02X,$%02X%s; DESC #%d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], idx);
                read(fd, buf, 2); cnt = TO_16(buf);
                printf("%s$%02X,$%02X%s; ATTRIB COUNT %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], cnt);
                for (ii = 0; ii < cnt; ii++)
                {
                    int nm, len;
                    
                    printf("%s ATTRIB INDEX %d\n", asmstring[CMNT], ii);
                    read(fd, buf, 2); nm = TO_16(buf);                
                    read(fd, buf, 4); len = TO_32(buf);                
                    DumpAttr(fd, nm, len, 'M', cc, ii);
                }
            }
            read(fd, buf, 2); attribs_count = TO_16(buf);
            printf("%s\n%s GLOBAL ATTRIBUTES\n%s\n", asmstring[CMNT], asmstring[CMNT], asmstring[CMNT]);
            printf("%s$%02X,$%02X%s; ATTRIB COUNT %d\n", asmstring[DFB], buf[0],buf[1], asmstring[TAB], attribs_count);
            for (cc = 0; cc < attribs_count; cc++)
            {
                int nm, len;
                
                printf("%s****** ATTRIB INDEX %d ********\n", asmstring[CMNT], cc);
                read(fd, buf, 2); nm = TO_16(buf);                
                read(fd, buf, 4); len = TO_32(buf);                
                DumpAttr(fd, nm, len, 'G', cc, ii);
            }
            close(fd);
        }
    }
    return (0);
}