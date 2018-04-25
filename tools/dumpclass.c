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

#define TO_32(b)    ((b[0]<<24)|(b[1]<<16)|(b[2]<<8)|(b[3]))
#define TO_16(b)    ((b[0]<<8)|(b[1]))

int CodeAttrName, ConstantAttrName, ExceptAttrName, InnerClassAttrName,
    SyntheticAttrName, SourceAttrName, LineNumAttrName, LocalVarAttrName;
unsigned char buf[256];

void DumpAttr(int fd, int name, int len)
{
    if (name == CodeAttrName)
    {
        unsigned short max_stack, max_locals, cnt, val;
        signed short sval;
        int ival, defval, lval, hval, pad;
        unsigned long code_len, pc;
        printf("Code:");
        read(fd, buf, 2); max_stack = TO_16(buf);
        printf(" max_stack %d", max_stack);
        read(fd, buf, 2); max_locals = TO_16(buf);
        printf("  max_locals %d", max_locals);
        read(fd, buf, 4); code_len = TO_32(buf);
        printf("  code_len %u\n", code_len);
        for (pc = 0; pc < code_len; pc++)
        {
            read(fd, buf, 1);
            printf("\t\t\t\t%06u[0x%04X]: %s", pc, pc, opcode[buf[0]]);
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
                    printf(" %u", buf[1]);
                    if (buf[0] == 0x84) // , const
                    {
                        read(fd, buf, 1);
                        printf(" %d", (signed char)buf[0]);
                        pc++;
                    }
                    pc++;
                    break;
                case 0xC4: // wide local var index16
                    read(fd, buf, 1);
                    if (buf[0] == 0x84)
                    {
                        read(fd, buf, 2); val = TO_16(buf);
                        printf(" iinc %u", val);            
                        read(fd, buf, 2); val = TO_16(buf);
                        printf(" %d", val);            
                        pc += 5;
                    }
                    else
                    {
                        printf(" %s", opcode[buf[0]]);            
                        read(fd, buf, 2); val = TO_16(buf);
                        printf(" %u", val);            
                        pc += 3;
                    }
                    break;
                case 0x12: // class const pool index8
                    read(fd, buf+1, 1);
                    printf(" #%u", buf[1]);
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
                    printf(" #%u", val);            
                    if (buf[2] == 0xC5) // , dimensions
                    {
                        read(fd, buf, 1);
                        printf(" %d", buf[0]);
                        pc++;
                    }
                   if (buf[2] == 0xB9) // , count, 0
                    {
                        read(fd, buf, 2);
                        printf(" %d",  buf[0]);
                        pc += 2;
                    }
                    pc += 2;
                    break;
                case 0x10: // byte
                    read(fd, buf, 1);
                    printf(" %d", buf[0]);            
                    pc++;
                    break;
                case 0xBC: // atype
                    read(fd, buf, 1);
                    printf(" %d (%s)", buf[0], atype[buf[0]]);            
                    pc++;
                    break;
                case 0x11: // short
                    read(fd, buf, 2); val = TO_16(buf);
                    printf(" %d", val);            
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
                    printf(" %+d (%06u)", sval, pc + sval);            
                    pc += 2;
                    break;
                case 0xC9: // branch32
                case 0xC8:
                    read(fd, buf, 4); ival = TO_32(buf);
                    printf(" %+d (%06u)", ival, pc + ival);            
                    pc += 4;
                    break;
                case 0xAB: // lookupswitch
                    pad = ((pc + 1) & 0x03);
                    if (pad)
                    {
                        pad = 4 - pad;
                        read(fd, buf, pad); // padding
                    }
                    read(fd, buf, 4); defval = TO_32(buf); // default
                    read(fd, buf, 4); hval = TO_32(buf); // num pairs
                    printf(" %d:\n", hval);
                    for (cnt = 0; cnt < hval; cnt++)
                    {
                        read(fd, buf, 4); ival = TO_32(buf); // match
                        read(fd, buf, 4); lval = TO_32(buf); // offsets
                        printf("\t\t\t\t\t%d: %+d (%06u)\n", ival, lval, pc + lval);
                    }
                    printf("\t\t\t\t\tdefault: %+d (%06u)", defval, pc + defval);
                    pc += pad + 8 + 8 * hval;
                    break;
                case 0xAA: // tableswitch
                    pad = ((pc + 1) & 0x03);
                    if (pad)
                    {
                        pad = 4 - pad;
                        read(fd, buf, pad); // padding
                    }
                    read(fd, buf, 4); defval = TO_32(buf); // default
                    read(fd, buf, 4); lval = TO_32(buf); // low value
                    read(fd, buf, 4); hval = TO_32(buf); // high value
                    printf(" %d to %d:\n", lval, hval);
                    for (cnt = lval; cnt <= hval; cnt++)
                    {
                        read(fd, buf, 4); ival = TO_32(buf); // offsets
                        printf("\t\t\t\t\t%d: %+d (%06u)\n", cnt, ival, pc + ival);
                    }
                    printf("\t\t\t\t\tdefault: %+d (%06u)", defval, pc + defval);
                    pc += pad + 12 + 4 * (hval - lval + 1);
                    break;
            }
            printf("\n");
        }
        read(fd, buf, 2); cnt = TO_16(buf);
        printf("\t\t\texcept_len: %d\n", cnt);
        while (cnt--)
        {
            read(fd, buf, 2); val = TO_16(buf);
            printf("\t\t\t\tfrom %06u", val);
            read(fd, buf, 2); val = TO_16(buf);
            printf(" to %06u", val);
            read(fd, buf, 2); val = TO_16(buf);
            printf(" handler %06u", val);
            read(fd, buf, 2); val = TO_16(buf);
            printf(" type %d\n", val);
        }
        read(fd, buf, 2); cnt = TO_16(buf);
        while (cnt--)
        {
            read(fd, buf, 2); name = TO_16(buf);                
            read(fd, buf, 4); len = TO_32(buf);
            printf("\t\t\tcode_attr: ");
            DumpAttr(fd, name, len);
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
        printf("ConstantValue: #%d\n", idx);
    }
    else if (name == ExceptAttrName)
    {
        unsigned short except_cnt, except_idx;
        
        read(fd, buf, 2); except_cnt = TO_16(buf);                
        printf("Exceptions count: %d\n", except_cnt);
        while (except_cnt--)
        {
            read(fd, buf, 2); except_idx = TO_16(buf);                
            printf("\t\t\t\t# %d\n", except_idx);
        }
    }
    else
    {
        if (name == InnerClassAttrName)
            printf("InnerClasses: ");
        else if (name == SyntheticAttrName)
            printf("Synthetic: ");
        else if (name ==  SourceAttrName)
            printf("SourceFile: ");
        else if (name == LineNumAttrName)
            printf("LineNumberTable: ");
        else if (name == LocalVarAttrName)
            printf("LocalVariableTable: ");
        else
            printf("name #%d  ", name);
        printf("len %d\n", len);
        while (len > 256)
        {
            read(fd, buf, 256);
            len -= 256;
        }
        if (len)
            read(fd, buf, len);
    }
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
        
    if (argc < 2)
    {
        printf("Usage: %s <classfile1> [classfile2] ...\n", argv[0]);
        return (1);
    }
    for (cf = 1; cf < argc; cf++)
    {
        if ((fd = open(argv[cf], O_RDONLY, 0)) > 0)
        {
            printf("Class file: %s\n", argv[cf]);
            read(fd, buf, 4); magic = TO_32(buf);
            printf("\tmagic: 0x%08X\n", magic);
            read(fd, buf, 2); minor_version = TO_16(buf);
            read(fd, buf, 2); major_version = TO_16(buf);
            printf("\tmajor.minor: %d.%d\n", major_version, minor_version);
            read(fd, buf, 2); const_pool_count = TO_16(buf);
            printf("\tconst_pool_count: %d\n", const_pool_count);
            for (cc = 1; cc < const_pool_count; cc++)
            {
                printf("\t\t[%02d] ", cc);
                read(fd, buf, 1);
                switch (buf[0])
                {
                    case 0: // BAD
                        printf("Bad tag 0");
                        read(fd, buf, 1);
                        break;
                    case 1: // CONSTANT_Utf8
                        printf("Utf8: ");
                        read(fd, buf, 2); idx = TO_16(buf);
                        read(fd, buf, idx);
                        buf[idx] = '\0';
                        printf("%s", buf);
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
                        printf("Integer: ");
                        read(fd, buf, 4); idx = TO_32(buf);
                        printf("%d", idx);
                        break;
                    case 4: // CONSTANT_Float
                        printf("Float: ");
                        read(fd, buf, 4); idx = TO_32(buf);
                        float_val = *(float *)(&idx);
                        printf("%f", float_val);
                        break;
                    case 5: // CONSTANT_Long
                        printf("Long: ");
                        read(fd, buf, 8); idx = TO_32((buf+4));
                        printf("%d", idx);
                        cc++;
                        break;
                    case 6: // CONSTANT_Double
                        printf("Double: ");
                        read(fd, buf, 8);
                        cc++;
                        break;
                    case 7: // CONSTANT_Class
                        printf("Class: ");
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("#%d", idx);
                        break;
                    case 8: // CONSTANT_String
                        printf("String: ");
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("#%d", idx);
                        break;
                    case 9: // CONSTANT_Fieldref
                        printf("Fieldref: ");
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("class #%d", idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("  name_type #%d", idx);
                        break;
                    case 10: // CONSTANT_Methodref
                        printf("Methodref: ");
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("class #%d", idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("  name_type #%d", idx);
                        break;
                    case 11: // CONSTANT_InterfaceMethodref
                        printf("InterfaceMethodref: ");
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("class #%d", idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("  name_type #%d", idx);
                        break;
                    case 12: // CONSTANT_NameAndType
                        printf("NameAndType: ");
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("name #%d", idx);
                        read(fd, buf, 2); idx = TO_16(buf);
                        printf("  desc #%d", idx);
                        break;
                    default:
                        printf("Bad tag %d", buf[0]);
                        read(fd, buf, 1);
                        break;
                }
                printf("\n");
            }
            read(fd, buf, 2); access_flags = TO_16(buf);
            printf("\taccess_flags: 0x%04X\n", access_flags);
            read(fd, buf, 2); this_class = TO_16(buf);
            printf("\tthis_class: %d\n", this_class);
            read(fd, buf, 2); super_class = TO_16(buf);
            printf("\tsuper_class: %d\n", super_class);
            read(fd, buf, 2); iface_count = TO_16(buf);
            printf("\tinterfaces_count: %d\n", iface_count);
            for (cc = 0; cc < iface_count; cc++)
            {
                printf("\t\t[%02d] ", cc);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("#%d\n", idx);
            }
            read(fd, buf, 2); fields_count = TO_16(buf);
            printf("\tfields_count: %d\n", fields_count);
            for (cc = 0; cc < fields_count; cc++)
            {
                printf("\t\t[%02d] ", cc);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("access 0x%02X  ", idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("name #%d  ", idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("desc #%d  ", idx);
                read(fd, buf, 2); cnt = TO_16(buf);
                printf("attr_cnt %d\n", cnt);
                for (ii = 0; ii < cnt; ii++)
                {
                    int nm, len;
                    
                    printf("\t\t\t[%02d] ", ii);
                    read(fd, buf, 2); nm = TO_16(buf);                
                    read(fd, buf, 4); len = TO_32(buf);                
                    DumpAttr(fd, nm, len);
                }
            }
            read(fd, buf, 2); methods_count = TO_16(buf);
            printf("\tmethods_count: %d\n", methods_count);
            for (cc = 0; cc < methods_count; cc++)
            {
                printf("\t\t[%02d] ", cc);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("access 0x%02X  ", idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("name #%d  ", idx);
                read(fd, buf, 2); idx = TO_16(buf);
                printf("desc #%d  ", idx);
                read(fd, buf, 2); cnt = TO_16(buf);
                printf("attr_cnt %d\n", cnt);
                for (ii = 0; ii < cnt; ii++)
                {
                    int nm, len;
                    
                    printf("\t\t\t[%02d] ", ii);
                    read(fd, buf, 2); nm = TO_16(buf);                
                    read(fd, buf, 4); len = TO_32(buf);                
                    DumpAttr(fd, nm, len);
                }
            }
            read(fd, buf, 2); attribs_count = TO_16(buf);
            printf("\tattributes_count: %d\n", attribs_count);
            for (cc = 0; cc < attribs_count; cc++)
            {
                int nm, len;
                
                printf("\t\t[%02d] ", cc);
                read(fd, buf, 2); nm = TO_16(buf);                
                read(fd, buf, 4); len = TO_32(buf);                
                DumpAttr(fd, nm, len);
            }
            close(fd);
        }
    }
    return (0);
}
