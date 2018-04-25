/*
 * Symbol table and fixup information.
 */
static int  consts = 0;
static char idconst_name[1024][17];
static int  idconst_value[1024];
static int  globals = 0;
static int  globalsize = 0;
static char idglobal_name[1024][17];
static int  idglobal_type[1024];
static int  idglobal_tag[1024];
static int  locals = 0;
static int  localsize = 0;
static char idlocal_name[128][17];
static int  idlocal_type[128];
static int  idlocal_offset[128];
static int  codetag = 0;
static int  fixup = 0;
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
    return (codetag++);
}
int add_fixup(tag)
{
    return (fixup++);
}
