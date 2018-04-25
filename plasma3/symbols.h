/*
 * Symbol table types.
 */
#define GLOBAL_TYPE     (0)
#define CONST_TYPE      (1 << 0)
#define WORD_TYPE       (1 << 1)
#define BYTE_TYPE       (1 << 2)
#define VAR_TYPE        (WORD_TYPE | BYTE_TYPE)
#define ASM_TYPE        (1 << 3)
#define DEF_TYPE        (1 << 4)
#define BRANCH_TYPE     (1 << 5)
#define FUNC_TYPE       (ASM_TYPE | DEF_TYPE)
#define LOCAL_TYPE      (1 << 6)
#define EXTERN_TYPE		(1 << 7)
#define ADDR_TYPE       (VAR_TYPE | FUNC_TYPE | EXTERN_TYPE)
#define WPTR_TYPE       (1 << 8)
#define BPTR_TYPE       (1 << 9)
#define PTR_TYPE        (BPTR_TYPE | WPTR_TYPE)
#define STRING_TYPE     (1 << 10)
#define TAG_TYPE        (1 << 11)
#define EXPORT_TYPE     (1 << 12)

int id_match(char *name, int len, char *id);
int idlocal_lookup(char *name, int len);
int idglobal_lookup(char *name, int len);
int idconst_lookup(char *name, int len);
int idlocal_add(char *name, int len, int type, int size);
int idglobal_add(char *name, int len, int type, int size);
int id_add(char *name, int len, int type, int size);
int idfunc_set(char *name, int len, int type);
int idfunc_add(char *name, int len, int type, int tag);
int idconst_add(char *name, int len, int value);
int id_tag(char *name, int len);
int id_const(char *name, int len);
int id_type(char *name, int len);
void idglobal_size(int type, int size, int constsize);
int idlocal_size(void);
void idlocal_reset(void);
int tag_new(int type);
