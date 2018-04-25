	.INCLUDE	"plstub.s"
;    1: ;
;    2: ; Global constants
;    3: ;
;    4: const FALSE             = 0
					; FALSE = 0
;    5: const TRUE              = !FALSE
					; TRUE = -1
;    6: ;
;    7: ; Data and code buffer variables
;    8: ;
;    9: const iobuffer          = $0800
					; iobuffer = 2048
;   10: const compbuff          = $6000
					; compbuff = 24576
;   11: const compbuffsz        = $4000
					; compbuffsz = 16384
;   12: const func_dict         = $5000
					; func_dict = 20480
;   13: const fixup_tbl			= $4000
					; fixup_tbl = 16384
;   14: const argbuff           = $2006
					; argbuff = 8198
;   15: const inbuff            = $0200
					; inbuff = 512
;   16: const instr             = $01FF
					; instr = 511
;   17: byte  inref
D0000:	DS	1			; inref
;   18: ;
;   19: ; REL file tables
;   20: ;
;   21: word datalen, codelen
D0001:	DS	2			; datalen
D0003:	DS	2			; codelen
;   22: word fixup              = fixup_tbl
D0005:					; fixup
	DW	$4000
;   23: word numfuncs           = 1
D0007:					; numfuncs
	DW	$0001
;   24: ;
;   25: ; Symbol table variables
;   26: ;
;   27: const idglobal_tblsz    = $0800
					; idglobal_tblsz = 2048
;   28: const idlocal_tblsz     = $0200
					; idlocal_tblsz = 512
;   29: const idglobal_tbl      = $1000
					; idglobal_tbl = 4096
;   30: const idlocal_tbl       = $1800
					; idlocal_tbl = 6144
;   31: const ctag_max          = 768
					; ctag_max = 768
;   32: const ctag_value        = $1A00
					; ctag_value = 6656
;   33: const ctag_flags        = $0D00
					; ctag_flags = 3328
;   34: const idval             = 0
					; idval = 0
;   35: const idtype            = 2
					; idtype = 2
;   36: const idname            = 3
					; idname = 3
;   37: const idrecsz           = 4
					; idrecsz = 4
;   38: word globals            = 0
D0009:					; globals
	DW	$0000
;   39: word datasize           = 0
D0011:					; datasize
	DW	$0000
;   40: word lastglobal
D0013:	DS	2			; lastglobal
;   41: byte locals             = 0
D0015:					; locals
	DB	$00
;   42: word framesize          = 0
D0016:					; framesize
	DW	$0000
;   43: word lastlocal
D0018:	DS	2			; lastlocal
;   44: const resolved          = 1
					; resolved = 1
;   45: const is_ctag           = $8000
					; is_ctag = 32768
;   46: const mask_ctag         = $7FFF
					; mask_ctag = 32767
;   47: word codetag            = -1
D0020:					; codetag
	DW	$FFFF
;   48: ;
;   49: ; Symbol types
;   50: ;
;   51: const EXTERN_SYM		= $10
					; EXTERN_SYM = 16
;   52: const EXPORT_SYM		= $08
					; EXPORT_SYM = 8
;   53: ;
;   54: ; Compiler pointers
;   55: ;
;   56: word codeptr
D0022:	DS	2			; codeptr
;   57: word entrypoint         = 0
D0024:					; entrypoint
	DW	$0000
;   58: byte lastop             = $FF
D0026:					; lastop
	DB	$FF
;   59: byte perr
D0027:	DS	1			; perr
;   60: ;
;   61: ; String variables
;   62: ;
;   63: byte version[]          = "PLASMA ][ COMPILER VERSION 0.8 "
D0028:					; version
	DB	$1F
	DB	$50,$4C,$41,$53,$4D,$41,$20,$5D
	DB	$5B,$20,$43,$4F,$4D,$50,$49,$4C
	DB	$45,$52,$20,$56,$45,$52,$53,$49
	DB	$4F,$4E,$20,$30,$2E,$38,$20
;   64: byte badfile[]          = "FILE NOT FOUND"
D0060:					; badfile
	DB	$0E
	DB	$46,$49,$4C,$45,$20,$4E,$4F,$54
	DB	$20,$46,$4F,$55,$4E,$44
;   65: ;
;   66: ; Tokens
;   67: ;
;   68: const ID_TKN            = $D6 ; V
					; ID_TKN = 214
;   69: const CHR_TKN           = $C3 ; C
					; CHR_TKN = 195
;   70: const INT_TKN           = $C9 ; I
					; INT_TKN = 201
;   71: const STR_TKN           = $D3 ; S
					; STR_TKN = 211
;   72: const EOL_TKN           = $02
					; EOL_TKN = 2
;   73: const EOF_TKN           = $01
					; EOF_TKN = 1
;   74: const ERR_TKN           = $00
					; ERR_TKN = 0
;   75: ;
;   76: ; Binary operand operators
;   77: ;
;   78: const SET_TKN           = $BD ; =
					; SET_TKN = 189
;   79: const SETLIST_TKN       = $B9 ; =,
					; SETLIST_TKN = 185
;   80: const ADD_TKN           = $AB ; +
					; ADD_TKN = 171
;   81: const SUB_TKN           = $AD ; -
					; SUB_TKN = 173
;   82: const MUL_TKN           = $AA ; *
					; MUL_TKN = 170
;   83: const DIV_TKN           = $AF ; /
					; DIV_TKN = 175
;   84: const MOD_TKN           = $A5 ; %
					; MOD_TKN = 165
;   85: const OR_TKN            = $BF ; ?
					; OR_TKN = 191
;   86: const EOR_TKN           = $DE ; ^
					; EOR_TKN = 222
;   87: const AND_TKN           = $A6 ; &
					; AND_TKN = 166
;   88: const SHR_TKN           = $D2 ; R
					; SHR_TKN = 210
;   89: const SHL_TKN           = $CC ; L
					; SHL_TKN = 204
;   90: const GT_TKN            = $BE ; >
					; GT_TKN = 190
;   91: const GE_TKN            = $C8 ; H
					; GE_TKN = 200
;   92: const LT_TKN            = $BC ; <
					; LT_TKN = 188
;   93: const LE_TKN            = $C2 ; B
					; LE_TKN = 194
;   94: const NE_TKN            = $D5 ; U
					; NE_TKN = 213
;   95: const EQ_TKN            = $C5 ; E
					; EQ_TKN = 197
;   96: const LOGIC_AND_TKN     = $CE ; N
					; LOGIC_AND_TKN = 206
;   97: const LOGIC_OR_TKN      = $CF ; O
					; LOGIC_OR_TKN = 207
;   98: ;
;   99: ; Unary operand operators
;  100: ;
;  101: const AT_TKN            = $C0 ; @
					; AT_TKN = 192
;  102: const DOT_TKN           = $AE ; .
					; DOT_TKN = 174
;  103: const COLON_TKN         = $BA ; :
					; COLON_TKN = 186
;  104: const NEG_TKN           = $AD ; -
					; NEG_TKN = 173
;  105: const COMP_TKN          = $A3 ; #
					; COMP_TKN = 163
;  106: const LOGIC_NOT_TKN     = $A1 ; !
					; LOGIC_NOT_TKN = 161
;  107: const BPTR_TKN          = $DE ; ^
					; BPTR_TKN = 222
;  108: const WPTR_TKN          = $AA ; *
					; WPTR_TKN = 170
;  109: const INC_TKN           = $C1 ; A
					; INC_TKN = 193
;  110: const DEC_TKN           = $C4 ; D
					; DEC_TKN = 196
;  111: ;
;  112: ; Enclosure tokens
;  113: ;
;  114: const OPEN_PAREN_TKN    = $A8 ; (
					; OPEN_PAREN_TKN = 168
;  115: const CLOSE_PAREN_TKN   = $A9 ; )
					; CLOSE_PAREN_TKN = 169
;  116: const OPEN_BRACKET_TKN  = $DB ; [
					; OPEN_BRACKET_TKN = 219
;  117: const CLOSE_BRACKET_TKN = $DD ; ]
					; CLOSE_BRACKET_TKN = 221
;  118: ;
;  119: ; Misc. tokens
;  120: ;
;  121: const COMMA_TKN         = $AC ; ,
					; COMMA_TKN = 172
;  122: const COMMENT_TKN       = $BB ; ;
					; COMMENT_TKN = 187
;  123: ;
;  124: ; Keyword tokens
;  125: ;
;  126: const CONST_TKN         = $80
					; CONST_TKN = 128
;  127: const BYTE_TKN          = $81
					; BYTE_TKN = 129
;  128: const WORD_TKN          = $82
					; WORD_TKN = 130
;  129: const IF_TKN            = $83
					; IF_TKN = 131
;  130: const ELSEIF_TKN        = $84
					; ELSEIF_TKN = 132
;  131: const ELSE_TKN          = $85
					; ELSE_TKN = 133
;  132: const FIN_TKN           = $86
					; FIN_TKN = 134
;  133: const END_TKN           = $87
					; END_TKN = 135
;  134: const WHILE_TKN         = $88
					; WHILE_TKN = 136
;  135: const LOOP_TKN          = $89
					; LOOP_TKN = 137
;  136: const CASE_TKN          = $8A
					; CASE_TKN = 138
;  137: const OF_TKN            = $8B
					; OF_TKN = 139
;  138: const DEFAULT_TKN       = $8C
					; DEFAULT_TKN = 140
;  139: const ENDCASE_TKN       = $8D
					; ENDCASE_TKN = 141
;  140: const FOR_TKN           = $8E
					; FOR_TKN = 142
;  141: const TO_TKN            = $8F
					; TO_TKN = 143
;  142: const DOWNTO_TKN        = $90
					; DOWNTO_TKN = 144
;  143: const STEP_TKN          = $91
					; STEP_TKN = 145
;  144: const NEXT_TKN          = $92
					; NEXT_TKN = 146
;  145: const REPEAT_TKN        = $93
					; REPEAT_TKN = 147
;  146: const UNTIL_TKN         = $94
					; UNTIL_TKN = 148
;  147: const DEF_TKN         	= $95
					; DEF_TKN = 149
;  148: const OPT_TKN         	= $96
					; OPT_TKN = 150
;  149: const DROP_TKN          = $97
					; DROP_TKN = 151
;  150: const DONE_TKN          = $98
					; DONE_TKN = 152
;  151: const RETURN_TKN        = $99
					; RETURN_TKN = 153
;  152: const BREAK_TKN         = $9A
					; BREAK_TKN = 154
;  153: const START_TKN         = $9B
					; START_TKN = 155
;  154: const EXIT_TKN          = $9C
					; EXIT_TKN = 156
;  155: const EVAL_TKN          = $9D
					; EVAL_TKN = 157
;  156: const FUNC_TKN          = $9E
					; FUNC_TKN = 158
;  157: const EXTERN_TKN        = $9F
					; EXTERN_TKN = 159
;  158: const ENTRY_TKN			= $A0
					; ENTRY_TKN = 160
;  159: const IMPORT_TKN        = $A1
					; IMPORT_TKN = 161
;  160: const INCLUDE_TKN       = $A2
					; INCLUDE_TKN = 162
;  161: ;
;  162: ; Types
;  163: ;
;  164: const CONST_TYPE        = $01
					; CONST_TYPE = 1
;  165: const BYTE_TYPE         = $02
					; BYTE_TYPE = 2
;  166: const WORD_TYPE         = $04
					; WORD_TYPE = 4
;  167: const VAR_TYPE          = $06 ; (WORD_TYPE | BYTE_TYPE)
					; VAR_TYPE = 6
;  168: const FUNC_TYPE         = $08
					; FUNC_TYPE = 8
;  169: const FUNC_CONST_TYPE   = $09
					; FUNC_CONST_TYPE = 9
;  170: const ADDR_TYPE         = $0E ; (VAR_TYPE | FUNC_TYPE)
					; ADDR_TYPE = 14
;  171: const LOCAL_TYPE        = $10
					; LOCAL_TYPE = 16
;  172: const BPTR_TYPE         = $20
					; BPTR_TYPE = 32
;  173: const WPTR_TYPE         = $40
					; WPTR_TYPE = 64
;  174: const PTR_TYPE          = $60 ; (BPTR_TYPE | WPTR_TYPE)
					; PTR_TYPE = 96
;  175: const XBYTE_TYPE        = $22 ; (BPTR_TYPE | BYTE_TYPE)
					; XBYTE_TYPE = 34
;  176: const XWORD_TYPE        = $44 ; (WPTR_TYPE | WORD_TYPE)
					; XWORD_TYPE = 68
;  177: const STR_TYPE          = $80
					; STR_TYPE = 128
;  178: ;
;  179: ; Keywords
;  180: ;
;  181: byte keywrds[]
D0075:					; keywrds
;  182: byte                    = "IF",      IF_TKN
	DB	$02
	DB	$49,$46
	DB	$83
;  183: byte                    = "TO",      TO_TKN
	DB	$02
	DB	$54,$4F
	DB	$8F
;  184: byte                    = "IS",      OF_TKN
	DB	$02
	DB	$49,$53
	DB	$8B
;  185: byte                    = "OR",      LOGIC_OR_TKN
	DB	$02
	DB	$4F,$52
	DB	$CF
;  186: byte                    = "FOR",     FOR_TKN
	DB	$03
	DB	$46,$4F,$52
	DB	$8E
;  187: byte                    = "FIN",     FIN_TKN
	DB	$03
	DB	$46,$49,$4E
	DB	$86
;  188: byte                    = "DEF",     DEF_TKN
	DB	$03
	DB	$44,$45,$46
	DB	$95
;  189: byte                    = "END",     END_TKN
	DB	$03
	DB	$45,$4E,$44
	DB	$87
;  190: byte                    = "AND",     LOGIC_AND_TKN
	DB	$03
	DB	$41,$4E,$44
	DB	$CE
;  191: byte                    = "NOT",     LOGIC_NOT_TKN
	DB	$03
	DB	$4E,$4F,$54
	DB	$A1
;  192: byte                    = "BYTE",    BYTE_TKN
	DB	$04
	DB	$42,$59,$54,$45
	DB	$81
;  193: byte                    = "WORD",    WORD_TKN
	DB	$04
	DB	$57,$4F,$52,$44
	DB	$82
;  194: byte                    = "DROP",    DROP_TKN
	DB	$04
	DB	$44,$52,$4F,$50
	DB	$97
;  195: byte                    = "ELSE",    ELSE_TKN
	DB	$04
	DB	$45,$4C,$53,$45
	DB	$85
;  196: byte                    = "NEXT",    NEXT_TKN
	DB	$04
	DB	$4E,$45,$58,$54
	DB	$92
;  197: byte                    = "WHEN",    CASE_TKN
	DB	$04
	DB	$57,$48,$45,$4E
	DB	$8A
;  198: byte                    = "LOOP",    LOOP_TKN
	DB	$04
	DB	$4C,$4F,$4F,$50
	DB	$89
;  199: byte                    = "FUNC",    FUNC_TKN
	DB	$04
	DB	$46,$55,$4E,$43
	DB	$9E
;  200: byte                    = "STEP",    STEP_TKN
	DB	$04
	DB	$53,$54,$45,$50
	DB	$91
;  201: byte                    = "EXIT",    EXIT_TKN
	DB	$04
	DB	$45,$58,$49,$54
	DB	$9C
;  202: byte                    = "DONE",    DONE_TKN
	DB	$04
	DB	$44,$4F,$4E,$45
	DB	$98
;  203: byte                    = "WEND",    ENDCASE_TKN
	DB	$04
	DB	$57,$45,$4E,$44
	DB	$8D
;  204: byte					= "ENTRY",   ENTRY_TKN
	DB	$05
	DB	$45,$4E,$54,$52,$59
	DB	$A0
;  205: byte                    = "CONST",   CONST_TKN
	DB	$05
	DB	$43,$4F,$4E,$53,$54
	DB	$80
;  206: byte                    = "ELSIF",   ELSEIF_TKN
	DB	$05
	DB	$45,$4C,$53,$49,$46
	DB	$84
;  207: byte                    = "WHILE",   WHILE_TKN
	DB	$05
	DB	$57,$48,$49,$4C,$45
	DB	$88
;  208: byte                    = "UNTIL",   UNTIL_TKN
	DB	$05
	DB	$55,$4E,$54,$49,$4C
	DB	$94
;  209: byte                    = "BREAK",   BREAK_TKN
	DB	$05
	DB	$42,$52,$45,$41,$4B
	DB	$9A
;  210: byte                    = "OTHER",   DEFAULT_TKN
	DB	$05
	DB	$4F,$54,$48,$45,$52
	DB	$8C
;  211: byte                    = "DOWNTO",  DOWNTO_TKN
	DB	$06
	DB	$44,$4F,$57,$4E,$54,$4F
	DB	$90
;  212: byte                    = "REPEAT",  REPEAT_TKN
	DB	$06
	DB	$52,$45,$50,$45,$41,$54
	DB	$93
;  213: byte                    = "RETURN",  RETURN_TKN
	DB	$06
	DB	$52,$45,$54,$55,$52,$4E
	DB	$99
;  214: byte					= "EXTERN",  EXTERN_TKN
	DB	$06
	DB	$45,$58,$54,$45,$52,$4E
	DB	$9F
;  215: byte					= "IMPORT",  IMPORT_TKN
	DB	$06
	DB	$49,$4D,$50,$4F,$52,$54
	DB	$A1
;  216: byte					= "INCLUDE", INCLUDE_TKN
	DB	$07
	DB	$49,$4E,$43,$4C,$55,$44,$45
	DB	$A2
;  217: byte                    = $FF
	DB	$FF
;  218: ;
;  219: ; Mathematical ops
;  220: ;
;  221: const bops_tblsz        = 18 ; minus 1
					; bops_tblsz = 18
;  222: byte bops_tbl[]         ; Highest precedence
D0292:					; bops_tbl
;  223: byte                    = MUL_TKN, DIV_TKN, MOD_TKN
	DB	$AA
	DB	$AF
	DB	$A5
;  224: byte                    = ADD_TKN, SUB_TKN
	DB	$AB
	DB	$AD
;  225: byte                    = SHR_TKN, SHL_TKN
	DB	$D2
	DB	$CC
;  226: byte                    = AND_TKN
	DB	$A6
;  227: byte                    = EOR_TKN
	DB	$DE
;  228: byte                    = OR_TKN
	DB	$BF
;  229: byte                    = GT_TKN, GE_TKN, LT_TKN, LE_TKN
	DB	$BE
	DB	$C8
	DB	$BC
	DB	$C2
;  230: byte                    = EQ_TKN, NE_TKN
	DB	$C5
	DB	$D5
;  231: byte                    = LOGIC_AND_TKN
	DB	$CE
;  232: byte                    = LOGIC_OR_TKN
	DB	$CF
;  233: byte                    = COMMA_TKN
	DB	$AC
;  234:                         ; Lowest precedence
;  235: byte bops_prec[]        ; Highest precedence
D0311:					; bops_prec
;  236: byte                    = 1, 1, 1
	DB	$01
	DB	$01
	DB	$01
;  237: byte                    = 2, 2
	DB	$02
	DB	$02
;  238: byte                    = 3, 3
	DB	$03
	DB	$03
;  239: byte                    = 4
	DB	$04
;  240: byte                    = 5
	DB	$05
;  241: byte                    = 6
	DB	$06
;  242: byte                    = 7, 7, 7, 7
	DB	$07
	DB	$07
	DB	$07
	DB	$07
;  243: byte                    = 8, 8
	DB	$08
	DB	$08
;  244: byte                    = 9
	DB	$09
;  245: byte                    = 10
	DB	$0A
;  246: byte                    = 11
	DB	$0B
;  247:                         ; Lowest precedence
;  248: byte opstack[16]
D0330:	DS	16			; opstack
;  249: byte precstack[16]
D0346:	DS	16			; precstack
;  250: word opsp = -1
D0362:					; opsp
	DW	$FFFF
;  251: ;
;  252: ; Scanner variables
;  253: ;
;  254: byte  token, tknlen
D0364:	DS	1			; token
D0365:	DS	1			; tknlen
;  255: word  scanptr, tknptr
D0366:	DS	2			; scanptr
D0368:	DS	2			; tknptr
;  256: word  constval
D0370:	DS	2			; constval
;  257: word  lineno = 0
D0372:					; lineno
	DW	$0000
;  258: ;
;  259: ; Compiler output messages
;  260: ;
;  261: byte entrypt_str[]      = "START: "
D0374:					; entrypt_str
	DB	$07
	DB	$53,$54,$41,$52,$54,$3A,$20
;  262: byte dup_id[]           = "DUPLICATE IDENTIFIER"
D0382:					; dup_id
	DB	$14
	DB	$44,$55,$50,$4C,$49,$43,$41,$54
	DB	$45,$20,$49,$44,$45,$4E,$54,$49
	DB	$46,$49,$45,$52
;  263: byte undecl_id[]        = "UNDECLARED IDENTIFIER"
D0403:					; undecl_id
	DB	$15
	DB	$55,$4E,$44,$45,$43,$4C,$41,$52
	DB	$45,$44,$20,$49,$44,$45,$4E,$54
	DB	$49,$46,$49,$45,$52
;  264: byte bad_cnst[]         = "BAD CONSTANT"
D0425:					; bad_cnst
	DB	$0C
	DB	$42,$41,$44,$20,$43,$4F,$4E,$53
	DB	$54,$41,$4E,$54
;  265: byte bad_offset[]       = "BAD STRUCT OFFSET"
D0438:					; bad_offset
	DB	$11
	DB	$42,$41,$44,$20,$53,$54,$52,$55
	DB	$43,$54,$20,$4F,$46,$46,$53,$45
	DB	$54
;  266: byte bad_decl[]         = "BAD DECLARATION"
D0456:					; bad_decl
	DB	$0F
	DB	$42,$41,$44,$20,$44,$45,$43,$4C
	DB	$41,$52,$41,$54,$49,$4F,$4E
;  267: byte bad_op[]           = "BAD OPERATION"
D0472:					; bad_op
	DB	$0D
	DB	$42,$41,$44,$20,$4F,$50,$45,$52
	DB	$41,$54,$49,$4F,$4E
;  268: byte bad_stmnt[]        = "BAD STATMENT"
D0486:					; bad_stmnt
	DB	$0C
	DB	$42,$41,$44,$20,$53,$54,$41,$54
	DB	$4D,$45,$4E,$54
;  269: byte bad_expr[]         = "BAD EXPRESSION"
D0499:					; bad_expr
	DB	$0E
	DB	$42,$41,$44,$20,$45,$58,$50,$52
	DB	$45,$53,$53,$49,$4F,$4E
;  270: byte bad_syntax[]       = "BAD SYNTAX"
D0514:					; bad_syntax
	DB	$0A
	DB	$42,$41,$44,$20,$53,$59,$4E,$54
	DB	$41,$58
;  271: byte estk_overflw[]     = "EVAL STACK OVERFLOW"
D0525:					; estk_overflw
	DB	$13
	DB	$45,$56,$41,$4C,$20,$53,$54,$41
	DB	$43,$4B,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  272: byte estk_underflw[]    = "EVAL STACK UNDERFLOW"
D0545:					; estk_underflw
	DB	$14
	DB	$45,$56,$41,$4C,$20,$53,$54,$41
	DB	$43,$4B,$20,$55,$4E,$44,$45,$52
	DB	$46,$4C,$4F,$57
;  273: byte local_overflw[]    = "LOCAL FRAME OVERFLOW"
D0566:					; local_overflw
	DB	$14
	DB	$4C,$4F,$43,$41,$4C,$20,$46,$52
	DB	$41,$4D,$45,$20,$4F,$56,$45,$52
	DB	$46,$4C,$4F,$57
;  274: byte global_sym_overflw[] = "GLOBAL SYMBOL TABLE OVERFLOW"
D0587:					; global_sym_overflw
	DB	$1C
	DB	$47,$4C,$4F,$42,$41,$4C,$20,$53
	DB	$59,$4D,$42,$4F,$4C,$20,$54,$41
	DB	$42,$4C,$45,$20,$4F,$56,$45,$52
	DB	$46,$4C,$4F,$57
;  275: byte local_sym_overflw[] = "LOCAL SYMBOL TABLE OVERFLOW"
D0616:					; local_sym_overflw
	DB	$1B
	DB	$4C,$4F,$43,$41,$4C,$20,$53,$59
	DB	$4D,$42,$4F,$4C,$20,$54,$41,$42
	DB	$4C,$45,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  276: byte ctag_full[]        = "CODE LABEL OVERFLOW"
D0644:					; ctag_full
	DB	$13
	DB	$43,$4F,$44,$45,$20,$4C,$41,$42
	DB	$45,$4C,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  277: byte no_close_paren[]   = "MISSING CLOSING PAREN"
D0664:					; no_close_paren
	DB	$15
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$43,$4C,$4F,$53,$49,$4E,$47,$20
	DB	$50,$41,$52,$45,$4E
;  278: byte no_close_bracket[] = "MISSING CLOSING BRACKET"
D0686:					; no_close_bracket
	DB	$17
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$43,$4C,$4F,$53,$49,$4E,$47,$20
	DB	$42,$52,$41,$43,$4B,$45,$54
;  279: byte missing_op[]       = "MISSING OPERAND"
D0710:					; missing_op
	DB	$0F
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$4F,$50,$45,$52,$41,$4E,$44
;  280: byte no_fin[]           = "MISSING FIN"
D0726:					; no_fin
	DB	$0B
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$46,$49,$4E
;  281: byte no_loop[]          = "MISSING LOOP"
D0738:					; no_loop
	DB	$0C
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$4C,$4F,$4F,$50
;  282: byte no_until[]         = "MISSING UNTIL"
D0751:					; no_until
	DB	$0D
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$55,$4E,$54,$49,$4C
;  283: byte no_done[]          = "MISSING DONE"
D0765:					; no_done
	DB	$0C
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$44,$4F,$4E,$45
;  284: byte no_local_init[]    = "NO INITIALIZED LOCALS"
D0778:					; no_local_init
	DB	$15
	DB	$4E,$4F,$20,$49,$4E,$49,$54,$49
	DB	$41,$4C,$49,$5A,$45,$44,$20,$4C
	DB	$4F,$43,$41,$4C,$53
;  285: ;
;  286: ; Runtime functions
;  287: ;
;  288: byte runtime0[]         = "romcall"
D0800:					; runtime0
	DB	$07
	DB	$72,$6F,$6D,$63,$61,$6C,$6C
;  289: byte RUNTIME0[]         = "ROMCALL"
D0808:					; RUNTIME0
	DB	$07
	DB	$52,$4F,$4D,$43,$41,$4C,$4C
;  290: byte runtime1[]         = "syscall"
D0816:					; runtime1
	DB	$07
	DB	$73,$79,$73,$63,$61,$6C,$6C
;  291: byte RUNTIME1[]         = "SYSCALL"
D0824:					; RUNTIME1
	DB	$07
	DB	$53,$59,$53,$43,$41,$4C,$4C
;  292: byte runtime2[]         = "memset"
D0832:					; runtime2
	DB	$06
	DB	$6D,$65,$6D,$73,$65,$74
;  293: byte RUNTIME2[]         = "MEMSET"
D0839:					; RUNTIME2
	DB	$06
	DB	$4D,$45,$4D,$53,$45,$54
;  294: byte runtime3[]         = "memcpy"
D0846:					; runtime3
	DB	$06
	DB	$6D,$65,$6D,$63,$70,$79
;  295: byte RUNTIME3[]         = "MEMCPY"
D0853:					; RUNTIME3
	DB	$06
	DB	$4D,$45,$4D,$43,$50,$59
;  296: byte runtime4[]         = "cout"
D0860:					; runtime4
	DB	$04
	DB	$63,$6F,$75,$74
;  297: byte RUNTIME4[]         = "COUT"
D0865:					; RUNTIME4
	DB	$04
	DB	$43,$4F,$55,$54
;  298: byte runtime5[]         = "cin"
D0870:					; runtime5
	DB	$03
	DB	$63,$69,$6E
;  299: byte RUNTIME5[]         = "CIN"
D0874:					; RUNTIME5
	DB	$03
	DB	$43,$49,$4E
;  300: byte runtime6[]         = "prstr"
D0878:					; runtime6
	DB	$05
	DB	$70,$72,$73,$74,$72
;  301: byte RUNTIME6[]         = "PRSTR"
D0884:					; RUNTIME6
	DB	$05
	DB	$50,$52,$53,$54,$52
;  302: byte runtime7[]         = "rdstr"
D0890:					; runtime7
	DB	$05
	DB	$72,$64,$73,$74,$72
;  303: byte RUNTIME7[]         = "RDSTR"
D0896:					; RUNTIME7
	DB	$05
	DB	$52,$44,$53,$54,$52
;  304: ;
;  305: ; Parser variables
;  306: ;
;  307: byte infunc             = 0
D0902:					; infunc
	DB	$00
;  308: byte stack_loop         = 0
D0903:					; stack_loop
	DB	$00
;  309: byte prevstmnt          = 0
D0904:					; prevstmnt
	DB	$00
;  310: word retfunc_tag        = 0
D0905:					; retfunc_tag
	DW	$0000
;  311: word break_tag          = 0
D0907:					; break_tag
	DW	$0000
;  312: func parse_expr_01, parse_module_01
;  313: ;
;  314: ; Defines for ASM routines
;  315: ;
;  316: asm equates
C0002:					; equates()
;  317:         TMP     EQU     $F0
        TMP     EQU     $F0
;  318:         TMPL    EQU     TMP
        TMPL    EQU     TMP
;  319:         TMPH    EQU     TMP+1
        TMPH    EQU     TMP+1
;  320:         SRC     EQU     TMP
        SRC     EQU     TMP
;  321:         SRCL    EQU     SRC
        SRCL    EQU     SRC
;  322:         SRCH    EQU     SRC+1
        SRCH    EQU     SRC+1
;  323:         DST     EQU     SRC+2
        DST     EQU     SRC+2
;  324:         DSTL    EQU     DST
        DSTL    EQU     DST
;  325:         DSTH    EQU     DST+1
        DSTH    EQU     DST+1
;  326:         ESP     EQU     DST+2
        ESP     EQU     DST+2
;  327: 		SAVEESP	EQU		ESP+1
		SAVEESP	EQU		ESP+1
;  328: 		SAVESP	EQU		SAVEESP+1
		SAVESP	EQU		SAVEESP+1
;  329: 		SAVEFP	EQU		SAVESP+1
		SAVEFP	EQU		SAVESP+1
;  330: 		SAVETMR	EQU		SAVEFP+2
		SAVETMR	EQU		SAVEFP+2
;  331: 		SAVEINT	EQU		SAVETMR+2
		SAVEINT	EQU		SAVETMR+2
;  332: 		TMRVEC	EQU		$03E8
		TMRVEC	EQU		$03E8
;  333: 		INTVEC	EQU		$03EA
		INTVEC	EQU		$03EA
;  334: JMPTMP:	JMP		(TMP)
JMPTMP:	JMP		(TMP)
;  335: STKOVFLW:
STKOVFLW:
;  336: 		LDY		#$02
		LDY		#$02
;  337: 		JMP		EXECRET
		JMP		EXECRET
;  338: BRKCHK:
BRKCHK:
;  339: 		LDA		$C000
		LDA		$C000
;  340: 		CMP		#$83		; CTRL-C
		CMP		#$83		; CTRL-C
;  341: 		BNE		:+
		BNE		:+
;  342: 		BIT		$C010
		BIT		$C010
;  343: 		LDY		#$01
		LDY		#$01
;  344: 		JMP		EXECRET
		JMP		EXECRET
;  345: :
:
;  346: end
	RTS
;  347: ;
;  348: ; CALL 6502 ROUTINE
;  349: ; ROMCALL(AREG, XREG, YREG, STATUS, ADDR)
;  350: ;
;  351: asm romcall
C0004:					; romcall()
;  352:         PHP
        PHP
;  353:         LDA     ESTKL,X
        LDA     ESTKL,X
;  354:         STA     TMPL
        STA     TMPL
;  355:         LDA     ESTKH,X
        LDA     ESTKH,X
;  356:         STA     TMPH
        STA     TMPH
;  357:         INX
        INX
;  358:         LDA     ESTKL,X
        LDA     ESTKL,X
;  359:         PHA
        PHA
;  360:         INX
        INX
;  361:         LDA     ESTKL,X
        LDA     ESTKL,X
;  362:         TAY
        TAY
;  363:         INX
        INX
;  364:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  365:         PHA
        PHA
;  366:         LDA     ESTKL,X
        LDA     ESTKL,X
;  367:         INX
        INX
;  368:         STX     ESP
        STX     ESP
;  369:         TAX
        TAX
;  370:         PLA
        PLA
;  371:         BIT     ROMIN
        BIT     ROMIN
;  372:         PLP
        PLP
;  373:         JSR     JMPTMP
        JSR     JMPTMP
;  374:         PHP
        PHP
;  375:         BIT     LCBNK2
        BIT     LCBNK2
;  376:         STA     REGVALS+0
        STA     REGVALS+0
;  377:         STX     REGVALS+1
        STX     REGVALS+1
;  378:         STY     REGVALS+2
        STY     REGVALS+2
;  379:         PLA
        PLA
;  380:         STA     REGVALS+3
        STA     REGVALS+3
;  381:         LDX     ESP
        LDX     ESP
;  382:         LDA     #<REGVALS
        LDA     #<REGVALS
;  383:         LDY     #>REGVALS
        LDY     #>REGVALS
;  384:         STA     ESTKL,X
        STA     ESTKL,X
;  385:         STY     ESTKH,X
        STY     ESTKH,X
;  386:         PLP
        PLP
;  387:         RTS
        RTS
;  388: REGVALS: DS 4
REGVALS: DS 4
;  389: end
	RTS
;  390: ;
;  391: ; CALL PRODOS
;  392: ; SYSCALL(CMD, PARAMS)
;  393: ;
;  394: asm syscall
C0006:					; syscall()
;  395:         LDA     ESTKL,X
        LDA     ESTKL,X
;  396:         LDY     ESTKH,X
        LDY     ESTKH,X
;  397:         STA     PARAMS
        STA     PARAMS
;  398:         STY     PARAMS+1
        STY     PARAMS+1
;  399:         INX
        INX
;  400:         LDA     ESTKL,X
        LDA     ESTKL,X
;  401:         STA     CMD
        STA     CMD
;  402:         STX     ESP
        STX     ESP
;  403:         BIT     ROMIN
        BIT     ROMIN
;  404:         JSR     $BF00
        JSR     $BF00
;  405: CMD:    DB      00
CMD:    DB      00
;  406: PARAMS: DW      0000
PARAMS: DW      0000
;  407:         BIT     LCBNK2
        BIT     LCBNK2
;  408:         LDX     ESP
        LDX     ESP
;  409:         STA     ESTKL,X
        STA     ESTKL,X
;  410:         LDY     #$00
        LDY     #$00
;  411:         STY     ESTKH,X
        STY     ESTKH,X
;  412: end
	RTS
;  413: ;
;  414: ; SET MEMORY TO VALUE
;  415: ; MEMSET(VALUE, ADDR, SIZE)
;  416: ;
;  417: asm memset
C0008:					; memset()
;  418:         LDY     #$00
        LDY     #$00
;  419:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  420:         STA     DSTL
        STA     DSTL
;  421:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  422:         STA     DSTH
        STA     DSTH
;  423:         INC     ESTKL,X
        INC     ESTKL,X
;  424:         INC     ESTKH,X
        INC     ESTKH,X
;  425: SETMEM: DEC     ESTKL,X
SETMEM: DEC     ESTKL,X
;  426:         BNE     :+
        BNE     :+
;  427:         DEC     ESTKH,X
        DEC     ESTKH,X
;  428:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  429: :       LDA     ESTKL+2,X
:       LDA     ESTKL+2,X
;  430:         STA     (DST),Y
        STA     (DST),Y
;  431:         INY
        INY
;  432:         BNE     :+
        BNE     :+
;  433:         INC     DSTH
        INC     DSTH
;  434: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  435:         BNE     :+
        BNE     :+
;  436:         DEC     ESTKH,X
        DEC     ESTKH,X
;  437:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  438: :       LDA     ESTKH+2,X
:       LDA     ESTKH+2,X
;  439:         STA     (DST),Y
        STA     (DST),Y
;  440:         INY
        INY
;  441:         BNE     SETMEM
        BNE     SETMEM
;  442:         INC     DSTH
        INC     DSTH
;  443:         BNE     SETMEM
        BNE     SETMEM
;  444: MEMEXIT: INX
MEMEXIT: INX
;  445:         INX
        INX
;  446:         INX
        INX
;  447: end
	RTS
;  448: ;
;  449: ; COPY MEMORY
;  450: ; MEMCPY(SRCADDR, DSTADDR, SIZE)
;  451: ;
;  452: asm memcpy
C0010:					; memcpy()
;  453:         LDY     #$00
        LDY     #$00
;  454:         LDA     ESTKL,X
        LDA     ESTKL,X
;  455:         BNE     :+
        BNE     :+
;  456:         LDA     ESTKH,X
        LDA     ESTKH,X
;  457:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  458: :       LDA     ESTKL+1,X
:       LDA     ESTKL+1,X
;  459:         STA     DSTL
        STA     DSTL
;  460:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  461:         STA     DSTH
        STA     DSTH
;  462:         LDA     ESTKL+2,X
        LDA     ESTKL+2,X
;  463:         STA     SRCL
        STA     SRCL
;  464:         LDA     ESTKH+2,X
        LDA     ESTKH+2,X
;  465:         STA     SRCH
        STA     SRCH
;  466:         CMP     DSTH
        CMP     DSTH
;  467:         BCC     REVCPY
        BCC     REVCPY
;  468:         BNE     FORCPY
        BNE     FORCPY
;  469:         LDA     SRCL
        LDA     SRCL
;  470:         CMP     DSTL
        CMP     DSTL
;  471:         BCS     FORCPY
        BCS     FORCPY
;  472: REVCPY:             ; REVERSE DIRECTION COPY
REVCPY:             ; REVERSE DIRECTION COPY
;  473: ;       CLC
;  474:         LDA     ESTKL,X
        LDA     ESTKL,X
;  475:         ADC     DSTL
        ADC     DSTL
;  476:         STA     DSTL
        STA     DSTL
;  477:         LDA     ESTKH,X
        LDA     ESTKH,X
;  478:         ADC     DSTH
        ADC     DSTH
;  479:         STA     DSTH
        STA     DSTH
;  480:         CLC
        CLC
;  481:         LDA     ESTKL,X
        LDA     ESTKL,X
;  482:         ADC     SRCL
        ADC     SRCL
;  483:         STA     SRCL
        STA     SRCL
;  484:         LDA     ESTKH,X
        LDA     ESTKH,X
;  485:         ADC     SRCH
        ADC     SRCH
;  486:         STA     SRCH
        STA     SRCH
;  487:         INC     ESTKH,X
        INC     ESTKH,X
;  488: REVCPYLP:
REVCPYLP:
;  489:         LDA     DSTL
        LDA     DSTL
;  490:         BNE     :+
        BNE     :+
;  491:         DEC     DSTH
        DEC     DSTH
;  492: :       DEC     DSTL
:       DEC     DSTL
;  493:         LDA     SRCL
        LDA     SRCL
;  494:         BNE     :+
        BNE     :+
;  495:         DEC     SRCH
        DEC     SRCH
;  496: :       DEC     SRCL
:       DEC     SRCL
;  497:         LDA     (SRC),Y
        LDA     (SRC),Y
;  498:         STA     (DST),Y
        STA     (DST),Y
;  499:         DEC     ESTKL,X
        DEC     ESTKL,X
;  500:         BNE     REVCPYLP
        BNE     REVCPYLP
;  501:         DEC     ESTKH,X
        DEC     ESTKH,X
;  502:         BNE     REVCPYLP
        BNE     REVCPYLP
;  503:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  504: FORCPY: INC     ESTKH,X
FORCPY: INC     ESTKH,X
;  505: FORCPYLP:
FORCPYLP:
;  506:         LDA     (SRC),Y
        LDA     (SRC),Y
;  507:         STA     (DST),Y
        STA     (DST),Y
;  508:         INC     DSTL
        INC     DSTL
;  509:         BNE     :+
        BNE     :+
;  510:         INC     DSTH
        INC     DSTH
;  511: :       INC     SRCL
:       INC     SRCL
;  512:         BNE     :+
        BNE     :+
;  513:         INC     SRCH
        INC     SRCH
;  514: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  515:         BNE     FORCPYLP
        BNE     FORCPYLP
;  516:         DEC     ESTKH,X
        DEC     ESTKH,X
;  517:         BNE     FORCPYLP
        BNE     FORCPYLP
;  518:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  519: end
	RTS
;  520: ;
;  521: ; CHAR OUT
;  522: ; COUT(CHAR)
;  523: ;
;  524: asm cout
C0012:					; cout()
;  525:         LDA     ESTKL,X
        LDA     ESTKL,X
;  526:         INX
        INX
;  527:         ORA     #$80
        ORA     #$80
;  528:         BIT     ROMIN
        BIT     ROMIN
;  529:         JSR     $FDED
        JSR     $FDED
;  530:         BIT     LCBNK2
        BIT     LCBNK2
;  531: end
	RTS
;  532: ;
;  533: ; CHAR IN
;  534: ; RDKEY()
;  535: ;
;  536: asm cin
C0014:					; cin()
;  537:         BIT     ROMIN
        BIT     ROMIN
;  538:         STX     ESP
        STX     ESP
;  539:         JSR     $FD0C
        JSR     $FD0C
;  540:         LDX     ESP
        LDX     ESP
;  541:         BIT     LCBNK2
        BIT     LCBNK2
;  542:         DEX
        DEX
;  543:         AND     #$7F
        AND     #$7F
;  544:         STA     ESTKL,X
        STA     ESTKL,X
;  545:         LDY     #$00
        LDY     #$00
;  546:         STY     ESTKH,X
        STY     ESTKH,X
;  547: end
	RTS
;  548: ;
;  549: ; PRINT STRING
;  550: ; PRSTR(STR)
;  551: ;
;  552: asm prstr
C0016:					; prstr()
;  553:         LDY     #$00
        LDY     #$00
;  554:         LDA     ESTKL,X
        LDA     ESTKL,X
;  555:         STA     SRCL
        STA     SRCL
;  556:         LDA     ESTKH,X
        LDA     ESTKH,X
;  557:         STA     SRCH
        STA     SRCH
;  558:         BIT     ROMIN
        BIT     ROMIN
;  559:         LDA     (SRC),Y
        LDA     (SRC),Y
;  560:         STA     ESTKL,X
        STA     ESTKL,X
;  561:         BEQ     :+
        BEQ     :+
;  562: _PRS1:  INY
_PRS1:  INY
;  563:         LDA     (SRC),Y
        LDA     (SRC),Y
;  564:         ORA     #$80
        ORA     #$80
;  565:         JSR     $FDED
        JSR     $FDED
;  566:         TYA
        TYA
;  567:         CMP     ESTKL,X
        CMP     ESTKL,X
;  568:         BNE     _PRS1
        BNE     _PRS1
;  569: :       INX
:       INX
;  570:         BIT     LCBNK2
        BIT     LCBNK2
;  571: end
	RTS
;  572: ;
;  573: ; READ STRING
;  574: ; STR = RDSTR(PROMPTCHAR)
;  575: ;
;  576: asm rdstr
C0018:					; rdstr()
;  577:         LDA     ESTKL,X
        LDA     ESTKL,X
;  578:         STA     $33
        STA     $33
;  579:         STX     ESP
        STX     ESP
;  580:         BIT     ROMIN
        BIT     ROMIN
;  581:         JSR     $FD6A
        JSR     $FD6A
;  582:         BIT     LCBNK2
        BIT     LCBNK2
;  583:         STX     $01FF
        STX     $01FF
;  584: :       LDA     $01FF,X
:       LDA     $01FF,X
;  585:         AND     #$7F
        AND     #$7F
;  586:         STA     $01FF,X
        STA     $01FF,X
;  587:         DEX
        DEX
;  588:         BPL     :-
        BPL     :-
;  589:         LDX     ESP
        LDX     ESP
;  590:         LDA     #$FF
        LDA     #$FF
;  591:         STA     ESTKL,X
        STA     ESTKL,X
;  592:         LDA     #$01
        LDA     #$01
;  593:         STA     ESTKH,X
        STA     ESTKH,X
;  594: end
	RTS
;  595: ;def toupper_11(c)
;  596: ;   if c >= 'a'
;  597: ;       if c <= 'z'
;  598: ;           return c - $20
;  599: ;       fin
;  600: ;   fin
;  601: ;   return c
;  602: ;end
;  603: asm toupper_11
C0020:					; toupper_11()
;  604:         LDA     ESTKL,X
        LDA     ESTKL,X
;  605:         CMP     #'a'
        CMP     #'a'
;  606:         BCC     :+
        BCC     :+
;  607:         CMP     #'z'+1
        CMP     #'z'+1
;  608:         BCS     :+
        BCS     :+
;  609:         SEC
        SEC
;  610:         SBC     #$20
        SBC     #$20
;  611:         STA     ESTKL,X
        STA     ESTKL,X
;  612: :
:
;  613: end
	RTS
;  614: ;
;  615: ; EXIT
;  616: ;
;  617: asm exit
C0022:					; exit()
;  618:         JSR $BF00
        JSR $BF00
;  619:         DB  $65
        DB  $65
;  620:         DW  EXITTBL
        DW  EXITTBL
;  621: EXITTBL:
EXITTBL:
;  622:         DB  4
        DB  4
;  623:         DB  0
        DB  0
;  624: end
	RTS
;  625: ;
;  626: ; ProDOS routines
;  627: ;
;  628: def getpfx_11(path)
C0024:					; getpfx_11()
					; path = 2
;  629:     byte params[3]
					; params = 4
;  630: 
;  631:     ^path    = 0
	JSR	_INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$70			; SB
;  632:     params.0 = 1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  633:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  634:     perr     = syscall($C7, @params)
	DB	$2A,$C7			; CB	199
	DB	$28,$04			; LLA	4
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  635:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  636: end
;  637: def setpfx_11(path)
C0026:					; setpfx_11()
					; path = 2
;  638:     byte params[3]
					; params = 4
;  639: 
;  640:     params.0 = 1
	JSR	_INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  641:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  642:     perr     = syscall($C6, @params)
	DB	$2A,$C6			; CB	198
	DB	$28,$04			; LLA	4
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  643:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  644: end
;  645: def open_21(path, buff)
C0028:					; open_21()
					; path = 2
					; buff = 4
;  646:     byte params[6]
					; params = 6
;  647: 
;  648:     params.0 = 3
	JSR	_INTERP
	DB	$58,$0C,$02		; ENTER	12,2
	DB	$28,$06			; LLA	6
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  649:     params:1 = path
	DB	$28,$07			; LLA	7
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  650:     params:3 = buff
	DB	$28,$09			; LLA	9
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  651:     params.5 = 0
	DB	$28,$0B			; LLA	11
	DB	$00			; ZERO
	DB	$70			; SB
;  652:     perr     = syscall($C8, @params)
	DB	$2A,$C8			; CB	200
	DB	$28,$06			; LLA	6
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  653:     return params.5
	DB	$28,$0B			; LLA	11
	DB	$60			; LB
	DB	$5A			; LEAVE
;  654: end
;  655: def close_11(refnum)
C0030:					; close_11()
					; refnum = 2
;  656:     byte params[2]
					; params = 4
;  657: 
;  658:     params.0 = 1
	JSR	_INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  659:     params.1 = refnum
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  660:     perr     = syscall($CC, @params)
	DB	$2A,$CC			; CB	204
	DB	$28,$04			; LLA	4
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  661:     return perr
	DB	$68,<D0027,>D0027	; LAB	D0027
	DB	$5A			; LEAVE
;  662: end
;  663: def read_31(refnum, buff, len)
C0032:					; read_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  664:     byte params[8]
					; params = 8
;  665: 
;  666:     params.0 = 4
	JSR	_INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  667:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  668:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  669:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  670:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  671:     perr     = syscall($CA, @params)
	DB	$2A,$CA			; CB	202
	DB	$28,$08			; LLA	8
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  672:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  673: end
;  674: def write_31(refnum, buff, len)
C0034:					; write_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  675:     byte params[8]
					; params = 8
;  676: 
;  677:     params.0 = 4
	JSR	_INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  678:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  679:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  680:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  681:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  682:     perr     = syscall($CB, @params)
	DB	$2A,$CB			; CB	203
	DB	$28,$08			; LLA	8
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  683:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  684: end
;  685: def create_41(path, access, type, aux)
C0036:					; create_41()
					; path = 2
					; access = 4
					; type = 6
					; aux = 8
;  686:     byte params[12]
					; params = 10
;  687: 
;  688:     params.0  = 7
	JSR	_INTERP
	DB	$58,$16,$04		; ENTER	22,4
	DB	$28,$0A			; LLA	10
	DB	$2A,$07			; CB	7
	DB	$70			; SB
;  689:     params:1  = path
	DB	$28,$0B			; LLA	11
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  690:     params.3  = access
	DB	$28,$0D			; LLA	13
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  691:     params.4  = type
	DB	$28,$0E			; LLA	14
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  692:     params:5  = aux
	DB	$28,$0F			; LLA	15
	DB	$66,$08			; LLW	8
	DB	$72			; SW
;  693:     params.7  = $1
	DB	$28,$11			; LLA	17
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  694:     params:8  = 0
	DB	$28,$12			; LLA	18
	DB	$00			; ZERO
	DB	$72			; SW
;  695:     params:10 = 0
	DB	$28,$14			; LLA	20
	DB	$00			; ZERO
	DB	$72			; SW
;  696:     perr      = syscall($C0, @params)
	DB	$2A,$C0			; CB	192
	DB	$28,$0A			; LLA	10
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  697:     return perr
	DB	$68,<D0027,>D0027	; LAB	D0027
	DB	$5A			; LEAVE
;  698: end
;  699: def destroy_11(path)
C0038:					; destroy_11()
					; path = 2
;  700:     byte params[12]
					; params = 4
;  701: 
;  702:     params.0 = 1
	JSR	_INTERP
	DB	$58,$10,$01		; ENTER	16,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  703:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  704:     perr     = syscall($C1, @params)
	DB	$2A,$C1			; CB	193
	DB	$28,$04			; LLA	4
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  705:     return perr
	DB	$68,<D0027,>D0027	; LAB	D0027
	DB	$5A			; LEAVE
;  706: end
;  707: def newline_31(refnum, emask, nlchar)
C0040:					; newline_31()
					; refnum = 2
					; emask = 4
					; nlchar = 6
;  708:     byte params[4]
					; params = 8
;  709: 
;  710:     params.0 = 3
	JSR	_INTERP
	DB	$58,$0C,$03		; ENTER	12,3
	DB	$28,$08			; LLA	8
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  711:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  712:     params.2 = emask
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  713:     params.3 = nlchar
	DB	$28,$0B			; LLA	11
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  714:     perr     = syscall($C9, @params)
	DB	$2A,$C9			; CB	201
	DB	$28,$08			; LLA	8
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$78,<D0027,>D0027	; SAB	D0027
;  715:     return perr
	DB	$68,<D0027,>D0027	; LAB	D0027
	DB	$5A			; LEAVE
;  716: end
;  717: def crout
C0042:					; crout()
;  718:     cout($0D)
	JSR	_INTERP
	DB	$2A,$0D			; CB	13
	DB	$54,<C0012,>C0012	; CALL	C0012
;  719: end
	DB	$5C			; RET
;  720: def prbyte_10(h)
C0044:					; prbyte_10()
					; h = 2
;  721:     cout('$')
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0012,>C0012	; CALL	C0012
;  722:     drop romcall(h, 0, 0, 0, $FDDA)
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DA,$FD		; CW	64986
	DB	$54,<C0004,>C0004	; CALL	C0004
	DB	$30			; DROP
;  723: end
	DB	$5A			; LEAVE
;  724: def prword_10(h)
C0046:					; prword_10()
					; h = 2
;  725:     cout('$')
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0012,>C0012	; CALL	C0012
;  726:     drop romcall(h >> 8, h, 0, 0, $F941)
	DB	$66,$02			; LLW	2
	DB	$2A,$08			; CB	8
	DB	$1C			; SHR
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$41,$F9		; CW	63809
	DB	$54,<C0004,>C0004	; CALL	C0004
	DB	$30			; DROP
;  727: end
	DB	$5A			; LEAVE
;  728: def print_10(i)
C0048:					; print_10()
					; i = 2
;  729:     byte numstr[7]
					; numstr = 4
;  730:     byte place, sign
					; place = 11
					; sign = 12
;  731: 
;  732:     place = 6
	JSR	_INTERP
	DB	$58,$0D,$01		; ENTER	13,1
	DB	$2A,$06			; CB	6
	DB	$74,$0B			; SLB	11
;  733:     if i < 0
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0050,>C0050	; SKPFLS	C0050
;  734:         sign = 1
	DB	$2A,$01			; CB	1
	DB	$74,$0C			; SLB	12
;  735:         i    = -i
	DB	$66,$02			; LLW	2
	DB	$10			; NEG
	DB	$76,$02			; SLW	2
;  736:     else
	DB	$50,<C0051,>C0051	; SKIP	C0051
C0050:
;  737:         sign = 0
	DB	$00			; ZERO
	DB	$74,$0C			; SLB	12
;  738:     fin
C0051:
;  739:     while i >= 10
C0052:
	DB	$66,$02			; LLW	2
	DB	$2A,$0A			; CB	10
	DB	$48			; ISGE
	DB	$4C,<C0053,>C0053	; SKPFLS	C0053
;  740:         i =, numstr[place] = i % 10 + '0'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$34			; PUSH
	DB	$66,$02			; LLW	2
	DB	$2A,$0A			; CB	10
	DB	$0A			; DIV,MOD
	DB	$2A,$30			; CB	48
	DB	$02			; ADD
	DB	$36			; PULL
	DB	$2E			; SWAP
	DB	$70			; SB
	DB	$76,$02			; SLW	2
;  741:         place              = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  742:     loop
	DB	$50,<C0052,>C0052	; SKIP	C0052
C0053:
;  743:     numstr[place] = i + '0'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$2A,$30			; CB	48
	DB	$02			; ADD
	DB	$70			; SB
;  744:     place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  745:     if sign
	DB	$64,$0C			; LLB	12
	DB	$4C,<C0054,>C0054	; SKPFLS	C0054
;  746:         numstr[place] = '-'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$2D			; CB	45
	DB	$70			; SB
;  747:         place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  748:     fin
C0054:
C0055:
;  749:     numstr[place] = 6 - place
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$06			; CB	6
	DB	$64,$0B			; LLB	11
	DB	$04			; SUB
	DB	$70			; SB
;  750:     prstr(@numstr[place])
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$54,<C0016,>C0016	; CALL	C0016
;  751: end
	DB	$5A			; LEAVE
;  752: def nametostr_30(namestr, len, strptr)
C0056:					; nametostr_30()
					; namestr = 2
					; len = 4
					; strptr = 6
;  753:     ^strptr = len
	JSR	_INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$06			; LLW	6
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  754:     memcpy(namestr, strptr + 1, len)
	DB	$66,$02			; LLW	2
	DB	$66,$06			; LLW	6
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$54,<C0010,>C0010	; CALL	C0010
;  755: end
	DB	$5A			; LEAVE
;  756: 
;  757: ;=====================================
;  758: ;
;  759: ;           PLASMA Compiler
;  760: ;
;  761: ;=====================================
;  762: 
;  763: ;
;  764: ; Error handler
;  765: ;
;  766: def parse_err_11(err)
C0058:					; parse_err_11()
					; err = 2
;  767:     word i
					; i = 4
;  768: 
;  769:     drop close_11(0)
	JSR	_INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$00			; ZERO
	DB	$54,<C0030,>C0030	; CALL	C0030
	DB	$30			; DROP
;  770:     crout()
	DB	$54,<C0042,>C0042	; CALL	C0042
;  771:     print_10(lineno)
	DB	$6A,<D0372,>D0372	; LAW	D0372
	DB	$54,<C0048,>C0048	; CALL	C0048
;  772:     cout(':')
	DB	$2A,$3A			; CB	58
	DB	$54,<C0012,>C0012	; CALL	C0012
;  773:     prstr(err)
	DB	$66,$02			; LLW	2
	DB	$54,<C0016,>C0016	; CALL	C0016
;  774:     crout()
	DB	$54,<C0042,>C0042	; CALL	C0042
;  775:     prstr(instr)
	DB	$2C,$FF,$01		; CW	511
	DB	$54,<C0016,>C0016	; CALL	C0016
;  776:     crout()
	DB	$54,<C0042,>C0042	; CALL	C0042
;  777:     for i = inbuff to tknptr - 1
	DB	$2C,$00,$02		; CW	512
C0061:
	DB	$6E,$04			; DLW	4
	DB	$6A,<D0368,>D0368	; LAW	D0368
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$3A,<C0060,>C0060	; SKPGT	C0060
	DB	$0C			; INCR
;  778:         cout(' ')
	DB	$2A,$20			; CB	32
	DB	$54,<C0012,>C0012	; CALL	C0012
;  779:     next
	DB	$50,<C0061,>C0061	; SKIP	C0061
C0060:
	DB	$30			; DROP
;  780:     cout('^')
	DB	$2A,$5E			; CB	94
	DB	$54,<C0012,>C0012	; CALL	C0012
;  781:     cin()
	DB	$54,<C0014,>C0014	; CALL	C0014
;  782:     exit()
	DB	$54,<C0022,>C0022	; CALL	C0022
;  783:     return ERR_TKN
	DB	$00			; ZERO
	DB	$5A			; LEAVE
;  784: end
;  785: ;
;  786: ; Fixup table and function directory
;  787: ;
;  788: def fixupword_add10(addr)
C0062:					; fixupword_add10()
					; addr = 2
;  789: end
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$5A			; LEAVE
;  790: def fixupbyte_add10(addr)
C0064:					; fixupbyte_add10()
					; addr = 2
;  791: end
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$5A			; LEAVE
;  792: def func_add(offset, len, flags)
C0066:					; func_add()
					; offset = 2
					; len = 4
					; flags = 6
;  793: end
	JSR	_INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$5A			; LEAVE
;  794: ;
;  795: ; Emit bytecode
;  796: ;
;  797: def ctag_new_01
C0068:					; ctag_new_01()
;  798:     if codetag >= ctag_max
	JSR	_INTERP
	DB	$6A,<D0020,>D0020	; LAW	D0020
	DB	$2C,$00,$03		; CW	768
	DB	$48			; ISGE
	DB	$4C,<C0070,>C0070	; SKPFLS	C0070
;  799:         return parse_err_11(@ctag_full)
	DB	$26,<D0644,>D0644	; LA	D0644
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5C			; RET
;  800:     fin
C0070:
C0071:
;  801:     codetag = codetag + 1
	DB	$6A,<D0020,>D0020	; LAW	D0020
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0020,>D0020	; SAW	D0020
;  802:     ctag_value:[codetag] = 0
	DB	$2C,$00,$1A		; CW	6656
	DB	$6A,<D0020,>D0020	; LAW	D0020
	DB	$1E			; IDXW
	DB	$00			; ZERO
	DB	$72			; SW
;  803:     ctag_flags.[codetag] = 0
	DB	$2C,$00,$0D		; CW	3328
	DB	$6A,<D0020,>D0020	; LAW	D0020
	DB	$02			; IDXB
	DB	$00			; ZERO
	DB	$70			; SB
;  804:     return codetag ? is_ctag
	DB	$6A,<D0020,>D0020	; LAW	D0020
	DB	$2C,$00,$80		; CW	32768
	DB	$16			; IOR
	DB	$5C			; RET
;  805: end
;  806: defopt ctag_resolve_21(tag, addr)
C0072:					; ctag_resolve_21()
					; tag = 2
					; addr = 4
;  807:     word updtptr, nextptr
					; updtptr = 6
					; nextptr = 8
;  808: 
;  809:     tag = tag & mask_ctag
	LDY	#10
	LDA	#2
	JSR	ENTER
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$FF
	STA	ESTKL,X
	LDA	#$7F
	STA	ESTKH,X
	JSR	BAND
	LDY	#$02
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  810:     if ctag_flags.[tag] & resolved
	LDY	#$00
	STY	ESTKL,X
	LDA	#$0D
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	ADD
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	BAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0074
:
;  811:         return parse_err_11(@dup_id)
	DEX
	LDA	#<D0382
	STA	ESTKL,X
	LDA	#>D0382
	STA	ESTKH,X
	JSR	C0058
	JMP	LEAVE
;  812:     fin
C0074:
C0075:
;  813:     updtptr = ctag_value:[tag]
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$1A
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	IDXW
	LDY	#$00
	JSR	LW
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  814:     while updtptr
	INX
C0076:
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0077
:
;  815:         ;
;  816:         ; Update list of addresses needing resolution
;  817:         ;
;  818:         nextptr  = *updtptr
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	LW
	LDY	#$08
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  819:         *updtptr = addr
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	SW
;  820:         updtptr  = nextptr
	DEX
	LDY	#$08
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  821:     loop
	INX
	JMP	C0076
C0077:
;  822:     ctag_value:[tag] = addr
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$1A
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	IDXW
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	SW
;  823:     ctag_flags.[tag] = ctag_flags.[tag] ? resolved
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$0D
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	ADD
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$0D
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	ADD
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	IOR
	JSR	SB
;  824:     return 0
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
;  825: end
;  826: defopt emit_byte_10(bval)
C0078:					; emit_byte_10()
					; bval = 2
;  827:     ^codeptr = bval
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	SB
;  828:     codeptr  = codeptr + 1
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0022
	LDA	ESTKH,X
	STA	D0022+1
;  829: end
	INX
	JMP	LEAVE
;  830: defopt emit_word_10(wval)
C0080:					; emit_word_10()
					; wval = 2
;  831:     *codeptr = wval
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	SW
;  832:     codeptr  = codeptr + 2
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0022
	LDA	ESTKH,X
	STA	D0022+1
;  833: end
	INX
	JMP	LEAVE
;  834: def emit_fill_10(size)
C0082:					; emit_fill_10()
					; size = 2
;  835:     memset(0, codeptr, size)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$00			; ZERO
	DB	$6A,<D0022,>D0022	; LAW	D0022
	DB	$66,$02			; LLW	2
	DB	$54,<C0008,>C0008	; CALL	C0008
;  836:     codeptr = codeptr + size
	DB	$6A,<D0022,>D0022	; LAW	D0022
	DB	$66,$02			; LLW	2
	DB	$02			; ADD
	DB	$7A,<D0022,>D0022	; SAW	D0022
;  837: end
	DB	$5A			; LEAVE
;  838: def emit_codetag_10(tag)
C0084:					; emit_codetag_10()
					; tag = 2
;  839:     drop ctag_resolve_21(tag, codeptr)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$6A,<D0022,>D0022	; LAW	D0022
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
;  840: end
	DB	$5A			; LEAVE
;  841: defopt emit_op_10(op)
C0086:					; emit_op_10()
					; op = 2
;  842:     lastop   = op
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDA	ESTKL,X
	STA	D0026
;  843:     ^codeptr = op
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	SB
;  844:     codeptr  = codeptr + 1
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0022
	LDA	ESTKH,X
	STA	D0022+1
;  845: end
	INX
	JMP	LEAVE
;  846: def emit_tag_10(tag)
C0088:					; emit_tag_10()
					; tag = 2
;  847:     word updtptr
					; updtptr = 4
;  848: 
;  849:     if tag & is_ctag
	JSR	_INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$80		; CW	32768
	DB	$14			; BAND
	DB	$4C,<C0090,>C0090	; SKPFLS	C0090
;  850:         tag = tag & mask_ctag
	DB	$66,$02			; LLW	2
	DB	$2C,$FF,$7F		; CW	32767
	DB	$14			; BAND
	DB	$76,$02			; SLW	2
;  851:         updtptr = ctag_value:[tag]
	DB	$2C,$00,$1A		; CW	6656
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$76,$04			; SLW	4
;  852:         if !(ctag_flags.[tag] & resolved)
	DB	$2C,$00,$0D		; CW	3328
	DB	$66,$02			; LLW	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0092,>C0092	; SKPFLS	C0092
;  853:             ;
;  854:             ; Add to list of tags needing resolution
;  855:             ;
;  856:             ctag_value:[tag] = codeptr
	DB	$2C,$00,$1A		; CW	6656
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$6A,<D0022,>D0022	; LAW	D0022
	DB	$72			; SW
;  857:         fin
C0092:
C0093:
;  858:         emit_word_10(updtptr)
	DB	$66,$04			; LLW	4
	DB	$54,<C0080,>C0080	; CALL	C0080
;  859:     else
	DB	$50,<C0091,>C0091	; SKIP	C0091
C0090:
;  860:         emit_word_10(tag + compbuff)
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$60		; CW	24576
	DB	$02			; ADD
	DB	$54,<C0080,>C0080	; CALL	C0080
;  861:     fin
C0091:
;  862: end
	DB	$5A			; LEAVE
;  863: def emit_iddata_30(value, size, namestr)
C0094:					; emit_iddata_30()
					; value = 2
					; size = 4
					; namestr = 6
;  864:     emit_fill_10(size)
	JSR	_INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$04			; LLW	4
	DB	$54,<C0082,>C0082	; CALL	C0082
;  865: end
	DB	$5A			; LEAVE
;  866: def emit_data_41(vartype, consttype, constval, constsize)
C0096:					; emit_data_41()
					; vartype = 2
					; consttype = 4
					; constval = 6
					; constsize = 8
;  867:     byte i
					; i = 10
;  868:     word size, chrptr
					; size = 11
					; chrptr = 13
;  869: 
;  870:     if consttype == 0
	JSR	_INTERP
	DB	$58,$0F,$04		; ENTER	15,4
	DB	$66,$04			; LLW	4
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0098,>C0098	; SKPFLS	C0098
;  871:         size = constsize
	DB	$66,$08			; LLW	8
	DB	$76,$0B			; SLW	11
;  872:         emit_fill_10(constsize)
	DB	$66,$08			; LLW	8
	DB	$54,<C0082,>C0082	; CALL	C0082
;  873:     elsif consttype == STR_TYPE
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0098:
	DB	$66,$04			; LLW	4
	DB	$2A,$80			; CB	128
	DB	$40			; ISEQ
	DB	$4C,<C0100,>C0100	; SKPFLS	C0100
;  874:         size = constsize
	DB	$66,$08			; LLW	8
	DB	$76,$0B			; SLW	11
;  875:         chrptr = constval
	DB	$66,$06			; LLW	6
	DB	$76,$0D			; SLW	13
;  876:         constsize = constsize - 1
	DB	$66,$08			; LLW	8
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$08			; SLW	8
;  877:         emit_byte_10(constsize)
	DB	$66,$08			; LLW	8
	DB	$54,<C0078,>C0078	; CALL	C0078
;  878:         while constsize > 0
C0101:
	DB	$66,$08			; LLW	8
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0102,>C0102	; SKPFLS	C0102
;  879:             emit_byte_10(^chrptr)
	DB	$66,$0D			; LLW	13
	DB	$60			; LB
	DB	$54,<C0078,>C0078	; CALL	C0078
;  880:             chrptr    = chrptr + 1
	DB	$66,$0D			; LLW	13
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$76,$0D			; SLW	13
;  881:             constsize = constsize - 1
	DB	$66,$08			; LLW	8
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$08			; SLW	8
;  882:         loop
	DB	$50,<C0101,>C0101	; SKIP	C0101
C0102:
;  883:     else
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0100:
;  884:         if vartype == WORD_TYPE
	DB	$66,$02			; LLW	2
	DB	$2A,$04			; CB	4
	DB	$40			; ISEQ
	DB	$4C,<C0103,>C0103	; SKPFLS	C0103
;  885:             size = 2
	DB	$2A,$02			; CB	2
	DB	$76,$0B			; SLW	11
;  886:             emit_word_10(constval)
	DB	$66,$06			; LLW	6
	DB	$54,<C0080,>C0080	; CALL	C0080
;  887:         else
	DB	$50,<C0104,>C0104	; SKIP	C0104
C0103:
;  888:             size = 1
	DB	$2A,$01			; CB	1
	DB	$76,$0B			; SLW	11
;  889:             emit_byte_10(constval)
	DB	$66,$06			; LLW	6
	DB	$54,<C0078,>C0078	; CALL	C0078
;  890:         fin
C0104:
;  891:     fin
C0099:
;  892:     return size
	DB	$66,$0B			; LLW	11
	DB	$5A			; LEAVE
;  893: end
;  894: def emit_const_10(cval)
C0105:					; emit_const_10()
					; cval = 2
;  895:     if cval == 0
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0107,>C0107	; SKPFLS	C0107
;  896:         emit_op_10($00)
	DB	$00			; ZERO
	DB	$54,<C0086,>C0086	; CALL	C0086
;  897:     elsif cval > 0 and cval < 256
	DB	$50,<C0108,>C0108	; SKIP	C0108
C0107:
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$01		; CW	256
	DB	$46			; ISLT
	DB	$24			; LAND
	DB	$4C,<C0109,>C0109	; SKPFLS	C0109
;  898:         emit_op_10($2A)
	DB	$2A,$2A			; CB	42
	DB	$54,<C0086,>C0086	; CALL	C0086
;  899:         emit_byte_10(cval)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  900:     else
	DB	$50,<C0108,>C0108	; SKIP	C0108
C0109:
;  901:         emit_op_10($2C)
	DB	$2A,$2C			; CB	44
	DB	$54,<C0086,>C0086	; CALL	C0086
;  902:         emit_word_10(cval)
	DB	$66,$02			; LLW	2
	DB	$54,<C0080,>C0080	; CALL	C0080
;  903:     fin
C0108:
;  904: end
	DB	$5A			; LEAVE
;  905: def emit_lb
C0110:					; emit_lb()
;  906:     emit_op_10($60)
	JSR	_INTERP
	DB	$2A,$60			; CB	96
	DB	$54,<C0086,>C0086	; CALL	C0086
;  907: end
	DB	$5C			; RET
;  908: def emit_lw
C0112:					; emit_lw()
;  909:     emit_op_10($62)
	JSR	_INTERP
	DB	$2A,$62			; CB	98
	DB	$54,<C0086,>C0086	; CALL	C0086
;  910: end
	DB	$5C			; RET
;  911: def emit_llb_10(index)
C0114:					; emit_llb_10()
					; index = 2
;  912:     emit_op_10($64)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$64			; CB	100
	DB	$54,<C0086,>C0086	; CALL	C0086
;  913:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  914: end
	DB	$5A			; LEAVE
;  915: def emit_llw_10(index)
C0116:					; emit_llw_10()
					; index = 2
;  916:     emit_op_10($66)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$66			; CB	102
	DB	$54,<C0086,>C0086	; CALL	C0086
;  917:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  918: end
	DB	$5A			; LEAVE
;  919: def emit_lab_10(tag)
C0118:					; emit_lab_10()
					; tag = 2
;  920:     emit_op_10($68)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$68			; CB	104
	DB	$54,<C0086,>C0086	; CALL	C0086
;  921:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  922: end
	DB	$5A			; LEAVE
;  923: def emit_law_10(tag)
C0120:					; emit_law_10()
					; tag = 2
;  924:     emit_op_10($6A)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6A			; CB	106
	DB	$54,<C0086,>C0086	; CALL	C0086
;  925:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  926: end
	DB	$5A			; LEAVE
;  927: def emit_sb
C0122:					; emit_sb()
;  928:     emit_op_10($70)
	JSR	_INTERP
	DB	$2A,$70			; CB	112
	DB	$54,<C0086,>C0086	; CALL	C0086
;  929: end
	DB	$5C			; RET
;  930: def emit_sw
C0124:					; emit_sw()
;  931:     emit_op_10($72)
	JSR	_INTERP
	DB	$2A,$72			; CB	114
	DB	$54,<C0086,>C0086	; CALL	C0086
;  932: end
	DB	$5C			; RET
;  933: def emit_slb_10(index)
C0126:					; emit_slb_10()
					; index = 2
;  934:     emit_op_10($74)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$74			; CB	116
	DB	$54,<C0086,>C0086	; CALL	C0086
;  935:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  936: end
	DB	$5A			; LEAVE
;  937: def emit_slw_10(index)
C0128:					; emit_slw_10()
					; index = 2
;  938:     emit_op_10($76)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$76			; CB	118
	DB	$54,<C0086,>C0086	; CALL	C0086
;  939:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  940: end
	DB	$5A			; LEAVE
;  941: def emit_dlb_10(index)
C0130:					; emit_dlb_10()
					; index = 2
;  942:     emit_op_10($6C)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6C			; CB	108
	DB	$54,<C0086,>C0086	; CALL	C0086
;  943:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  944: end
	DB	$5A			; LEAVE
;  945: def emit_dlw_10(index)
C0132:					; emit_dlw_10()
					; index = 2
;  946:     emit_op_10($6E)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6E			; CB	110
	DB	$54,<C0086,>C0086	; CALL	C0086
;  947:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  948: end
	DB	$5A			; LEAVE
;  949: def emit_sab_10(tag)
C0134:					; emit_sab_10()
					; tag = 2
;  950:     emit_op_10($78)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$78			; CB	120
	DB	$54,<C0086,>C0086	; CALL	C0086
;  951:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  952: end
	DB	$5A			; LEAVE
;  953: def emit_saw_10(tag)
C0136:					; emit_saw_10()
					; tag = 2
;  954:     emit_op_10($7A)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7A			; CB	122
	DB	$54,<C0086,>C0086	; CALL	C0086
;  955:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  956: end
	DB	$5A			; LEAVE
;  957: def emit_dab_10(tag)
C0138:					; emit_dab_10()
					; tag = 2
;  958:     emit_op_10($7C)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7C			; CB	124
	DB	$54,<C0086,>C0086	; CALL	C0086
;  959:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  960: end
	DB	$5A			; LEAVE
;  961: def emit_daw_10(tag)
C0140:					; emit_daw_10()
					; tag = 2
;  962:     emit_op_10($7E)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7E			; CB	126
	DB	$54,<C0086,>C0086	; CALL	C0086
;  963:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  964: end
	DB	$5A			; LEAVE
;  965: def emit_call_10(tag)
C0142:					; emit_call_10()
					; tag = 2
;  966:     emit_op_10($54)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$54			; CB	84
	DB	$54,<C0086,>C0086	; CALL	C0086
;  967:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  968: end
	DB	$5A			; LEAVE
;  969: def emit_ical
C0144:					; emit_ical()
;  970:     emit_op_10($56)
	JSR	_INTERP
	DB	$2A,$56			; CB	86
	DB	$54,<C0086,>C0086	; CALL	C0086
;  971: end
	DB	$5C			; RET
;  972: def emit_push
C0146:					; emit_push()
;  973:     emit_op_10($34)
	JSR	_INTERP
	DB	$2A,$34			; CB	52
	DB	$54,<C0086,>C0086	; CALL	C0086
;  974: end
	DB	$5C			; RET
;  975: def emit_pull
C0148:					; emit_pull()
;  976:     ;
;  977:     ; Skip if last op was push
;  978:     ;
;  979:     if lastop == $34
	JSR	_INTERP
	DB	$68,<D0026,>D0026	; LAB	D0026
	DB	$2A,$34			; CB	52
	DB	$40			; ISEQ
	DB	$4C,<C0150,>C0150	; SKPFLS	C0150
;  980:         codeptr = codeptr - 1
	DB	$6A,<D0022,>D0022	; LAW	D0022
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0022,>D0022	; SAW	D0022
;  981:         lastop = $FF
	DB	$2A,$FF			; CB	255
	DB	$78,<D0026,>D0026	; SAB	D0026
;  982:     else
	DB	$50,<C0151,>C0151	; SKIP	C0151
C0150:
;  983:         emit_op_10($36)
	DB	$2A,$36			; CB	54
	DB	$54,<C0086,>C0086	; CALL	C0086
;  984:     fin
C0151:
;  985: end
	DB	$5C			; RET
;  986: def emit_localaddr_10(index)
C0152:					; emit_localaddr_10()
					; index = 2
;  987:     emit_op_10($28)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$28			; CB	40
	DB	$54,<C0086,>C0086	; CALL	C0086
;  988:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  989: end
	DB	$5A			; LEAVE
;  990: def emit_globaladdr_10(tag)
C0154:					; emit_globaladdr_10()
					; tag = 2
;  991:     emit_op_10($26)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$26			; CB	38
	DB	$54,<C0086,>C0086	; CALL	C0086
;  992:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
;  993: end
	DB	$5A			; LEAVE
;  994: def emit_indexbyte
C0156:					; emit_indexbyte()
;  995:     emit_op_10($02)
	JSR	_INTERP
	DB	$2A,$02			; CB	2
	DB	$54,<C0086,>C0086	; CALL	C0086
;  996: end
	DB	$5C			; RET
;  997: def emit_indexword
C0158:					; emit_indexword()
;  998:     emit_op_10($1E)
	JSR	_INTERP
	DB	$2A,$1E			; CB	30
	DB	$54,<C0086,>C0086	; CALL	C0086
;  999: end
	DB	$5C			; RET
; 1000: defopt emit_unaryop_11(op)
C0160:					; emit_unaryop_11()
					; op = 2
; 1001:     when op
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
; 1002:         is NEG_TKN
	DEX
	LDA	#$AD
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0163
:
; 1003:             emit_op_10($10)
	DEX
	LDA	#$10
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1004:         is COMP_TKN
	JMP	C0162
C0163:
	DEX
	LDA	#$A3
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0164
:
; 1005:             emit_op_10($12)
	DEX
	LDA	#$12
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1006:         is LOGIC_NOT_TKN
	JMP	C0162
C0164:
	DEX
	LDA	#$A1
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0165
:
; 1007:             emit_op_10($20)
	DEX
	LDA	#$20
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1008:         is INC_TKN
	JMP	C0162
C0165:
	DEX
	LDA	#$C1
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0166
:
; 1009:             emit_op_10($0C)
	DEX
	LDA	#$0C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1010:         is DEC_TKN
	JMP	C0162
C0166:
	DEX
	LDA	#$C4
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0167
:
; 1011:             emit_op_10($0E)
	DEX
	LDA	#$0E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1012:         is BPTR_TKN
	JMP	C0162
C0167:
	DEX
	LDA	#$DE
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0168
:
; 1013:             emit_op_10($60)
	DEX
	LDA	#$60
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1014:         is WPTR_TKN
	JMP	C0162
C0168:
	DEX
	LDA	#$AA
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0169
:
; 1015:             emit_op_10($62)
	DEX
	LDA	#$62
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1016:         otherwise
	JMP	C0162
C0169:
; 1017:             return FALSE
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 1018:     wend
C0162:
; 1019:     return TRUE
	LDA	#$FF
	STA	ESTKL,X
	STA	ESTKH,X
	JMP	LEAVE
; 1020: end
; 1021: defopt emit_binaryop_11(op)
C0171:					; emit_binaryop_11()
					; op = 2
; 1022:     when op
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
; 1023:         is MUL_TKN
	DEX
	LDA	#$AA
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0174
:
; 1024:             ;
; 1025:             ; Replace MUL 2 with SHL 1
; 1026:             ;
; 1027:             if lastop == $2A and ^(codeptr - 1) == 2 ; CB 2
	DEX
	LDA	D0026
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	JSR	LB
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0175
:
; 1028:                 codeptr = codeptr - 1
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0022
	LDA	ESTKH,X
	STA	D0022+1
; 1029:                 emit_byte_10(1) ; CB 1
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0078
; 1030:                 emit_op_10($1A) ; SHL
	DEX
	LDA	#$1A
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0086
; 1031:             else
	JMP	C0176
C0175:
; 1032:                 emit_op_10($06)
	DEX
	LDA	#$06
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0086
; 1033:             fin
C0176:
; 1034:         is DIV_TKN
	JMP	C0173
C0174:
	DEX
	LDA	#$AF
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0177
:
; 1035:             ;
; 1036:             ; Replace DIV 2 with SHR 1
; 1037:             ;
; 1038:             if lastop == $2A and ^(codeptr - 1) == 2 ; CB 2
	DEX
	LDA	D0026
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	JSR	LB
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0178
:
; 1039:                 codeptr = codeptr - 1
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0022
	LDA	ESTKH,X
	STA	D0022+1
; 1040:                 emit_byte_10(1) ; CB 1
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0078
; 1041:                 emit_op_10($1C) ; SHR
	DEX
	LDA	#$1C
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0086
; 1042:             else
	JMP	C0179
C0178:
; 1043:                 emit_op_10($08)
	DEX
	LDA	#$08
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0086
; 1044:             fin
C0179:
; 1045:         is MOD_TKN
	JMP	C0173
C0177:
	DEX
	LDA	#$A5
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0180
:
; 1046:             emit_op_10($0A)
	DEX
	LDA	#$0A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1047:         is ADD_TKN
	JMP	C0173
C0180:
	DEX
	LDA	#$AB
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0181
:
; 1048:             ;
; 1049:             ; Replace ADD 1 with INCR
; 1050:             ;
; 1051:             if lastop == $2A and ^(codeptr - 1) == 1 ; CB 1
	DEX
	LDA	D0026
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	JSR	LB
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0182
:
; 1052:                 codeptr = codeptr - 2
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0022
	LDA	ESTKH,X
	STA	D0022+1
; 1053:                 emit_op_10($0C) ; INC_OP
	LDA	#$0C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1054:             else
	JMP	C0183
C0182:
; 1055:                 emit_op_10($02)
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0086
; 1056:             fin
C0183:
; 1057:         is SUB_TKN
	JMP	C0173
C0181:
	DEX
	LDA	#$AD
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0184
:
; 1058:             ;
; 1059:             ; Replace SUB 1 with DECR
; 1060:             ;
; 1061:             if lastop == $2A and ^(codeptr - 1)  == 1 ; CB 1
	DEX
	LDA	D0026
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	JSR	LB
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0185
:
; 1062:                 codeptr = codeptr - 2
	DEX
	LDA	D0022
	STA	ESTKL,X
	LDA	D0022+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0022
	LDA	ESTKH,X
	STA	D0022+1
; 1063:                 emit_op_10($0E) ; DEC_OP
	LDA	#$0E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1064:             else
	JMP	C0186
C0185:
; 1065:                 emit_op_10($04)
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0086
; 1066:             fin
C0186:
; 1067:         is SHL_TKN
	JMP	C0173
C0184:
	DEX
	LDA	#$CC
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0187
:
; 1068:             emit_op_10($1A)
	DEX
	LDA	#$1A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1069:         is SHR_TKN
	JMP	C0173
C0187:
	DEX
	LDA	#$D2
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0188
:
; 1070:             emit_op_10($1C)
	DEX
	LDA	#$1C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1071:         is AND_TKN
	JMP	C0173
C0188:
	DEX
	LDA	#$A6
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0189
:
; 1072:             emit_op_10($14)
	DEX
	LDA	#$14
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1073:         is OR_TKN
	JMP	C0173
C0189:
	DEX
	LDA	#$BF
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0190
:
; 1074:             emit_op_10($16)
	DEX
	LDA	#$16
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1075:         is EOR_TKN
	JMP	C0173
C0190:
	DEX
	LDA	#$DE
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0191
:
; 1076:             emit_op_10($18)
	DEX
	LDA	#$18
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1077:         is EQ_TKN
	JMP	C0173
C0191:
	DEX
	LDA	#$C5
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0192
:
; 1078:             emit_op_10($40)
	DEX
	LDA	#$40
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1079:         is NE_TKN
	JMP	C0173
C0192:
	DEX
	LDA	#$D5
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0193
:
; 1080:             emit_op_10($42)
	DEX
	LDA	#$42
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1081:         is GE_TKN
	JMP	C0173
C0193:
	DEX
	LDA	#$C8
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0194
:
; 1082:             emit_op_10($48)
	DEX
	LDA	#$48
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1083:         is LT_TKN
	JMP	C0173
C0194:
	DEX
	LDA	#$BC
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0195
:
; 1084:             emit_op_10($46)
	DEX
	LDA	#$46
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1085:         is GT_TKN
	JMP	C0173
C0195:
	DEX
	LDA	#$BE
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0196
:
; 1086:             emit_op_10($44)
	DEX
	LDA	#$44
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1087:         is LE_TKN
	JMP	C0173
C0196:
	DEX
	LDA	#$C2
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0197
:
; 1088:             emit_op_10($4A)
	DEX
	LDA	#$4A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1089:         is LOGIC_OR_TKN
	JMP	C0173
C0197:
	DEX
	LDA	#$CF
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0198
:
; 1090:             emit_op_10($22)
	DEX
	LDA	#$22
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1091:         is LOGIC_AND_TKN
	JMP	C0173
C0198:
	DEX
	LDA	#$CE
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0199
:
; 1092:             emit_op_10($24)
	DEX
	LDA	#$24
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0086
; 1093:         is COMMA_TKN
	JMP	C0173
C0199:
	DEX
	LDA	#$AC
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0200
:
; 1094:             ; Do nothing except move to next stanza in expression
; 1095:         otherwise
	JMP	C0173
C0200:
; 1096:             return FALSE
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 1097:     wend
C0173:
; 1098:     return TRUE
	LDA	#$FF
	STA	ESTKL,X
	STA	ESTKH,X
	JMP	LEAVE
; 1099: end
; 1100: def emit_brtru_10(tag)
C0202:					; emit_brtru_10()
					; tag = 2
; 1101:     emit_op_10($4E)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$4E			; CB	78
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1102:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
; 1103: end
	DB	$5A			; LEAVE
; 1104: def emit_brfls_10(tag)
C0204:					; emit_brfls_10()
					; tag = 2
; 1105:     emit_op_10($4C)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$4C			; CB	76
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1106:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
; 1107: end
	DB	$5A			; LEAVE
; 1108: def emit_brgt_10(tag)
C0206:					; emit_brgt_10()
					; tag = 2
; 1109:     emit_op_10($3A)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$3A			; CB	58
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1110:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
; 1111: end
	DB	$5A			; LEAVE
; 1112: def emit_brlt_10(tag)
C0208:					; emit_brlt_10()
					; tag = 2
; 1113:     emit_op_10($38)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$38			; CB	56
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1114:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
; 1115: end
	DB	$5A			; LEAVE
; 1116: def emit_brne_10(tag)
C0210:					; emit_brne_10()
					; tag = 2
; 1117:     emit_op_10($3E)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$3E			; CB	62
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1118:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
; 1119: end
	DB	$5A			; LEAVE
; 1120: def emit_jump_10(tag)
C0212:					; emit_jump_10()
					; tag = 2
; 1121:     emit_op_10($50)
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$50			; CB	80
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1122:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0088,>C0088	; CALL	C0088
; 1123: end
	DB	$5A			; LEAVE
; 1124: def emit_drop
C0214:					; emit_drop()
; 1125:     emit_op_10($30)
	JSR	_INTERP
	DB	$2A,$30			; CB	48
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1126: end
	DB	$5C			; RET
; 1127: def emit_swap
C0216:					; emit_swap()
; 1128:     emit_op_10($2E)
	JSR	_INTERP
	DB	$2A,$2E			; CB	46
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1129: end
	DB	$5C			; RET
; 1130: def emit_leave_10(framesize)
C0218:					; emit_leave_10()
					; framesize = 2
; 1131:     if framesize > 2
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$44			; ISGT
	DB	$4C,<C0220,>C0220	; SKPFLS	C0220
; 1132:         emit_op_10($5A)
	DB	$2A,$5A			; CB	90
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1133:     else
	DB	$50,<C0221,>C0221	; SKIP	C0221
C0220:
; 1134:         emit_op_10($5C)
	DB	$2A,$5C			; CB	92
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1135:     fin
C0221:
; 1136: end
	DB	$5A			; LEAVE
; 1137: def emit_enter_20(framesize, cparams)
C0222:					; emit_enter_20()
					; framesize = 2
					; cparams = 4
; 1138:     emit_byte_10($20)
	JSR	_INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$2A,$20			; CB	32
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1139:     emit_byte_10($D0)
	DB	$2A,$D0			; CB	208
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1140:     emit_byte_10($03)
	DB	$2A,$03			; CB	3
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1141:     if framesize > 2
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$44			; ISGT
	DB	$4C,<C0224,>C0224	; SKPFLS	C0224
; 1142:         emit_op_10($58)
	DB	$2A,$58			; CB	88
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1143:         emit_byte_10(framesize)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1144:         emit_byte_10(cparams)
	DB	$66,$04			; LLW	4
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1145:     fin
C0224:
C0225:
; 1146: end
	DB	$5A			; LEAVE
; 1147: def emit_start
C0226:					; emit_start()
; 1148:     ;
; 1149:     ; Save address
; 1150:     ;
; 1151:     entrypoint = codeptr
	JSR	_INTERP
	DB	$6A,<D0022,>D0022	; LAW	D0022
	DB	$7A,<D0024,>D0024	; SAW	D0024
; 1152:     emit_byte_10(emit_start.[0])
	DB	$26,<C0226,>C0226	; LA	C0226
	DB	$00			; ZERO
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1153:     emit_byte_10(emit_start.[1])
	DB	$26,<C0226,>C0226	; LA	C0226
	DB	$2A,$01			; CB	1
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1154:     emit_byte_10(emit_start.[2])
	DB	$26,<C0226,>C0226	; LA	C0226
	DB	$2A,$02			; CB	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0078,>C0078	; CALL	C0078
; 1155: end
	DB	$5C			; RET
; 1156: def emit_exit
C0228:					; emit_exit()
; 1157:     emit_op_10($00)
	JSR	_INTERP
	DB	$00			; ZERO
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1158:     emit_op_10($5C)
	DB	$2A,$5C			; CB	92
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1159: end
	DB	$5C			; RET
; 1160: ;
; 1161: ; Lexical anaylzer
; 1162: ;
; 1163: ;def isalpha_11(c)
; 1164: ;   if c >= 'A' and c <= 'Z'
; 1165: ;       return TRUE
; 1166: ;   elsif c >= 'a' and c <= 'z'
; 1167: ;       return TRUE
; 1168: ;   elsif c == '_'
; 1169: ;       return TRUE
; 1170: ;   fin
; 1171: ;   return FALSE
; 1172: ;end
; 1173: asm isalpha_11
C0230:					; isalpha_11()
; 1174:         LDY     #$00
        LDY     #$00
; 1175:         LDA     ESTKL,X
        LDA     ESTKL,X
; 1176:         CMP     #'A'
        CMP     #'A'
; 1177:         BCC     ISALRET
        BCC     ISALRET
; 1178:         CMP     #'Z'+1
        CMP     #'Z'+1
; 1179:         BCS     :+
        BCS     :+
; 1180:         DEY
        DEY
; 1181:         BNE     ISALRET
        BNE     ISALRET
; 1182: :       CMP     #'a'
:       CMP     #'a'
; 1183:         BCC     ISALRET
        BCC     ISALRET
; 1184:         CMP     #'z'+1
        CMP     #'z'+1
; 1185:         BCS     :+
        BCS     :+
; 1186:         DEY
        DEY
; 1187:         BNE     ISALRET
        BNE     ISALRET
; 1188: :       CMP     #'_'
:       CMP     #'_'
; 1189:         BNE     ISALRET
        BNE     ISALRET
; 1190:         DEY
        DEY
; 1191: ISALRET:
ISALRET:
; 1192:         STY     ESTKL,X
        STY     ESTKL,X
; 1193:         STY     ESTKH,X
        STY     ESTKH,X
; 1194:         RTS
        RTS
; 1195: end
	RTS
; 1196: ;def isnum_11(c)
; 1197: ;   if c >= '0' and c <= '9'
; 1198: ;       return TRUE
; 1199: ;   fin
; 1200: ;   return FALSE
; 1201: ;end
; 1202: asm isnum_11
C0232:					; isnum_11()
; 1203:         LDY     #$00
        LDY     #$00
; 1204:         LDA     ESTKL,X
        LDA     ESTKL,X
; 1205:         CMP     #'0'
        CMP     #'0'
; 1206:         BCC     :+
        BCC     :+
; 1207:         CMP     #'9'+1
        CMP     #'9'+1
; 1208:         BCS     :+
        BCS     :+
; 1209:         DEY
        DEY
; 1210: :       STY     ESTKL,X
:       STY     ESTKL,X
; 1211:         STY     ESTKH,X
        STY     ESTKH,X
; 1212:         RTS
        RTS
; 1213: end
	RTS
; 1214: ;def isalphanum_11(c)
; 1215: ;   if c >= 'A' and c <= 'Z'
; 1216: ;       return TRUE
; 1217: ;   elsif c >= '0' and c <= '9'
; 1218: ;       return TRUE
; 1219: ;   elsif c >= 'a' and c <= 'z'
; 1220: ;       return TRUE
; 1221: ;   elsif c == '_'
; 1222: ;       return TRUE
; 1223: ;   fin
; 1224: ;   return FALSE
; 1225: ;end
; 1226: asm isalphanum_11
C0234:					; isalphanum_11()
; 1227:         LDY     #$00
        LDY     #$00
; 1228:         LDA     ESTKL,X
        LDA     ESTKL,X
; 1229:         CMP     #'0'
        CMP     #'0'
; 1230:         BCC     ISANRET
        BCC     ISANRET
; 1231:         CMP     #'9'+1
        CMP     #'9'+1
; 1232:         BCS     :+
        BCS     :+
; 1233:         DEY
        DEY
; 1234:         BNE     ISANRET
        BNE     ISANRET
; 1235: :       CMP     #'A'
:       CMP     #'A'
; 1236:         BCC     ISANRET
        BCC     ISANRET
; 1237:         CMP     #'Z'+1
        CMP     #'Z'+1
; 1238:         BCS     :+
        BCS     :+
; 1239:         DEY
        DEY
; 1240:         BNE     ISANRET
        BNE     ISANRET
; 1241: :       CMP     #'a'
:       CMP     #'a'
; 1242:         BCC     :+
        BCC     :+
; 1243:         CMP     #'z'+1
        CMP     #'z'+1
; 1244:         BCS     ISANRET
        BCS     ISANRET
; 1245:         DEY
        DEY
; 1246:         BNE     ISANRET
        BNE     ISANRET
; 1247: :       CMP     #'_'
:       CMP     #'_'
; 1248:         BNE     ISANRET
        BNE     ISANRET
; 1249:         DEY
        DEY
; 1250: ISANRET:
ISANRET:
; 1251:         STY     ESTKL,X
        STY     ESTKL,X
; 1252:         STY     ESTKH,X
        STY     ESTKH,X
; 1253:         RTS
        RTS
; 1254: end
	RTS
; 1255: defopt keymatch_21(chrptr, len)
C0236:					; keymatch_21()
					; chrptr = 2
					; len = 4
; 1256:     byte i, keypos
					; i = 6
					; keypos = 7
; 1257: 
; 1258:     keypos = 0
	LDY	#8
	LDA	#2
	JSR	ENTER
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 1259:     while keywrds[keypos] < len
	INX
C0238:
	DEX
	LDA	#<D0075
	STA	ESTKL,X
	LDA	#>D0075
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	ISLT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0239
:
; 1260:         keypos = keypos + keywrds[keypos] + 2
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0075
	STA	ESTKL,X
	LDA	#>D0075
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 1261:     loop
	INX
	JMP	C0238
C0239:
; 1262:     while keywrds[keypos] == len
C0240:
	DEX
	LDA	#<D0075
	STA	ESTKL,X
	LDA	#>D0075
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0241
:
; 1263:         for i = 1 to len
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
C0243:
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	LDA	ESTKH-1,X
	SBC	ESTKH,X
	BPL	:+
	JMP	C0242
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
; 1264:             if toupper_11((chrptr).[i - 1]) <> keywrds[keypos + i]
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	JSR	ADD
	JSR	LB
	JSR	C0020
	DEX
	LDA	#<D0075
	STA	ESTKL,X
	LDA	#>D0075
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	ADD
	JSR	LB
	JSR	ISNE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0244
:
; 1265:                 break
	JMP	C0242
; 1266:             fin
C0244:
C0245:
; 1267:         next
	JMP	C0243
C0242:
; 1268:         if i > len
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	ISGT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0246
:
; 1269:             return keywrds[keypos + keywrds[keypos] + 1]
	DEX
	LDA	#<D0075
	STA	ESTKL,X
	LDA	#>D0075
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0075
	STA	ESTKL,X
	LDA	#>D0075
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	JSR	ADD
	JSR	LB
	JMP	LEAVE
; 1270:         fin
C0246:
C0247:
; 1271:         keypos = keypos + keywrds[keypos] + 2
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0075
	STA	ESTKL,X
	LDA	#>D0075
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 1272:     loop
	INX
	JMP	C0240
C0241:
; 1273:     return ID_TKN
	DEX
	LDA	#$D6
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
; 1274: end
; 1275: defopt scan_01
C0248:					; scan_01()
; 1276:     ;
; 1277:     ; Scan for token based on first character
; 1278:     ;
; 1279:     while ^scanptr and ^scanptr <= ' '
C0250:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	DEX
	LDA	#$20
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISLE
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0251
:
; 1280:         scanptr = scanptr + 1
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1281:     loop
	INX
	JMP	C0250
C0251:
; 1282:     tknptr = scanptr
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDA	ESTKL,X
	STA	D0368
	LDA	ESTKH,X
	STA	D0368+1
; 1283:     if !^scanptr or ^scanptr == ';'
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	NOT
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	DEX
	LDA	#$3B
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	JSR	LOR
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0252
:
; 1284:         if token <> EOF_TKN
	DEX
	LDA	D0364
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISNE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0254
:
; 1285:             token = EOL_TKN
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1286:         fin
	INX
C0254:
C0255:
; 1287:     elsif isalpha_11(^scanptr)
	JMP	C0253
C0252:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	C0230
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0256
:
; 1288:         ;
; 1289:         ; ID,    either variable name or reserved word
; 1290:         ;
; 1291:         repeat
C0258:
; 1292:             scanptr = scanptr + 1
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1293:         until !isalphanum_11(^scanptr)
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	JSR	C0234
	LDY	#$00
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0258
:
C0257:
; 1294:         tknlen = scanptr - tknptr;
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	D0368
	STA	ESTKL,X
	LDA	D0368+1
	STA	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0365
; 1295:         token = keymatch_21(tknptr, tknlen)
	LDA	D0368
	STA	ESTKL,X
	LDA	D0368+1
	STA	ESTKH,X
	DEX
	LDA	D0365
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0236
	LDA	ESTKL,X
	STA	D0364
; 1296:     elsif isnum_11(^scanptr)
	INX
	JMP	C0253
C0256:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	C0232
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0259
:
; 1297:         ;
; 1298:         ; Number constant
; 1299:         ;
; 1300:         token       = INT_TKN
	DEX
	LDA	#$C9
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1301:         constval = 0
	STY	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1302:         repeat
	INX
C0261:
; 1303:             constval = constval * 10 + ^scanptr - '0'
	DEX
	LDA	D0370
	STA	ESTKL,X
	LDA	D0370+1
	STA	ESTKH,X
	DEX
	LDA	#$0A
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	MUL
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$30
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1304:             scanptr  = scanptr + 1
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1305:         until !isnum_11(^scanptr)
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	JSR	C0232
	LDY	#$00
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0261
:
C0260:
; 1306:     elsif ^scanptr == '$'
	JMP	C0253
C0259:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$24
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0262
:
; 1307:         ;
; 1308:         ; Hexadecimal constant
; 1309:         ;
; 1310:         token    = INT_TKN;
	DEX
	LDA	#$C9
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1311:         constval = 0
	STY	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1312:         repeat
	INX
C0264:
; 1313:             scanptr = scanptr + 1
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1314:             if ^scanptr >= '0' and ^scanptr <= '9'
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	DEX
	LDA	#$30
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$39
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISLE
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0265
:
; 1315:                 constval = (constval << 4) + ^scanptr - '0'
	DEX
	LDA	D0370
	STA	ESTKL,X
	LDA	D0370+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SHL
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$30
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1316:             elsif ^scanptr >= 'A' and ^scanptr <= 'F'
	INX
	JMP	C0266
C0265:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$41
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$46
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISLE
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0267
:
; 1317:                 constval = (constval << 4) + ^scanptr - '7'; 'A'-10
	DEX
	LDA	D0370
	STA	ESTKL,X
	LDA	D0370+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SHL
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$37
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1318:             elsif ^scanptr >= 'a' and ^scanptr <= 'f'
	INX
	JMP	C0266
C0267:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$61
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$66
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISLE
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0268
:
; 1319:                 constval = (constval << 4) + ^scanptr - 'W'; 'a'-10
	DEX
	LDA	D0370
	STA	ESTKL,X
	LDA	D0370+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SHL
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$57
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1320:             else
	INX
	JMP	C0266
C0268:
; 1321:                 break;
	JMP	C0263
; 1322:             fin
C0266:
; 1323:         until !^scanptr
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0264
:
C0263:
; 1324:     elsif ^scanptr == $27 ; '
	JMP	C0253
C0262:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$27
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0269
:
; 1325:         ;
; 1326:         ; Character constant
; 1327:         ;
; 1328:         token = CHR_TKN
	DEX
	LDA	#$C3
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1329:         if ^(scanptr + 1) <> $5C ; \
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$5C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISNE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0270
:
; 1330:             constval = ^(scanptr + 1)
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1331:             if ^(scanptr + 2) <> $27 ; '
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$27
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISNE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0272
:
; 1332:                 return parse_err_11(@bad_cnst)
	DEX
	LDA	#<D0425
	STA	ESTKL,X
	LDA	#>D0425
	STA	ESTKH,X
	JSR	C0058
	RTS
; 1333:             fin
C0272:
C0273:
; 1334:             scanptr = scanptr + 3
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1335:         else
	INX
	JMP	C0271
C0270:
; 1336:             when ^(scanptr + 2)
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
; 1337:                 is 'n'
	DEX
	LDA	#$6E
	STA	ESTKL,X
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0275
:
; 1338:                     constval = $0D
	DEX
	LDA	#$0D
	STA	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1339:                 is 'r'
	INX
	JMP	C0274
C0275:
	DEX
	LDA	#$72
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0276
:
; 1340:                     constval = $0A
	DEX
	LDA	#$0A
	STA	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1341:                 is 't'
	INX
	JMP	C0274
C0276:
	DEX
	LDA	#$74
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0277
:
; 1342:                     constval = $09
	DEX
	LDA	#$09
	STA	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1343:                 otherwise
	INX
	JMP	C0274
C0277:
; 1344:                     constval = ^(scanptr + 2)
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1345:             wend
	INX
C0274:
; 1346:             if ^(scanptr + 3) <> $27 ; '
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$27
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISNE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0279
:
; 1347:                 return parse_err_11(@bad_cnst)
	DEX
	LDA	#<D0425
	STA	ESTKL,X
	LDA	#>D0425
	STA	ESTKH,X
	JSR	C0058
	RTS
; 1348:             fin
C0279:
C0280:
; 1349:             scanptr = scanptr + 4
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1350:         fin
	INX
C0271:
; 1351:     elsif ^scanptr == '"'
	JMP	C0253
C0269:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$22
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0281
:
; 1352:         ;
; 1353:         ; String constant
; 1354:         ;
; 1355:         token    = STR_TKN
	DEX
	LDA	#$D3
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1356:         scanptr  = scanptr + 1
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1357:         constval = scanptr
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDA	ESTKL,X
	STA	D0370
	LDA	ESTKH,X
	STA	D0370+1
; 1358:         while ^scanptr and ^scanptr <> '"'
	INX
C0282:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	JSR	LB
	DEX
	LDA	#$22
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISNE
	JSR	LAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0283
:
; 1359:             scanptr = scanptr + 1
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1360:         loop
	INX
	JMP	C0282
C0283:
; 1361:         if !^scanptr
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0284
:
; 1362:             return parse_err_11(@bad_cnst)
	DEX
	LDA	#<D0425
	STA	ESTKL,X
	LDA	#>D0425
	STA	ESTKH,X
	JSR	C0058
	RTS
; 1363:         fin
C0284:
C0285:
; 1364:         scanptr = scanptr + 1
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1365:     else
	INX
	JMP	C0253
C0281:
; 1366:         ;
; 1367:         ; Potential two and three character tokens
; 1368:         ;
; 1369:         when ^scanptr
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
; 1370:             is '>'
	DEX
	LDA	#$3E
	STA	ESTKL,X
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0287
:
; 1371:                 if ^(scanptr + 1) == '>'
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$3E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0288
:
; 1372:                     token   = SHR_TKN
	DEX
	LDA	#$D2
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1373:                     scanptr = scanptr + 2
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1374:                 elsif ^(scanptr + 1) == '='
	INX
	JMP	C0289
C0288:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$3D
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0290
:
; 1375:                     token   = GE_TKN
	DEX
	LDA	#$C8
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1376:                     scanptr = scanptr + 2
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1377:                 else
	INX
	JMP	C0289
C0290:
; 1378:                     token   = GT_TKN
	DEX
	LDA	#$BE
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1379:                     scanptr = scanptr + 1
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1380:                 fin
	INX
C0289:
; 1381:             is '<'
	JMP	C0286
C0287:
	DEX
	LDA	#$3C
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0291
:
; 1382:                 if ^(scanptr + 1) == '<'
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$3C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0292
:
; 1383:                     token   = SHL_TKN
	DEX
	LDA	#$CC
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1384:                     scanptr = scanptr + 2
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1385:                 elsif ^(scanptr + 1) == '='
	INX
	JMP	C0293
C0292:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$3D
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0294
:
; 1386:                     token   = LE_TKN
	DEX
	LDA	#$C2
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1387:                     scanptr = scanptr + 2
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1388:                 elsif ^(scanptr + 1) == '>'
	INX
	JMP	C0293
C0294:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$3E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0295
:
; 1389:                     token   = NE_TKN
	DEX
	LDA	#$D5
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1390:                     scanptr = scanptr + 2
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1391:                 else
	INX
	JMP	C0293
C0295:
; 1392:                     token   = LT_TKN
	DEX
	LDA	#$BC
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1393:                     scanptr = scanptr + 1
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1394:                 fin
	INX
C0293:
; 1395:             is '='
	JMP	C0286
C0291:
	DEX
	LDA	#$3D
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	BNE	:+
	LDA	ESTKH-1,X
	CMP	ESTKH,X
	BEQ	:++
:	JMP	C0296
:
; 1396:                 if ^(scanptr + 1) == '='
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$3D
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0297
:
; 1397:                     token   = EQ_TKN
	DEX
	LDA	#$C5
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1398:                     scanptr = scanptr + 2;
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1399:                 elsif ^(scanptr + 1) == ','
	INX
	JMP	C0298
C0297:
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$2C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0299
:
; 1400:                     token   = SETLIST_TKN
	DEX
	LDA	#$B9
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1401:                     scanptr = scanptr + 2;
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1402:                 else
	INX
	JMP	C0298
C0299:
; 1403:                     token   = SET_TKN;
	DEX
	LDA	#$BD
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0364
; 1404:                     scanptr = scanptr + 1
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1405:                 fin
	INX
C0298:
; 1406:             otherwise
	JMP	C0286
C0296:
; 1407:                 ;
; 1408:                 ; Simple single character tokens
; 1409:                 ;
; 1410:                 token   = ^scanptr ? $80
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$80
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	IOR
	LDA	ESTKL,X
	STA	D0364
; 1411:                 scanptr = scanptr + 1
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0366
	LDA	ESTKH,X
	STA	D0366+1
; 1412:         wend
	INX
C0286:
; 1413:     fin
	INX
C0253:
; 1414:     tknlen = scanptr - tknptr
	DEX
	LDA	D0366
	STA	ESTKL,X
	LDA	D0366+1
	STA	ESTKH,X
	DEX
	LDA	D0368
	STA	ESTKL,X
	LDA	D0368+1
	STA	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0365
; 1415:     return token
	LDA	D0364
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	RTS
; 1416: end
; 1417: def rewind_10(ptr)
C0301:					; rewind_10()
					; ptr = 2
; 1418:     scanptr = ptr
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$7A,<D0366,>D0366	; SAW	D0366
; 1419: end
	DB	$5A			; LEAVE
; 1420: ;
; 1421: ; Get next line of input
; 1422: ;
; 1423: def nextln_01
C0303:					; nextln_01()
; 1424:     byte i, chr
					; i = 2
					; chr = 3
; 1425: 
; 1426:     scanptr = inbuff
	JSR	_INTERP
	DB	$58,$04,$00		; ENTER	4,0
	DB	$2C,$00,$02		; CW	512
	DB	$7A,<D0366,>D0366	; SAW	D0366
; 1427:     ^instr = read_31(inref, inbuff, $7F)
	DB	$2C,$FF,$01		; CW	511
	DB	$68,<D0000,>D0000	; LAB	D0000
	DB	$2C,$00,$02		; CW	512
	DB	$2A,$7F			; CB	127
	DB	$54,<C0032,>C0032	; CALL	C0032
	DB	$70			; SB
; 1428:     inbuff[^instr] = $00
	DB	$2C,$00,$02		; CW	512
	DB	$2C,$FF,$01		; CW	511
	DB	$60			; LB
	DB	$02			; IDXB
	DB	$00			; ZERO
	DB	$70			; SB
; 1429:     if ^instr
	DB	$2C,$FF,$01		; CW	511
	DB	$60			; LB
	DB	$4C,<C0305,>C0305	; SKPFLS	C0305
; 1430:         lineno = lineno + 1
	DB	$6A,<D0372,>D0372	; LAW	D0372
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0372,>D0372	; SAW	D0372
; 1431:         if !(lineno & $0F)
	DB	$6A,<D0372,>D0372	; LAW	D0372
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0307,>C0307	; SKPFLS	C0307
; 1432:             cout('.')
	DB	$2A,$2E			; CB	46
	DB	$54,<C0012,>C0012	; CALL	C0012
; 1433:         fin
C0307:
C0308:
; 1434: ;        cout('>')
; 1435: ;        prstr(instr)
; 1436: ;        crout
; 1437:         drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 1438:     else
	DB	$50,<C0306,>C0306	; SKIP	C0306
C0305:
; 1439:         ^instr  = 0
	DB	$2C,$FF,$01		; CW	511
	DB	$00			; ZERO
	DB	$70			; SB
; 1440:         ^inbuff = $00
	DB	$2C,$00,$02		; CW	512
	DB	$00			; ZERO
	DB	$70			; SB
; 1441:         token   = DONE_TKN
	DB	$2A,$98			; CB	152
	DB	$78,<D0364,>D0364	; SAB	D0364
; 1442:     fin
C0306:
; 1443:     return ^instr
	DB	$2C,$FF,$01		; CW	511
	DB	$60			; LB
	DB	$5A			; LEAVE
; 1444: end
; 1445: ;
; 1446: ; Alebraic op to stack op
; 1447: ;
; 1448: def push_op_21(op, prec)
C0309:					; push_op_21()
					; op = 2
					; prec = 4
; 1449:     opsp = opsp + 1
	JSR	_INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0362,>D0362	; SAW	D0362
; 1450:     if opsp == 16
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$2A,$10			; CB	16
	DB	$40			; ISEQ
	DB	$4C,<C0311,>C0311	; SKPFLS	C0311
; 1451:         return parse_err_11(@estk_overflw)
	DB	$26,<D0525,>D0525	; LA	D0525
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1452:     fin
C0311:
C0312:
; 1453:     opstack[opsp]   = op
	DB	$26,<D0330,>D0330	; LA	D0330
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$70			; SB
; 1454:     precstack[opsp] = prec
	DB	$26,<D0346,>D0346	; LA	D0346
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$02			; IDXB
	DB	$66,$04			; LLW	4
	DB	$70			; SB
; 1455:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1456: end
; 1457: def pop_op_01
C0313:					; pop_op_01()
; 1458:     if opsp < 0
	JSR	_INTERP
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0315,>C0315	; SKPFLS	C0315
; 1459:         return parse_err_11(@estk_underflw)
	DB	$26,<D0545,>D0545	; LA	D0545
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5C			; RET
; 1460:     fin
C0315:
C0316:
; 1461:     opsp = opsp - 1
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0362,>D0362	; SAW	D0362
; 1462:     return opstack[opsp + 1]
	DB	$26,<D0330,>D0330	; LA	D0330
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5C			; RET
; 1463: end
; 1464: def tos_op_01
C0317:					; tos_op_01()
; 1465:     if opsp < 0
	JSR	_INTERP
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0319,>C0319	; SKPFLS	C0319
; 1466:         return 0
	DB	$00			; ZERO
	DB	$5C			; RET
; 1467:     fin
C0319:
C0320:
; 1468:     return opstack[opsp]
	DB	$26,<D0330,>D0330	; LA	D0330
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5C			; RET
; 1469: end
; 1470: def tos_op_prec_11(tos)
C0321:					; tos_op_prec_11()
					; tos = 2
; 1471:     if opsp <= tos
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$66,$02			; LLW	2
	DB	$4A			; ISLE
	DB	$4C,<C0323,>C0323	; SKPFLS	C0323
; 1472:         return 100
	DB	$2A,$64			; CB	100
	DB	$5A			; LEAVE
; 1473:     fin
C0323:
C0324:
; 1474:     return precstack[opsp]
	DB	$26,<D0346,>D0346	; LA	D0346
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5A			; LEAVE
; 1475: end
; 1476: ;
; 1477: ; Symbol table
; 1478: ;
; 1479: defopt idmatch_41(nameptr, len, idptr, idcnt)
C0325:					; idmatch_41()
					; nameptr = 2
					; len = 4
					; idptr = 6
					; idcnt = 8
; 1480:     byte i
					; i = 10
; 1481: 
; 1482:     while idcnt
	LDY	#11
	LDA	#4
	JSR	ENTER
C0327:
	DEX
	LDY	#$08
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0328
:
; 1483:         if len == (idptr).idname
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	JSR	ISEQ
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0329
:
; 1484:             for i = 1 to len
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
C0332:
	LDY	#$0A
	LDA	ESTKL,X
	STA	(FRMP),Y
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	LDA	ESTKH-1,X
	SBC	ESTKH,X
	BPL	:+
	JMP	C0331
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
; 1485:                 if (nameptr).[i - 1] <> (idptr).idname.[i]
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$0A
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	JSR	ADD
	JSR	LB
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	DEX
	LDY	#$0A
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	JSR	ISNE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0333
:
; 1486:                     break
	JMP	C0331
; 1487:                 fin
C0333:
C0334:
; 1488:             next
	JMP	C0332
C0331:
; 1489:             if i > len
	LDY	#$0A
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	ISGT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0335
:
; 1490:                 return idptr
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JMP	LEAVE
; 1491:             fin
C0335:
C0336:
; 1492:         fin
C0329:
C0330:
; 1493:         idptr = idptr + (idptr).idname + idrecsz
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$04
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
; 1494:         idcnt = idcnt - 1
	LDY	#$08
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDY	#$08
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
; 1495:     loop
	INX
	JMP	C0327
C0328:
; 1496:     return 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 1497: end
; 1498: ;def dumpsym_20(idptr, idcnt)
; 1499: ;   while idcnt
; 1500: ;       prword_10((idptr):idval)
; 1501: ;       cout(' ')
; 1502: ;       prbyte_10((idptr).idtype)
; 1503: ;       cout(' ')
; 1504: ;       prstr(@(idptr).idname)
; 1505: ;       cout('=')
; 1506: ;       if (idptr).idtype & ADDR_TYPE
; 1507: ;            if (idptr):idval & is_ctag
; 1508: ;                prword_10(ctag_value:[(idptr):idval & mask_ctag])
; 1509: ;            else
; 1510: ;                prword_10((idptr):idval + compbuff)
; 1511: ;            fin
; 1512: ;        else
; 1513: ;            prword_10((idptr):idval)
; 1514: ;        fin
; 1515: ;       crout()
; 1516: ;       idptr = idptr + (idptr).idname + idrecsz
; 1517: ;       idcnt = idcnt - 1
; 1518: ;   loop
; 1519: ;end
; 1520: def id_lookup_21(nameptr, len)
C0337:					; id_lookup_21()
					; nameptr = 2
					; len = 4
; 1521:     word idptr
					; idptr = 6
; 1522: 
; 1523:     idptr = idmatch_41(nameptr, len, idlocal_tbl, locals)
	JSR	_INTERP
	DB	$58,$08,$02		; ENTER	8,2
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$18		; CW	6144
	DB	$68,<D0015,>D0015	; LAB	D0015
	DB	$54,<C0325,>C0325	; CALL	C0325
	DB	$76,$06			; SLW	6
; 1524:     if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C0339,>C0339	; SKPFLS	C0339
; 1525:         return idptr
	DB	$66,$06			; LLW	6
	DB	$5A			; LEAVE
; 1526:     fin
C0339:
C0340:
; 1527:     idptr = idmatch_41(nameptr, len, idglobal_tbl, globals)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0009,>D0009	; LAW	D0009
	DB	$54,<C0325,>C0325	; CALL	C0325
	DB	$76,$06			; SLW	6
; 1528:     if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C0341,>C0341	; SKPFLS	C0341
; 1529:         return idptr
	DB	$66,$06			; LLW	6
	DB	$5A			; LEAVE
; 1530:     fin
C0341:
C0342:
; 1531:     return parse_err_11(@undecl_id)
	DB	$26,<D0403,>D0403	; LA	D0403
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1532: end
; 1533: def idglobal_lookup_21(nameptr, len)
C0343:					; idglobal_lookup_21()
					; nameptr = 2
					; len = 4
; 1534:     return idmatch_41(nameptr, len, idglobal_tbl, globals)
	JSR	_INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0009,>D0009	; LAW	D0009
	DB	$54,<C0325,>C0325	; CALL	C0325
	DB	$5A			; LEAVE
; 1535: end
; 1536: def idlocal_add_41(namestr, len, type, size)
C0345:					; idlocal_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; size = 8
; 1537:     if idmatch_41(namestr, len, @idlocal_tbl, locals)
	JSR	_INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$18		; CW	6144
	DB	$68,<D0015,>D0015	; LAB	D0015
	DB	$54,<C0325,>C0325	; CALL	C0325
	DB	$4C,<C0347,>C0347	; SKPFLS	C0347
; 1538:         return parse_err_11(@dup_id)
	DB	$26,<D0382,>D0382	; LA	D0382
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1539:     fin
C0347:
C0348:
; 1540:     (lastlocal):idval  = framesize
	DB	$6A,<D0018,>D0018	; LAW	D0018
	DB	$6A,<D0016,>D0016	; LAW	D0016
	DB	$72			; SW
; 1541:     (lastlocal).idtype = type ? LOCAL_TYPE
	DB	$6A,<D0018,>D0018	; LAW	D0018
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$2A,$10			; CB	16
	DB	$16			; IOR
	DB	$70			; SB
; 1542:     nametostr_30(namestr, len, lastlocal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0018,>D0018	; LAW	D0018
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0056,>C0056	; CALL	C0056
; 1543:     locals    = locals + 1
	DB	$68,<D0015,>D0015	; LAB	D0015
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0015,>D0015	; SAB	D0015
; 1544:     lastlocal = lastlocal + idrecsz + len
	DB	$6A,<D0018,>D0018	; LAW	D0018
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0018,>D0018	; SAW	D0018
; 1545:     if lastlocal > idlocal_tbl + idlocal_tblsz
	DB	$6A,<D0018,>D0018	; LAW	D0018
	DB	$2C,$00,$18		; CW	6144
	DB	$2C,$00,$02		; CW	512
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0349,>C0349	; SKPFLS	C0349
; 1546:         prstr(@local_sym_overflw)
	DB	$26,<D0616,>D0616	; LA	D0616
	DB	$54,<C0016,>C0016	; CALL	C0016
; 1547:         exit
	DB	$54,<C0022,>C0022	; CALL	C0022
; 1548:     fin
C0349:
C0350:
; 1549:     framesize = framesize + size
	DB	$6A,<D0016,>D0016	; LAW	D0016
	DB	$66,$08			; LLW	8
	DB	$02			; ADD
	DB	$7A,<D0016,>D0016	; SAW	D0016
; 1550:     if framesize > 255
	DB	$6A,<D0016,>D0016	; LAW	D0016
	DB	$2A,$FF			; CB	255
	DB	$44			; ISGT
	DB	$4C,<C0351,>C0351	; SKPFLS	C0351
; 1551:         prstr(@local_overflw)
	DB	$26,<D0566,>D0566	; LA	D0566
	DB	$54,<C0016,>C0016	; CALL	C0016
; 1552:         return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1553:     fin
C0351:
C0352:
; 1554:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 1555: end
; 1556: def iddata_add_41(namestr, len, type, size)
C0353:					; iddata_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; size = 8
; 1557:     if idmatch_41(namestr, len, idglobal_tbl, globals)
	JSR	_INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0009,>D0009	; LAW	D0009
	DB	$54,<C0325,>C0325	; CALL	C0325
	DB	$4C,<C0355,>C0355	; SKPFLS	C0355
; 1558:         return parse_err_11(@dup_id)
	DB	$26,<D0382,>D0382	; LA	D0382
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1559:     fin
C0355:
C0356:
; 1560:     (lastglobal):idval  = datasize
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$6A,<D0011,>D0011	; LAW	D0011
	DB	$72			; SW
; 1561:     (lastglobal).idtype = type
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$70			; SB
; 1562:     nametostr_30(namestr, len, lastglobal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0056,>C0056	; CALL	C0056
; 1563:     emit_iddata_30(datasize, size, lastglobal + idname)
	DB	$6A,<D0011,>D0011	; LAW	D0011
	DB	$66,$08			; LLW	8
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0094,>C0094	; CALL	C0094
; 1564:     globals    = globals + 1
	DB	$6A,<D0009,>D0009	; LAW	D0009
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0009,>D0009	; SAW	D0009
; 1565:     lastglobal = lastglobal + idrecsz + len
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0013,>D0013	; SAW	D0013
; 1566:     if lastglobal > idglobal_tbl + idglobal_tblsz
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2C,$00,$10		; CW	4096
	DB	$2C,$00,$08		; CW	2048
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0357,>C0357	; SKPFLS	C0357
; 1567:         prstr(@global_sym_overflw)
	DB	$26,<D0587,>D0587	; LA	D0587
	DB	$54,<C0016,>C0016	; CALL	C0016
; 1568:         exit
	DB	$54,<C0022,>C0022	; CALL	C0022
; 1569:     fin
C0357:
C0358:
; 1570:     datasize = datasize + size
	DB	$6A,<D0011,>D0011	; LAW	D0011
	DB	$66,$08			; LLW	8
	DB	$02			; ADD
	DB	$7A,<D0011,>D0011	; SAW	D0011
; 1571:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 1572: end
; 1573: def iddata_size_30(type, varsize, initsize)
C0359:					; iddata_size_30()
					; type = 2
					; varsize = 4
					; initsize = 6
; 1574:     if varsize > initsize
	JSR	_INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$04			; LLW	4
	DB	$66,$06			; LLW	6
	DB	$44			; ISGT
	DB	$4C,<C0361,>C0361	; SKPFLS	C0361
; 1575:         datasize = datasize + emit_data_41(0, 0, 0, varsize - initsize)
	DB	$6A,<D0011,>D0011	; LAW	D0011
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$66,$04			; LLW	4
	DB	$66,$06			; LLW	6
	DB	$04			; SUB
	DB	$54,<C0096,>C0096	; CALL	C0096
	DB	$02			; ADD
	DB	$7A,<D0011,>D0011	; SAW	D0011
; 1576:     else
	DB	$50,<C0362,>C0362	; SKIP	C0362
C0361:
; 1577:         datasize = datasize + initsize
	DB	$6A,<D0011,>D0011	; LAW	D0011
	DB	$66,$06			; LLW	6
	DB	$02			; ADD
	DB	$7A,<D0011,>D0011	; SAW	D0011
; 1578:     fin
C0362:
; 1579: ;   if datasize <> codeptr - compbuff
; 1580: ;       prstr(@emiterr)
; 1581: ;       keyin_01()
; 1582: ;   fin
; 1583: end
	DB	$5A			; LEAVE
; 1584: def idglobal_add_41(namestr, len, type, value)
C0363:					; idglobal_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; value = 8
; 1585:     if idmatch_41(namestr, len, idglobal_tbl, globals)
	JSR	_INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0009,>D0009	; LAW	D0009
	DB	$54,<C0325,>C0325	; CALL	C0325
	DB	$4C,<C0365,>C0365	; SKPFLS	C0365
; 1586:         return parse_err_11(@dup_id)
	DB	$26,<D0382,>D0382	; LA	D0382
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1587:     fin
C0365:
C0366:
; 1588:     (lastglobal):idval  = value
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$66,$08			; LLW	8
	DB	$72			; SW
; 1589:     (lastglobal).idtype = type
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$70			; SB
; 1590:     nametostr_30(namestr, len, lastglobal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0056,>C0056	; CALL	C0056
; 1591:     globals    = globals + 1
	DB	$6A,<D0009,>D0009	; LAW	D0009
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0009,>D0009	; SAW	D0009
; 1592:     lastglobal = lastglobal + idrecsz + len
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0013,>D0013	; SAW	D0013
; 1593:     if lastglobal > idglobal_tbl + idglobal_tblsz
	DB	$6A,<D0013,>D0013	; LAW	D0013
	DB	$2C,$00,$10		; CW	4096
	DB	$2C,$00,$08		; CW	2048
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0367,>C0367	; SKPFLS	C0367
; 1594:         prstr(@global_sym_overflw)
	DB	$26,<D0587,>D0587	; LA	D0587
	DB	$54,<C0016,>C0016	; CALL	C0016
; 1595:         exit
	DB	$54,<C0022,>C0022	; CALL	C0022
; 1596:     fin
C0367:
C0368:
; 1597:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 1598: end
; 1599: def idfunc_add_31(namestr, len, tag)
C0369:					; idfunc_add_31()
					; namestr = 2
					; len = 4
					; tag = 6
; 1600:     return idglobal_add_41(namestr, len, FUNC_TYPE, tag)
	JSR	_INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2A,$08			; CB	8
	DB	$66,$06			; LLW	6
	DB	$54,<C0363,>C0363	; CALL	C0363
	DB	$5A			; LEAVE
; 1601: end
; 1602: def idconst_add_31(namestr, len, value)
C0371:					; idconst_add_31()
					; namestr = 2
					; len = 4
					; value = 6
; 1603:     return idglobal_add_41(namestr, len, CONST_TYPE, value)
	JSR	_INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2A,$01			; CB	1
	DB	$66,$06			; LLW	6
	DB	$54,<C0363,>C0363	; CALL	C0363
	DB	$5A			; LEAVE
; 1604: end
; 1605: def idglobal_init
C0373:					; idglobal_init()
; 1606:     word ctag
					; ctag = 2
; 1607: 
; 1608:     lineno       = 0
	JSR	_INTERP
	DB	$58,$04,$00		; ENTER	4,0
	DB	$00			; ZERO
	DB	$7A,<D0372,>D0372	; SAW	D0372
; 1609:     codeptr      = compbuff
	DB	$2C,$00,$60		; CW	24576
	DB	$7A,<D0022,>D0022	; SAW	D0022
; 1610:     lastop       = $FF
	DB	$2A,$FF			; CB	255
	DB	$78,<D0026,>D0026	; SAB	D0026
; 1611:     entrypoint   = 0
	DB	$00			; ZERO
	DB	$7A,<D0024,>D0024	; SAW	D0024
; 1612:     datasize     = 0
	DB	$00			; ZERO
	DB	$7A,<D0011,>D0011	; SAW	D0011
; 1613:     globals      = 0
	DB	$00			; ZERO
	DB	$7A,<D0009,>D0009	; SAW	D0009
; 1614:     lastglobal   = idglobal_tbl
	DB	$2C,$00,$10		; CW	4096
	DB	$7A,<D0013,>D0013	; SAW	D0013
; 1615:     codetag      = -1
	DB	$2C,$FF,$FF		; CW	-1
	DB	$7A,<D0020,>D0020	; SAW	D0020
; 1616:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1617:     drop idfunc_add_31(@runtime0 + 1, runtime0, ctag)
	DB	$26,<D0800,>D0800	; LA	D0800
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0800,>D0800	; LAB	D0800
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1618:     drop idfunc_add_31(@RUNTIME0 + 1, RUNTIME0, ctag)
	DB	$26,<D0808,>D0808	; LA	D0808
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0808,>D0808	; LAB	D0808
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1619:     drop ctag_resolve_21(ctag, @romcall)
	DB	$66,$02			; LLW	2
	DB	$26,<C0004,>C0004	; LA	C0004
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1620:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1621:     drop idfunc_add_31(@runtime1 + 1, runtime1, ctag)
	DB	$26,<D0816,>D0816	; LA	D0816
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0816,>D0816	; LAB	D0816
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1622:     drop idfunc_add_31(@RUNTIME1 + 1, RUNTIME1, ctag)
	DB	$26,<D0824,>D0824	; LA	D0824
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0824,>D0824	; LAB	D0824
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1623:     drop ctag_resolve_21(ctag, @syscall)
	DB	$66,$02			; LLW	2
	DB	$26,<C0006,>C0006	; LA	C0006
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1624:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1625:     drop idfunc_add_31(@runtime2 + 1, runtime2, ctag)
	DB	$26,<D0832,>D0832	; LA	D0832
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0832,>D0832	; LAB	D0832
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1626:     drop idfunc_add_31(@RUNTIME2 + 1, RUNTIME2, ctag)
	DB	$26,<D0839,>D0839	; LA	D0839
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0839,>D0839	; LAB	D0839
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1627:     drop ctag_resolve_21(ctag, @memset)
	DB	$66,$02			; LLW	2
	DB	$26,<C0008,>C0008	; LA	C0008
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1628:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1629:     drop idfunc_add_31(@runtime3 + 1, runtime3, ctag)
	DB	$26,<D0846,>D0846	; LA	D0846
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0846,>D0846	; LAB	D0846
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1630:     drop idfunc_add_31(@RUNTIME3 + 1, RUNTIME3, ctag)
	DB	$26,<D0853,>D0853	; LA	D0853
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0853,>D0853	; LAB	D0853
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1631:     drop ctag_resolve_21(ctag, @memcpy)
	DB	$66,$02			; LLW	2
	DB	$26,<C0010,>C0010	; LA	C0010
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1632:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1633:     drop idfunc_add_31(@runtime4 + 1, runtime4, ctag)
	DB	$26,<D0860,>D0860	; LA	D0860
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0860,>D0860	; LAB	D0860
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1634:     drop idfunc_add_31(@RUNTIME4 + 1, RUNTIME4, ctag)
	DB	$26,<D0865,>D0865	; LA	D0865
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0865,>D0865	; LAB	D0865
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1635:     drop ctag_resolve_21(ctag, @cout)
	DB	$66,$02			; LLW	2
	DB	$26,<C0012,>C0012	; LA	C0012
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1636:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1637:     drop idfunc_add_31(@runtime5 + 1, runtime5, ctag)
	DB	$26,<D0870,>D0870	; LA	D0870
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0870,>D0870	; LAB	D0870
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1638:     drop idfunc_add_31(@RUNTIME5 + 1, RUNTIME5, ctag)
	DB	$26,<D0874,>D0874	; LA	D0874
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0874,>D0874	; LAB	D0874
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1639:     drop ctag_resolve_21(ctag, @cin)
	DB	$66,$02			; LLW	2
	DB	$26,<C0014,>C0014	; LA	C0014
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1640:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1641:     drop idfunc_add_31(@runtime6 + 1, runtime6, ctag)
	DB	$26,<D0878,>D0878	; LA	D0878
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0878,>D0878	; LAB	D0878
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1642:     drop idfunc_add_31(@RUNTIME6 + 1, RUNTIME6, ctag)
	DB	$26,<D0884,>D0884	; LA	D0884
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0884,>D0884	; LAB	D0884
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1643:     drop ctag_resolve_21(ctag, @prstr)
	DB	$66,$02			; LLW	2
	DB	$26,<C0016,>C0016	; LA	C0016
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1644:     ctag = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$02			; SLW	2
; 1645:     drop idfunc_add_31(@runtime7 + 1, runtime7, ctag)
	DB	$26,<D0890,>D0890	; LA	D0890
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0890,>D0890	; LAB	D0890
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1646:     drop idfunc_add_31(@RUNTIME7 + 1, RUNTIME7, ctag)
	DB	$26,<D0896,>D0896	; LA	D0896
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0896,>D0896	; LAB	D0896
	DB	$66,$02			; LLW	2
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 1647:     drop ctag_resolve_21(ctag, @rdstr)
	DB	$66,$02			; LLW	2
	DB	$26,<C0018,>C0018	; LA	C0018
	DB	$54,<C0072,>C0072	; CALL	C0072
	DB	$30			; DROP
; 1648: end
	DB	$5A			; LEAVE
; 1649: def idlocal_init
C0375:					; idlocal_init()
; 1650:     locals    = 0
	JSR	_INTERP
	DB	$00			; ZERO
	DB	$78,<D0015,>D0015	; SAB	D0015
; 1651:     framesize = 2
	DB	$2A,$02			; CB	2
	DB	$7A,<D0016,>D0016	; SAW	D0016
; 1652:     lastlocal = idlocal_tbl
	DB	$2C,$00,$18		; CW	6144
	DB	$7A,<D0018,>D0018	; SAW	D0018
; 1653: end
	DB	$5C			; RET
; 1654: ;
; 1655: ; Parser
; 1656: ;
; 1657: def parse_term_01
C0377:					; parse_term_01()
; 1658:     when scan_01()
	JSR	_INTERP
	DB	$54,<C0248,>C0248	; CALL	C0248
; 1659:         is ID_TKN
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0380,>C0380	; SKPNE	C0380
; 1660:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1661:         is INT_TKN
	DB	$50,<C0379,>C0379	; SKIP	C0379
C0380:
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0381,>C0381	; SKPNE	C0381
; 1662:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1663:         is CHR_TKN
	DB	$50,<C0379,>C0379	; SKIP	C0379
C0381:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0382,>C0382	; SKPNE	C0382
; 1664:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1665:         is STR_TKN
	DB	$50,<C0379,>C0379	; SKIP	C0379
C0382:
	DB	$2A,$D3			; CB	211
	DB	$3E,<C0383,>C0383	; SKPNE	C0383
; 1666:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1667:         is OPEN_PAREN_TKN
	DB	$50,<C0379,>C0379	; SKIP	C0379
C0383:
	DB	$2A,$A8			; CB	168
	DB	$3E,<C0384,>C0384	; SKPNE	C0384
; 1668:             if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0385,>C0385	; SKPFLS	C0385
; 1669:                 return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5C			; RET
; 1670:             fin
C0385:
C0386:
; 1671:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C0387,>C0387	; SKPFLS	C0387
; 1672:                 return parse_err_11(@no_close_paren)
	DB	$30			; DROP
	DB	$26,<D0664,>D0664	; LA	D0664
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5C			; RET
; 1673:             fin
C0387:
C0388:
; 1674:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1675:     wend
	DB	$50,<C0379,>C0379	; SKIP	C0379
C0384:
C0379:
	DB	$30			; DROP
; 1676:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 1677: end
; 1678: def parse_constval_21(valptr, sizeptr)
C0390:					; parse_constval_21()
					; valptr = 2
					; sizeptr = 4
; 1679:     byte mod, type
					; mod = 6
					; type = 7
; 1680:     word idptr
					; idptr = 8
; 1681: 
; 1682:     mod         = 0
	JSR	_INTERP
	DB	$58,$0A,$02		; ENTER	10,2
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 1683:     type        = 0
	DB	$00			; ZERO
	DB	$74,$07			; SLB	7
; 1684:     *valptr = 0
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$72			; SW
; 1685:     while !parse_term_01()
C0392:
	DB	$54,<C0377,>C0377	; CALL	C0377
	DB	$20			; NOT
	DB	$4C,<C0393,>C0393	; SKPFLS	C0393
; 1686:         when token
	DB	$68,<D0364,>D0364	; LAB	D0364
; 1687:             is SUB_TKN
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0395,>C0395	; SKPNE	C0395
; 1688:                 mod = mod ? 1
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1689:             is COMP_TKN
	DB	$50,<C0394,>C0394	; SKIP	C0394
C0395:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0396,>C0396	; SKPNE	C0396
; 1690:                 mod = mod ? 2
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1691:             is LOGIC_NOT_TKN
	DB	$50,<C0394,>C0394	; SKIP	C0394
C0396:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0397,>C0397	; SKPNE	C0397
; 1692:                 mod = mod ? 4
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1693:             is AT_TKN
	DB	$50,<C0394,>C0394	; SKIP	C0394
C0397:
	DB	$2A,$C0			; CB	192
	DB	$3E,<C0398,>C0398	; SKPNE	C0398
; 1694:                 mod = mod ? 8
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1695:             otherwise
	DB	$50,<C0394,>C0394	; SKIP	C0394
C0398:
; 1696:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1697:         wend
C0394:
	DB	$30			; DROP
; 1698:     loop
	DB	$50,<C0392,>C0392	; SKIP	C0392
C0393:
; 1699:     when token
	DB	$68,<D0364,>D0364	; LAB	D0364
; 1700:         is STR_TKN
	DB	$2A,$D3			; CB	211
	DB	$3E,<C0401,>C0401	; SKPNE	C0401
; 1701:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0370,>D0370	; LAW	D0370
	DB	$72			; SW
; 1702:             ^sizeptr = tknlen - 1
	DB	$66,$04			; LLW	4
	DB	$68,<D0365,>D0365	; LAB	D0365
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
; 1703:             type         = STR_TYPE
	DB	$2A,$80			; CB	128
	DB	$74,$07			; SLB	7
; 1704:             if mod
	DB	$64,$06			; LLB	6
	DB	$4C,<C0402,>C0402	; SKPFLS	C0402
; 1705:                 return parse_err_11(@bad_op)
	DB	$30			; DROP
	DB	$26,<D0472,>D0472	; LA	D0472
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1706:             fin
C0402:
C0403:
; 1707:         is CHR_TKN
	DB	$50,<C0400,>C0400	; SKIP	C0400
C0401:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0404,>C0404	; SKPNE	C0404
; 1708:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0370,>D0370	; LAW	D0370
	DB	$72			; SW
; 1709:             ^sizeptr = 1
	DB	$66,$04			; LLW	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
; 1710:             type         = BYTE_TYPE
	DB	$2A,$02			; CB	2
	DB	$74,$07			; SLB	7
; 1711:         is INT_TKN
	DB	$50,<C0400,>C0400	; SKIP	C0400
C0404:
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0405,>C0405	; SKPNE	C0405
; 1712:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0370,>D0370	; LAW	D0370
	DB	$72			; SW
; 1713:             ^sizeptr = 2
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$70			; SB
; 1714:             type         = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$07			; SLB	7
; 1715:         is ID_TKN
	DB	$50,<C0400,>C0400	; SKIP	C0400
C0405:
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0406,>C0406	; SKPNE	C0406
; 1716:             ^sizeptr = 2
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$70			; SB
; 1717:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0368,>D0368	; LAW	D0368
	DB	$68,<D0365,>D0365	; LAB	D0365
	DB	$54,<C0337,>C0337	; CALL	C0337
	DB	$76,$08			; SLW	8
; 1718:             if !idptr
	DB	$66,$08			; LLW	8
	DB	$20			; NOT
	DB	$4C,<C0407,>C0407	; SKPFLS	C0407
; 1719:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0425,>D0425	; LA	D0425
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1720:             fin
C0407:
C0408:
; 1721:             type    = (idptr).idtype
	DB	$66,$08			; LLW	8
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$07			; SLB	7
; 1722:             *valptr = (idptr):idval
	DB	$66,$02			; LLW	2
	DB	$66,$08			; LLW	8
	DB	$62			; LW
	DB	$72			; SW
; 1723:             if type & VAR_TYPE and !(mod & 8)
	DB	$64,$07			; LLB	7
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$24			; LAND
	DB	$4C,<C0409,>C0409	; SKPFLS	C0409
; 1724:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0425,>D0425	; LA	D0425
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1725:             fin
C0409:
C0410:
; 1726:         otherwise
	DB	$50,<C0400,>C0400	; SKIP	C0400
C0406:
; 1727:             return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0425,>D0425	; LA	D0425
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1728:     wend
C0400:
	DB	$30			; DROP
; 1729:     if mod & 1
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0412,>C0412	; SKPFLS	C0412
; 1730:         *valptr = -*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$10			; NEG
	DB	$72			; SW
; 1731:     fin
C0412:
C0413:
; 1732:     if mod & 2
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0414,>C0414	; SKPFLS	C0414
; 1733:         *valptr = #*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$12			; COMP
	DB	$72			; SW
; 1734:     fin
C0414:
C0415:
; 1735:     if mod & 4
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0416,>C0416	; SKPFLS	C0416
; 1736:         *valptr = !*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$20			; NOT
	DB	$72			; SW
; 1737:     fin
C0416:
C0417:
; 1738:     return type
	DB	$64,$07			; LLB	7
	DB	$5A			; LEAVE
; 1739: end
; 1740: def ispostop_01
C0418:					; ispostop_01()
; 1741:     when token
	JSR	_INTERP
	DB	$68,<D0364,>D0364	; LAB	D0364
; 1742:         is OPEN_PAREN_TKN
	DB	$2A,$A8			; CB	168
	DB	$3E,<C0421,>C0421	; SKPNE	C0421
; 1743:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1744:         is OPEN_BRACKET_TKN
	DB	$50,<C0420,>C0420	; SKIP	C0420
C0421:
	DB	$2A,$DB			; CB	219
	DB	$3E,<C0422,>C0422	; SKPNE	C0422
; 1745:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1746:         is DOT_TKN
	DB	$50,<C0420,>C0420	; SKIP	C0420
C0422:
	DB	$2A,$AE			; CB	174
	DB	$3E,<C0423,>C0423	; SKPNE	C0423
; 1747:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1748:         is COLON_TKN
	DB	$50,<C0420,>C0420	; SKIP	C0420
C0423:
	DB	$2A,$BA			; CB	186
	DB	$3E,<C0424,>C0424	; SKPNE	C0424
; 1749:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1750:     wend
	DB	$50,<C0420,>C0420	; SKIP	C0420
C0424:
C0420:
	DB	$30			; DROP
; 1751:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 1752: end
; 1753: def parse_value_11(rvalue)
C0426:					; parse_value_11()
					; rvalue = 2
; 1754:     byte cparams, deref, type, emit_val
					; cparams = 4
					; deref = 5
					; type = 6
					; emit_val = 7
; 1755:     word optos, idptr, value
					; optos = 8
					; idptr = 10
					; value = 12
; 1756:     byte elem_type, elem_size
					; elem_type = 14
					; elem_size = 15
; 1757:     word elem_offset
					; elem_offset = 16
; 1758: 
; 1759:     deref    = rvalue
	JSR	_INTERP
	DB	$58,$12,$01		; ENTER	18,1
	DB	$66,$02			; LLW	2
	DB	$74,$05			; SLB	5
; 1760:     optos    = opsp
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$76,$08			; SLW	8
; 1761:     type     = 0
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 1762:     emit_val = 0
	DB	$00			; ZERO
	DB	$74,$07			; SLB	7
; 1763:     value    = 0
	DB	$00			; ZERO
	DB	$76,$0C			; SLW	12
; 1764: 
; 1765:     ;
; 1766:     ; Parse pre-ops
; 1767:     ;
; 1768:     while !parse_term_01()
C0428:
	DB	$54,<C0377,>C0377	; CALL	C0377
	DB	$20			; NOT
	DB	$4C,<C0429,>C0429	; SKPFLS	C0429
; 1769:         when token
	DB	$68,<D0364,>D0364	; LAB	D0364
; 1770:             is ADD_TKN
	DB	$2A,$AB			; CB	171
	DB	$3E,<C0431,>C0431	; SKPNE	C0431
; 1771:             is BPTR_TKN
	DB	$50,<C0430,>C0430	; SKIP	C0430
C0431:
	DB	$2A,$DE			; CB	222
	DB	$3E,<C0432,>C0432	; SKPNE	C0432
; 1772:                 if deref
	DB	$64,$05			; LLB	5
	DB	$4C,<C0433,>C0433	; SKPFLS	C0433
; 1773:                     drop push_op_21(token, 0)
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$00			; ZERO
	DB	$54,<C0309,>C0309	; CALL	C0309
	DB	$30			; DROP
; 1774:                 else
	DB	$50,<C0434,>C0434	; SKIP	C0434
C0433:
; 1775:                     type = type ? BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1776:                     deref = deref + 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$05			; SLB	5
; 1777:                 fin
C0434:
; 1778:             is WPTR_TKN
	DB	$50,<C0430,>C0430	; SKIP	C0430
C0432:
	DB	$2A,$AA			; CB	170
	DB	$3E,<C0435,>C0435	; SKPNE	C0435
; 1779:                 if deref
	DB	$64,$05			; LLB	5
	DB	$4C,<C0436,>C0436	; SKPFLS	C0436
; 1780:                     drop push_op_21(token, 0)
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$00			; ZERO
	DB	$54,<C0309,>C0309	; CALL	C0309
	DB	$30			; DROP
; 1781:                 else
	DB	$50,<C0437,>C0437	; SKIP	C0437
C0436:
; 1782:                     type = type ? WPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$40			; CB	64
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1783:                     deref = deref + 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$05			; SLB	5
; 1784:                 fin
C0437:
; 1785:             is AT_TKN
	DB	$50,<C0430,>C0430	; SKIP	C0430
C0435:
	DB	$2A,$C0			; CB	192
	DB	$3E,<C0438,>C0438	; SKPNE	C0438
; 1786:                 deref = deref - 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$05			; SLB	5
; 1787:             is SUB_TKN
	DB	$50,<C0430,>C0430	; SKIP	C0430
C0438:
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0439,>C0439	; SKPNE	C0439
; 1788:                 drop push_op_21(token, 0)
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$00			; ZERO
	DB	$54,<C0309,>C0309	; CALL	C0309
	DB	$30			; DROP
; 1789:             is COMP_TKN
	DB	$50,<C0430,>C0430	; SKIP	C0430
C0439:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0440,>C0440	; SKPNE	C0440
; 1790:                 drop push_op_21(token, 0)
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$00			; ZERO
	DB	$54,<C0309,>C0309	; CALL	C0309
	DB	$30			; DROP
; 1791:             is LOGIC_NOT_TKN
	DB	$50,<C0430,>C0430	; SKIP	C0430
C0440:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0441,>C0441	; SKPNE	C0441
; 1792:                 drop push_op_21(token, 0)
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$00			; ZERO
	DB	$54,<C0309,>C0309	; CALL	C0309
	DB	$30			; DROP
; 1793:             otherwise
	DB	$50,<C0430,>C0430	; SKIP	C0430
C0441:
; 1794:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1795:         wend
C0430:
	DB	$30			; DROP
; 1796:     loop
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0429:
; 1797:     ;
; 1798:     ; Determine terminal type
; 1799:     ;
; 1800:     when token
	DB	$68,<D0364,>D0364	; LAB	D0364
; 1801:         is INT_TKN
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0444,>C0444	; SKPNE	C0444
; 1802:             type  = type ? CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1803:             value = constval
	DB	$6A,<D0370,>D0370	; LAW	D0370
	DB	$76,$0C			; SLW	12
; 1804:         is CHR_TKN
	DB	$50,<C0443,>C0443	; SKIP	C0443
C0444:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0445,>C0445	; SKPNE	C0445
; 1805:             type  = type ? CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1806:             value = constval
	DB	$6A,<D0370,>D0370	; LAW	D0370
	DB	$76,$0C			; SLW	12
; 1807:         is ID_TKN
	DB	$50,<C0443,>C0443	; SKIP	C0443
C0445:
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0446,>C0446	; SKPNE	C0446
; 1808:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0368,>D0368	; LAW	D0368
	DB	$68,<D0365,>D0365	; LAB	D0365
	DB	$54,<C0337,>C0337	; CALL	C0337
	DB	$76,$0A			; SLW	10
; 1809:             if !idptr
	DB	$66,$0A			; LLW	10
	DB	$20			; NOT
	DB	$4C,<C0447,>C0447	; SKPFLS	C0447
; 1810:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1811:             fin
C0447:
C0448:
; 1812:             if !(idptr).idtype
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$20			; NOT
	DB	$4C,<C0449,>C0449	; SKPFLS	C0449
; 1813:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1814:             fin
C0449:
C0450:
; 1815:             type  = type ? (idptr).idtype
	DB	$64,$06			; LLB	6
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1816:             value = (idptr):idval
	DB	$66,$0A			; LLW	10
	DB	$62			; LW
	DB	$76,$0C			; SLW	12
; 1817:         is CLOSE_PAREN_TKN
	DB	$50,<C0443,>C0443	; SKIP	C0443
C0446:
	DB	$2A,$A9			; CB	169
	DB	$3E,<C0451,>C0451	; SKPNE	C0451
; 1818:             type     = type ? WORD_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1819:             emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1820:         otherwise
	DB	$50,<C0443,>C0443	; SKIP	C0443
C0451:
; 1821:             return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1822:     wend
C0443:
	DB	$30			; DROP
; 1823:     ;
; 1824:     ; Constant optimizations
; 1825:     ;
; 1826:     if type & CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0453,>C0453	; SKPFLS	C0453
; 1827:         cparams = TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$74,$04			; SLB	4
; 1828:         while optos < opsp and cparams
C0455:
	DB	$66,$08			; LLW	8
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$46			; ISLT
	DB	$64,$04			; LLB	4
	DB	$24			; LAND
	DB	$4C,<C0456,>C0456	; SKPFLS	C0456
; 1829:             when tos_op_01()
	DB	$54,<C0317,>C0317	; CALL	C0317
; 1830:                 is NEG_TKN
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0458,>C0458	; SKPNE	C0458
; 1831:                     drop pop_op_01()
	DB	$54,<C0313,>C0313	; CALL	C0313
	DB	$30			; DROP
; 1832:                     value = -value
	DB	$66,$0C			; LLW	12
	DB	$10			; NEG
	DB	$76,$0C			; SLW	12
; 1833:                 is COMP_TKN
	DB	$50,<C0457,>C0457	; SKIP	C0457
C0458:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0459,>C0459	; SKPNE	C0459
; 1834:                     drop pop_op_01()
	DB	$54,<C0313,>C0313	; CALL	C0313
	DB	$30			; DROP
; 1835:                     value = #value
	DB	$66,$0C			; LLW	12
	DB	$12			; COMP
	DB	$76,$0C			; SLW	12
; 1836:                 is LOGIC_NOT_TKN
	DB	$50,<C0457,>C0457	; SKIP	C0457
C0459:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0460,>C0460	; SKPNE	C0460
; 1837:                     drop pop_op_01()
	DB	$54,<C0313,>C0313	; CALL	C0313
	DB	$30			; DROP
; 1838:                     value = !value
	DB	$66,$0C			; LLW	12
	DB	$20			; NOT
	DB	$76,$0C			; SLW	12
; 1839:                 otherwise
	DB	$50,<C0457,>C0457	; SKIP	C0457
C0460:
; 1840:                     cparams = FALSE
	DB	$00			; ZERO
	DB	$74,$04			; SLB	4
; 1841:             wend
C0457:
	DB	$30			; DROP
; 1842:         loop
	DB	$50,<C0455,>C0455	; SKIP	C0455
C0456:
; 1843:     fin
C0453:
C0454:
; 1844:     ;
; 1845:     ; Parse post-ops
; 1846:     ;
; 1847:     drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 1848:     while ispostop_01()
C0462:
	DB	$54,<C0418,>C0418	; CALL	C0418
	DB	$4C,<C0463,>C0463	; SKPFLS	C0463
; 1849:         if token == OPEN_BRACKET_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C0464,>C0464	; SKPFLS	C0464
; 1850:             ;
; 1851:             ; Array
; 1852:             ;
; 1853:             if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0466,>C0466	; SKPFLS	C0466
; 1854:                 if type & ADDR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C0468,>C0468	; SKPFLS	C0468
; 1855:                     if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0470,>C0470	; SKPFLS	C0470
; 1856:                         emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 1857:                     else
	DB	$50,<C0471,>C0471	; SKIP	C0471
C0470:
; 1858:                         emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0154,>C0154	; CALL	C0154
; 1859:                     fin
C0471:
; 1860:                 elsif type & CONST_TYPE
	DB	$50,<C0469,>C0469	; SKIP	C0469
C0468:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0472,>C0472	; SKPFLS	C0472
; 1861:                     emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1862:                 fin
C0472:
C0469:
; 1863:                 emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1864:             fin ; !emit_val
C0466:
C0467:
; 1865:             if type & PTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$4C,<C0473,>C0473	; SKPFLS	C0473
; 1866:                 emit_lw()
	DB	$54,<C0112,>C0112	; CALL	C0112
; 1867:             fin
C0473:
C0474:
; 1868:             if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0475,>C0475	; SKPFLS	C0475
; 1869:                 return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1870:             fin
C0475:
C0476:
; 1871:             if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C0477,>C0477	; SKPFLS	C0477
; 1872:                 return parse_err_11(@no_close_bracket)
	DB	$26,<D0686,>D0686	; LA	D0686
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1873:             fin
C0477:
C0478:
; 1874:             if type & WORD_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0479,>C0479	; SKPFLS	C0479
; 1875:                 type = WPTR_TYPE
	DB	$2A,$40			; CB	64
	DB	$74,$06			; SLB	6
; 1876:                 emit_indexword()
	DB	$54,<C0158,>C0158	; CALL	C0158
; 1877:             else
	DB	$50,<C0480,>C0480	; SKIP	C0480
C0479:
; 1878:                 type = BPTR_TYPE
	DB	$2A,$20			; CB	32
	DB	$74,$06			; SLB	6
; 1879:                 emit_indexbyte()
	DB	$54,<C0156,>C0156	; CALL	C0156
; 1880:             fin
C0480:
; 1881:             drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 1882:         elsif token == DOT_TKN or token == COLON_TKN
	DB	$50,<C0465,>C0465	; SKIP	C0465
C0464:
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$AE			; CB	174
	DB	$40			; ISEQ
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$BA			; CB	186
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0481,>C0481	; SKPFLS	C0481
; 1883:             ;
; 1884:             ; Dot and Colon
; 1885:             ;
; 1886:             if token == DOT_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$AE			; CB	174
	DB	$40			; ISEQ
	DB	$4C,<C0482,>C0482	; SKPFLS	C0482
; 1887:                 elem_type = BPTR_TYPE
	DB	$2A,$20			; CB	32
	DB	$74,$0E			; SLB	14
; 1888:             else
	DB	$50,<C0483,>C0483	; SKIP	C0483
C0482:
; 1889:                 elem_type = WPTR_TYPE
	DB	$2A,$40			; CB	64
	DB	$74,$0E			; SLB	14
; 1890:             fin
C0483:
; 1891:             if parse_constval_21(@elem_offset, @elem_size)
	DB	$28,$10			; LLA	16
	DB	$28,$0F			; LLA	15
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$4C,<C0484,>C0484	; SKPFLS	C0484
; 1892:                 ;
; 1893:                 ; Constant structure offset
; 1894:                 ;
; 1895:                 if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0486,>C0486	; SKPFLS	C0486
; 1896:                     if type & VAR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0488,>C0488	; SKPFLS	C0488
; 1897:                         if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0490,>C0490	; SKPFLS	C0490
; 1898:                             emit_localaddr_10(value + elem_offset)
	DB	$66,$0C			; LLW	12
	DB	$66,$10			; LLW	16
	DB	$02			; ADD
	DB	$54,<C0152,>C0152	; CALL	C0152
; 1899:                         else
	DB	$50,<C0491,>C0491	; SKIP	C0491
C0490:
; 1900:                             ; emit_globaladdr_10(value + elem_offset)
; 1901:                             emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0154,>C0154	; CALL	C0154
; 1902:                             emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1903:                             drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0171,>C0171	; CALL	C0171
	DB	$30			; DROP
; 1904:                         fin
C0491:
; 1905:                     elsif type & CONST_TYPE
	DB	$50,<C0489,>C0489	; SKIP	C0489
C0488:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0492,>C0492	; SKPFLS	C0492
; 1906:                         value = value + elem_offset
	DB	$66,$0C			; LLW	12
	DB	$66,$10			; LLW	16
	DB	$02			; ADD
	DB	$76,$0C			; SLW	12
; 1907:                         emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1908:                     else ; FUNC_TYPE
	DB	$50,<C0489,>C0489	; SKIP	C0489
C0492:
; 1909:                         emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0154,>C0154	; CALL	C0154
; 1910:                         emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1911:                         drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0171,>C0171	; CALL	C0171
	DB	$30			; DROP
; 1912:                     fin
C0489:
; 1913:                     emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1914:                 else
	DB	$50,<C0487,>C0487	; SKIP	C0487
C0486:
; 1915:                     if elem_offset <> 0
	DB	$66,$10			; LLW	16
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0493,>C0493	; SKPFLS	C0493
; 1916:                         emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1917:                         drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0171,>C0171	; CALL	C0171
	DB	$30			; DROP
; 1918:                     fin
C0493:
C0494:
; 1919:                 fin ; !emit_val
C0487:
; 1920:                 drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 1921:             elsif token == OPEN_BRACKET_TKN
	DB	$50,<C0485,>C0485	; SKIP	C0485
C0484:
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C0495,>C0495	; SKPFLS	C0495
; 1922:                 ;
; 1923:                 ; Array of arrays
; 1924:                 ;
; 1925:                 if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0496,>C0496	; SKPFLS	C0496
; 1926:                     if type & ADDR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C0498,>C0498	; SKPFLS	C0498
; 1927:                         if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0500,>C0500	; SKPFLS	C0500
; 1928:                             emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 1929:                         else
	DB	$50,<C0501,>C0501	; SKIP	C0501
C0500:
; 1930:                             emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0154,>C0154	; CALL	C0154
; 1931:                         fin
C0501:
; 1932:                     elsif type & CONST_TYPE
	DB	$50,<C0499,>C0499	; SKIP	C0499
C0498:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0502,>C0502	; SKPFLS	C0502
; 1933:                         emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1934:                     fin
C0502:
C0499:
; 1935:                     emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1936:                 fin ; !emit_val
C0496:
C0497:
; 1937:                 repeat
C0504:
; 1938:                         if emit_val > 1
	DB	$64,$07			; LLB	7
	DB	$2A,$01			; CB	1
	DB	$44			; ISGT
	DB	$4C,<C0505,>C0505	; SKPFLS	C0505
; 1939:                                 emit_indexword()
	DB	$54,<C0158,>C0158	; CALL	C0158
; 1940:                                 emit_lw()
	DB	$54,<C0112,>C0112	; CALL	C0112
; 1941:                         fin
C0505:
C0506:
; 1942:                         emit_val = emit_val + 1
	DB	$64,$07			; LLB	7
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$07			; SLB	7
; 1943:                         if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0507,>C0507	; SKPFLS	C0507
; 1944:                                 return parse_err_11(@bad_expr)
	DB	$26,<D0499,>D0499	; LA	D0499
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1945:                         fin
C0507:
C0508:
; 1946:                         if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C0509,>C0509	; SKPFLS	C0509
; 1947:                             return parse_err_11(@no_close_bracket)
	DB	$26,<D0686,>D0686	; LA	D0686
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1948:                         fin
C0509:
C0510:
; 1949:                 until scan_01() <> OPEN_BRACKET_TKN
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$2A,$DB			; CB	219
	DB	$42			; ISNE
	DB	$4C,<C0504,>C0504	; SKPFLS	C0504
C0503:
; 1950:                 if elem_type & WPTR_TYPE
	DB	$64,$0E			; LLB	14
	DB	$2A,$40			; CB	64
	DB	$14			; BAND
	DB	$4C,<C0511,>C0511	; SKPFLS	C0511
; 1951:                     emit_indexword()
	DB	$54,<C0158,>C0158	; CALL	C0158
; 1952:                 else
	DB	$50,<C0512,>C0512	; SKIP	C0512
C0511:
; 1953:                     emit_indexbyte()
	DB	$54,<C0156,>C0156	; CALL	C0156
; 1954:                 fin
C0512:
; 1955:             else
	DB	$50,<C0485,>C0485	; SKIP	C0485
C0495:
; 1956:                 return parse_err_11(@bad_offset)
	DB	$26,<D0438,>D0438	; LA	D0438
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1957:             fin
C0485:
; 1958:             type = elem_type
	DB	$64,$0E			; LLB	14
	DB	$74,$06			; SLB	6
; 1959:         elsif token == OPEN_PAREN_TKN
	DB	$50,<C0465,>C0465	; SKIP	C0465
C0481:
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$A8			; CB	168
	DB	$40			; ISEQ
	DB	$4C,<C0513,>C0513	; SKPFLS	C0513
; 1960:             ;
; 1961:             ; Function call
; 1962:             ;
; 1963:             if !emit_val and type & VAR_TYPE
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0514,>C0514	; SKPFLS	C0514
; 1964:                 if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0516,>C0516	; SKPFLS	C0516
; 1965:                     emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 1966:                 else
	DB	$50,<C0517,>C0517	; SKIP	C0517
C0516:
; 1967:                     emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0154,>C0154	; CALL	C0154
; 1968:                 fin
C0517:
; 1969:             fin
C0514:
C0515:
; 1970:             if !(type & FUNC_CONST_TYPE)
	DB	$64,$06			; LLB	6
	DB	$2A,$09			; CB	9
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0518,>C0518	; SKPFLS	C0518
; 1971:                     emit_push()
	DB	$54,<C0146,>C0146	; CALL	C0146
; 1972:             fin
C0518:
C0519:
; 1973:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 1974:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C0520,>C0520	; SKPFLS	C0520
; 1975:                 return parse_err_11(@no_close_paren)
	DB	$26,<D0664,>D0664	; LA	D0664
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 1976:             fin
C0520:
C0521:
; 1977:             if type & FUNC_CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$09			; CB	9
	DB	$14			; BAND
	DB	$4C,<C0522,>C0522	; SKPFLS	C0522
; 1978:                 emit_call_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0142,>C0142	; CALL	C0142
; 1979:             else
	DB	$50,<C0523,>C0523	; SKIP	C0523
C0522:
; 1980:                 emit_pull()
	DB	$54,<C0148,>C0148	; CALL	C0148
; 1981:                 emit_ical()
	DB	$54,<C0144,>C0144	; CALL	C0144
; 1982:             fin
C0523:
; 1983:             emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1984:             type = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$06			; SLB	6
; 1985:             drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 1986:         fin
C0513:
C0465:
; 1987:     loop
	DB	$50,<C0462,>C0462	; SKIP	C0462
C0463:
; 1988:     if emit_val
	DB	$64,$07			; LLB	7
	DB	$4C,<C0524,>C0524	; SKPFLS	C0524
; 1989:         if rvalue
	DB	$66,$02			; LLW	2
	DB	$4C,<C0526,>C0526	; SKPFLS	C0526
; 1990:             if deref and type & PTR_TYPE
	DB	$64,$05			; LLB	5
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0528,>C0528	; SKPFLS	C0528
; 1991:                 if type & BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C0530,>C0530	; SKPFLS	C0530
; 1992:                     emit_lb()
	DB	$54,<C0110,>C0110	; CALL	C0110
; 1993:                 else
	DB	$50,<C0531,>C0531	; SKIP	C0531
C0530:
; 1994:                     emit_lw()
	DB	$54,<C0112,>C0112	; CALL	C0112
; 1995:                 fin
C0531:
; 1996:             fin
C0528:
C0529:
; 1997:         fin
C0526:
C0527:
; 1998:     else ; emit_val
	DB	$50,<C0525,>C0525	; SKIP	C0525
C0524:
; 1999:         if type & CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0532,>C0532	; SKPFLS	C0532
; 2000:             emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0105,>C0105	; CALL	C0105
; 2001:         elsif deref
	DB	$50,<C0533,>C0533	; SKIP	C0533
C0532:
	DB	$64,$05			; LLB	5
	DB	$4C,<C0534,>C0534	; SKPFLS	C0534
; 2002:             if type & FUNC_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0535,>C0535	; SKPFLS	C0535
; 2003:                 emit_call_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0142,>C0142	; CALL	C0142
; 2004:             elsif type & VAR_TYPE
	DB	$50,<C0536,>C0536	; SKIP	C0536
C0535:
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0537,>C0537	; SKPFLS	C0537
; 2005:                 if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0538,>C0538	; SKPFLS	C0538
; 2006:                     if type & BYTE_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0540,>C0540	; SKPFLS	C0540
; 2007:                         emit_llb_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0114,>C0114	; CALL	C0114
; 2008:                     else
	DB	$50,<C0541,>C0541	; SKIP	C0541
C0540:
; 2009:                         emit_llw_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0116,>C0116	; CALL	C0116
; 2010:                     fin
C0541:
; 2011:                 else
	DB	$50,<C0539,>C0539	; SKIP	C0539
C0538:
; 2012:                     if type & BYTE_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0542,>C0542	; SKPFLS	C0542
; 2013:                         emit_lab_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0118,>C0118	; CALL	C0118
; 2014:                     else
	DB	$50,<C0543,>C0543	; SKIP	C0543
C0542:
; 2015:                         emit_law_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0120,>C0120	; CALL	C0120
; 2016:                     fin
C0543:
; 2017:                 fin
C0539:
; 2018:             elsif type & PTR_TYPE
	DB	$50,<C0536,>C0536	; SKIP	C0536
C0537:
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$4C,<C0544,>C0544	; SKPFLS	C0544
; 2019:                 if type & BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C0545,>C0545	; SKPFLS	C0545
; 2020:                     emit_lb()
	DB	$54,<C0110,>C0110	; CALL	C0110
; 2021:                 else
	DB	$50,<C0546,>C0546	; SKIP	C0546
C0545:
; 2022:                     emit_lw()
	DB	$54,<C0112,>C0112	; CALL	C0112
; 2023:                 fin
C0546:
; 2024:             fin
C0544:
C0536:
; 2025:         else
	DB	$50,<C0533,>C0533	; SKIP	C0533
C0534:
; 2026:             if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0547,>C0547	; SKPFLS	C0547
; 2027:                 emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 2028:             else
	DB	$50,<C0548,>C0548	; SKIP	C0548
C0547:
; 2029:                 emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0154,>C0154	; CALL	C0154
; 2030:             fin
C0548:
; 2031:         fin
C0533:
; 2032:     fin ; emit_val
C0525:
; 2033:     while optos < opsp
C0549:
	DB	$66,$08			; LLW	8
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$46			; ISLT
	DB	$4C,<C0550,>C0550	; SKPFLS	C0550
; 2034:         if !emit_unaryop_11(pop_op_01())
	DB	$54,<C0313,>C0313	; CALL	C0313
	DB	$54,<C0160,>C0160	; CALL	C0160
	DB	$20			; NOT
	DB	$4C,<C0551,>C0551	; SKPFLS	C0551
; 2035:             return parse_err_11(@bad_op)
	DB	$26,<D0472,>D0472	; LA	D0472
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2036:         fin
C0551:
C0552:
; 2037:     loop
	DB	$50,<C0549,>C0549	; SKIP	C0549
C0550:
; 2038:     return type
	DB	$64,$06			; LLB	6
	DB	$5A			; LEAVE
; 2039: end
; 2040: def parse_constexpr_21(valptr, sizeptr)
C0553:					; parse_constexpr_21()
					; valptr = 2
					; sizeptr = 4
; 2041:     byte type, size1, size2
					; type = 6
					; size1 = 7
					; size2 = 8
; 2042:     word val1, val2
					; val1 = 9
					; val2 = 11
; 2043: 
; 2044:     type = parse_constval_21(@val1, @size1)
	JSR	_INTERP
	DB	$58,$0D,$02		; ENTER	13,2
	DB	$28,$09			; LLA	9
	DB	$28,$07			; LLA	7
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2045:     if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0555,>C0555	; SKPFLS	C0555
; 2046:         return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2047:     fin
C0555:
C0556:
; 2048:     size2 = 0
	DB	$00			; ZERO
	DB	$74,$08			; SLB	8
; 2049:     when scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
; 2050:         is ADD_TKN
	DB	$2A,$AB			; CB	171
	DB	$3E,<C0558,>C0558	; SKPNE	C0558
; 2051:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2052:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0559,>C0559	; SKPFLS	C0559
; 2053:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2054:             fin
C0559:
C0560:
; 2055:             *valptr = val1 + val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$02			; ADD
	DB	$72			; SW
; 2056:         is SUB_TKN
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0558:
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0561,>C0561	; SKPNE	C0561
; 2057:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2058:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0562,>C0562	; SKPFLS	C0562
; 2059:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2060:             fin
C0562:
C0563:
; 2061:             *valptr = val1 - val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$04			; SUB
	DB	$72			; SW
; 2062:         is MUL_TKN
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0561:
	DB	$2A,$AA			; CB	170
	DB	$3E,<C0564,>C0564	; SKPNE	C0564
; 2063:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2064:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0565,>C0565	; SKPFLS	C0565
; 2065:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2066:             fin
C0565:
C0566:
; 2067:             *valptr = val1 * val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$06			; MUL
	DB	$72			; SW
; 2068:         is DIV_TKN
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0564:
	DB	$2A,$AF			; CB	175
	DB	$3E,<C0567,>C0567	; SKPNE	C0567
; 2069:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2070:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0568,>C0568	; SKPFLS	C0568
; 2071:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2072:             fin
C0568:
C0569:
; 2073:             *valptr = val1 + val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$02			; ADD
	DB	$72			; SW
; 2074:         is MOD_TKN
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0567:
	DB	$2A,$A5			; CB	165
	DB	$3E,<C0570,>C0570	; SKPNE	C0570
; 2075:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2076:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0571,>C0571	; SKPFLS	C0571
; 2077:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2078:             fin
C0571:
C0572:
; 2079:             *valptr = val1 % val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$0A			; DIV,MOD
	DB	$72			; SW
; 2080:             drop
	DB	$30			; DROP
; 2081:         is AND_TKN
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0570:
	DB	$2A,$A6			; CB	166
	DB	$3E,<C0573,>C0573	; SKPNE	C0573
; 2082:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2083:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0574,>C0574	; SKPFLS	C0574
; 2084:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2085:             fin
C0574:
C0575:
; 2086:             *valptr = val1 & val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$14			; BAND
	DB	$72			; SW
; 2087:         is OR_TKN
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0573:
	DB	$2A,$BF			; CB	191
	DB	$3E,<C0576,>C0576	; SKPNE	C0576
; 2088:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2089:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0577,>C0577	; SKPFLS	C0577
; 2090:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2091:             fin
C0577:
C0578:
; 2092:             *valptr = val1 ? val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$16			; IOR
	DB	$72			; SW
; 2093:         is EOR_TKN
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0576:
	DB	$2A,$DE			; CB	222
	DB	$3E,<C0579,>C0579	; SKPNE	C0579
; 2094:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0390,>C0390	; CALL	C0390
	DB	$74,$06			; SLB	6
; 2095:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0580,>C0580	; SKPFLS	C0580
; 2096:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2097:             fin
C0580:
C0581:
; 2098:             *valptr = val1 ^ val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$18			; XOR
	DB	$72			; SW
; 2099:         otherwise
	DB	$50,<C0557,>C0557	; SKIP	C0557
C0579:
; 2100:             *valptr = val1
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$72			; SW
; 2101:     wend
C0557:
	DB	$30			; DROP
; 2102:     if size1 > size2
	DB	$64,$07			; LLB	7
	DB	$64,$08			; LLB	8
	DB	$44			; ISGT
	DB	$4C,<C0583,>C0583	; SKPFLS	C0583
; 2103:         ^sizeptr = size1
	DB	$66,$04			; LLW	4
	DB	$64,$07			; LLB	7
	DB	$70			; SB
; 2104:     else
	DB	$50,<C0584,>C0584	; SKIP	C0584
C0583:
; 2105:         ^sizeptr = size2
	DB	$66,$04			; LLW	4
	DB	$64,$08			; LLB	8
	DB	$70			; SB
; 2106:     fin
C0584:
; 2107:     return type
	DB	$64,$06			; LLB	6
	DB	$5A			; LEAVE
; 2108: end
; 2109: def parse_expr_01
C0000:					; parse_expr_01()
; 2110:     byte prevmatch, matchop, i
					; prevmatch = 2
					; matchop = 3
					; i = 4
; 2111:     word optos
					; optos = 5
; 2112: 
; 2113:     matchop = 0
	JSR	_INTERP
	DB	$58,$07,$00		; ENTER	7,0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 2114:     optos   = opsp
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$76,$05			; SLW	5
; 2115:     repeat
C0587:
; 2116:         prevmatch = matchop
	DB	$64,$03			; LLB	3
	DB	$74,$02			; SLB	2
; 2117:         matchop     = 0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 2118:         if parse_value_11(1)
	DB	$2A,$01			; CB	1
	DB	$54,<C0426,>C0426	; CALL	C0426
	DB	$4C,<C0588,>C0588	; SKPFLS	C0588
; 2119:             matchop = 1
	DB	$2A,$01			; CB	1
	DB	$74,$03			; SLB	3
; 2120:             for i = 0 to bops_tblsz
	DB	$00			; ZERO
C0591:
	DB	$6C,$04			; DLB	4
	DB	$2A,$12			; CB	18
	DB	$3A,<C0590,>C0590	; SKPGT	C0590
	DB	$0C			; INCR
; 2121:                 if token == bops_tbl[i]
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$26,<D0292,>D0292	; LA	D0292
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$40			; ISEQ
	DB	$4C,<C0592,>C0592	; SKPFLS	C0592
; 2122:                     matchop = 2
	DB	$2A,$02			; CB	2
	DB	$74,$03			; SLB	3
; 2123:                     if bops_prec[i] >= tos_op_prec_11(optos)
	DB	$26,<D0311,>D0311	; LA	D0311
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$66,$05			; LLW	5
	DB	$54,<C0321,>C0321	; CALL	C0321
	DB	$48			; ISGE
	DB	$4C,<C0594,>C0594	; SKPFLS	C0594
; 2124:                         if !emit_binaryop_11(pop_op_01())
	DB	$54,<C0313,>C0313	; CALL	C0313
	DB	$54,<C0171,>C0171	; CALL	C0171
	DB	$20			; NOT
	DB	$4C,<C0596,>C0596	; SKPFLS	C0596
; 2125:                             return parse_err_11(@bad_op)
	DB	$30			; DROP
	DB	$26,<D0472,>D0472	; LA	D0472
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2126:                         fin
C0596:
C0597:
; 2127:                     fin
C0594:
C0595:
; 2128:                     drop push_op_21(token, bops_prec[i])
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$26,<D0311,>D0311	; LA	D0311
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0309,>C0309	; CALL	C0309
	DB	$30			; DROP
; 2129:                     break
	DB	$50,<C0590,>C0590	; SKIP	C0590
; 2130:                 fin
C0592:
C0593:
; 2131:             next
	DB	$50,<C0591,>C0591	; SKIP	C0591
C0590:
	DB	$30			; DROP
; 2132:         fin
C0588:
C0589:
; 2133:     until matchop <> 2
	DB	$64,$03			; LLB	3
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C0587,>C0587	; SKPFLS	C0587
C0586:
; 2134:     if matchop == 0 and prevmatch == 2
	DB	$64,$03			; LLB	3
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0598,>C0598	; SKPFLS	C0598
; 2135:         return parse_err_11(@missing_op)
	DB	$26,<D0710,>D0710	; LA	D0710
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2136:     fin
C0598:
C0599:
; 2137:     while optos < opsp
C0600:
	DB	$66,$05			; LLW	5
	DB	$6A,<D0362,>D0362	; LAW	D0362
	DB	$46			; ISLT
	DB	$4C,<C0601,>C0601	; SKPFLS	C0601
; 2138:         if !emit_binaryop_11(pop_op_01())
	DB	$54,<C0313,>C0313	; CALL	C0313
	DB	$54,<C0171,>C0171	; CALL	C0171
	DB	$20			; NOT
	DB	$4C,<C0602,>C0602	; SKPFLS	C0602
; 2139:             return parse_err_11(@bad_op)
	DB	$26,<D0472,>D0472	; LA	D0472
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2140:         fin
C0602:
C0603:
; 2141:     loop
	DB	$50,<C0600,>C0600	; SKIP	C0600
C0601:
; 2142:     return matchop or prevmatch
	DB	$64,$03			; LLB	3
	DB	$64,$02			; LLB	2
	DB	$22			; LOR
	DB	$5A			; LEAVE
; 2143: end
; 2144: def parse_setlist_21(addr, type)
C0604:					; parse_setlist_21()
					; addr = 2
					; type = 4
; 2145:     word nexttype, nextaddr, idptr, saveptr
					; nexttype = 6
					; nextaddr = 8
					; idptr = 10
					; saveptr = 12
; 2146: 
; 2147:     if !(type & VAR_TYPE)
	JSR	_INTERP
	DB	$58,$0E,$02		; ENTER	14,2
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0606,>C0606	; SKPFLS	C0606
; 2148:         emit_push()
	DB	$54,<C0146,>C0146	; CALL	C0146
; 2149:     fin
C0606:
C0607:
; 2150:     nexttype = 0
	DB	$00			; ZERO
	DB	$76,$06			; SLW	6
; 2151:     nextaddr = 0
	DB	$00			; ZERO
	DB	$76,$08			; SLW	8
; 2152:     if scan_01() == ID_TKN
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C0608,>C0608	; SKPFLS	C0608
; 2153:         idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0368,>D0368	; LAW	D0368
	DB	$68,<D0365,>D0365	; LAB	D0365
	DB	$54,<C0337,>C0337	; CALL	C0337
	DB	$76,$0A			; SLW	10
; 2154:         if !idptr
	DB	$66,$0A			; LLW	10
	DB	$20			; NOT
	DB	$4C,<C0610,>C0610	; SKPFLS	C0610
; 2155:             return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2156:         fin
C0610:
C0611:
; 2157:         nexttype = (idptr).idtype
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$76,$06			; SLW	6
; 2158:         if type & VAR_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0612,>C0612	; SKPFLS	C0612
; 2159:             nextaddr = (idptr):idval
	DB	$66,$0A			; LLW	10
	DB	$62			; LW
	DB	$76,$08			; SLW	8
; 2160:         fin
C0612:
C0613:
; 2161:     fin
C0608:
C0609:
; 2162:     saveptr = tknptr
	DB	$6A,<D0368,>D0368	; LAW	D0368
	DB	$76,$0C			; SLW	12
; 2163:     drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 2164:     if nexttype & VAR_TYPE and token == SET_TKN
	DB	$66,$06			; LLW	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0614,>C0614	; SKPFLS	C0614
; 2165:         drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2166:         if type & LOCAL_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0616,>C0616	; SKPFLS	C0616
; 2167:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0618,>C0618	; SKPFLS	C0618
; 2168:                 emit_slb_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0126,>C0126	; CALL	C0126
; 2169:             else
	DB	$50,<C0619,>C0619	; SKIP	C0619
C0618:
; 2170:                 emit_slw_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0128,>C0128	; CALL	C0128
; 2171:             fin
C0619:
; 2172:         else
	DB	$50,<C0617,>C0617	; SKIP	C0617
C0616:
; 2173:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0620,>C0620	; SKPFLS	C0620
; 2174:                 emit_sab_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0134,>C0134	; CALL	C0134
; 2175:             else
	DB	$50,<C0621,>C0621	; SKIP	C0621
C0620:
; 2176:                 emit_saw_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0136,>C0136	; CALL	C0136
; 2177:             fin
C0621:
; 2178:         fin
C0617:
; 2179:     elsif nexttype & VAR_TYPE and token == SETLIST_TKN
	DB	$50,<C0615,>C0615	; SKIP	C0615
C0614:
	DB	$66,$06			; LLW	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0622,>C0622	; SKPFLS	C0622
; 2180:         if !parse_setlist_21(nextaddr, nexttype)
	DB	$66,$08			; LLW	8
	DB	$66,$06			; LLW	6
	DB	$54,<C0604,>C0604	; CALL	C0604
	DB	$20			; NOT
	DB	$4C,<C0623,>C0623	; SKPFLS	C0623
; 2181:             return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2182:         fin
C0623:
C0624:
; 2183:     else
	DB	$50,<C0615,>C0615	; SKIP	C0615
C0622:
; 2184:         tknptr = saveptr
	DB	$66,$0C			; LLW	12
	DB	$7A,<D0368,>D0368	; SAW	D0368
; 2185:         rewind_10(tknptr)
	DB	$6A,<D0368,>D0368	; LAW	D0368
	DB	$54,<C0301,>C0301	; CALL	C0301
; 2186:         nexttype = parse_value_11(0)
	DB	$00			; ZERO
	DB	$54,<C0426,>C0426	; CALL	C0426
	DB	$76,$06			; SLW	6
; 2187:         if nexttype <> 0
	DB	$66,$06			; LLW	6
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0625,>C0625	; SKPFLS	C0625
; 2188:             if token == SET_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C0627,>C0627	; SKPFLS	C0627
; 2189:                 emit_push()
	DB	$54,<C0146,>C0146	; CALL	C0146
; 2190:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2191:                 emit_pull()
	DB	$54,<C0148,>C0148	; CALL	C0148
; 2192:                 emit_swap()
	DB	$54,<C0216,>C0216	; CALL	C0216
; 2193:                 if nexttype & (BYTE_TYPE ? BPTR_TYPE)
	DB	$66,$06			; LLW	6
	DB	$2A,$02			; CB	2
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$14			; BAND
	DB	$4C,<C0629,>C0629	; SKPFLS	C0629
; 2194:                     emit_sb()
	DB	$54,<C0122,>C0122	; CALL	C0122
; 2195:                 else
	DB	$50,<C0630,>C0630	; SKIP	C0630
C0629:
; 2196:                     emit_sw()
	DB	$54,<C0124,>C0124	; CALL	C0124
; 2197:                 fin
C0630:
; 2198:             fin
C0627:
C0628:
; 2199:         elsif token == SETLIST_TKN
	DB	$50,<C0626,>C0626	; SKIP	C0626
C0625:
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$4C,<C0631,>C0631	; SKPFLS	C0631
; 2200:             if !parse_setlist_21(0, nexttype)
	DB	$00			; ZERO
	DB	$66,$06			; LLW	6
	DB	$54,<C0604,>C0604	; CALL	C0604
	DB	$20			; NOT
	DB	$4C,<C0632,>C0632	; SKPFLS	C0632
; 2201:                 return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2202:             fin
C0632:
C0633:
; 2203:         else
	DB	$50,<C0626,>C0626	; SKIP	C0626
C0631:
; 2204:             return parse_err_11(@bad_syntax)
	DB	$26,<D0514,>D0514	; LA	D0514
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2205:         fin
C0626:
; 2206:     fin
C0615:
; 2207:     if type & VAR_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0634,>C0634	; SKPFLS	C0634
; 2208:         if type & LOCAL_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0636,>C0636	; SKPFLS	C0636
; 2209:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0638,>C0638	; SKPFLS	C0638
; 2210:                 emit_slb_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0126,>C0126	; CALL	C0126
; 2211:             else
	DB	$50,<C0639,>C0639	; SKIP	C0639
C0638:
; 2212:                 emit_slw_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0128,>C0128	; CALL	C0128
; 2213:             fin
C0639:
; 2214:         else
	DB	$50,<C0637,>C0637	; SKIP	C0637
C0636:
; 2215:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0640,>C0640	; SKPFLS	C0640
; 2216:                 emit_sab_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0134,>C0134	; CALL	C0134
; 2217:             else
	DB	$50,<C0641,>C0641	; SKIP	C0641
C0640:
; 2218:                 emit_saw_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0136,>C0136	; CALL	C0136
; 2219:             fin
C0641:
; 2220:         fin
C0637:
; 2221:     else
	DB	$50,<C0635,>C0635	; SKIP	C0635
C0634:
; 2222:         emit_pull()
	DB	$54,<C0148,>C0148	; CALL	C0148
; 2223:         emit_swap()
	DB	$54,<C0216,>C0216	; CALL	C0216
; 2224:         if type & (BYTE_TYPE ? BPTR_TYPE)
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$14			; BAND
	DB	$4C,<C0642,>C0642	; SKPFLS	C0642
; 2225:             emit_sb()
	DB	$54,<C0122,>C0122	; CALL	C0122
; 2226:         else
	DB	$50,<C0643,>C0643	; SKIP	C0643
C0642:
; 2227:             emit_sw()
	DB	$54,<C0124,>C0124	; CALL	C0124
; 2228:         fin
C0643:
; 2229:     fin
C0635:
; 2230:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2231: end
; 2232: def parse_stmnt_01
C0644:					; parse_stmnt_01()
; 2233:     byte type, i
					; type = 2
					; i = 3
; 2234:     word tag_prevbrk, tag_else, tag_endif, tag_while, tag_wend
					; tag_prevbrk = 4
					; tag_else = 6
					; tag_endif = 8
					; tag_while = 10
					; tag_wend = 12
; 2235:     word tag_repeat, tag_for, tag_choice, idptr, saveptr, addr, stepdir
					; tag_repeat = 14
					; tag_for = 16
					; tag_choice = 18
					; idptr = 20
					; saveptr = 22
					; addr = 24
					; stepdir = 26
; 2236: 
; 2237:     if token <> END_TKN and token <> DONE_TKN
	JSR	_INTERP
	DB	$58,$1C,$00		; ENTER	28,0
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$87			; CB	135
	DB	$42			; ISNE
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$98			; CB	152
	DB	$42			; ISNE
	DB	$24			; LAND
	DB	$4C,<C0646,>C0646	; SKPFLS	C0646
; 2238:         prevstmnt = token
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$78,<D0904,>D0904	; SAB	D0904
; 2239:     fin
C0646:
C0647:
; 2240:     when token
	DB	$68,<D0364,>D0364	; LAB	D0364
; 2241:         is IF_TKN
	DB	$2A,$83			; CB	131
	DB	$3E,<C0649,>C0649	; SKPNE	C0649
; 2242:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2243:             tag_else  = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$06			; SLW	6
; 2244:             tag_endif = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$08			; SLW	8
; 2245:             emit_brfls_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0204,>C0204	; CALL	C0204
; 2246:             drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 2247:             repeat
C0651:
; 2248:                 while parse_stmnt_01()
C0652:
	DB	$54,<C0644,>C0644	; CALL	C0644
	DB	$4C,<C0653,>C0653	; SKPFLS	C0653
; 2249:                     drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2250:                 loop
	DB	$50,<C0652,>C0652	; SKIP	C0652
C0653:
; 2251:                 if token <> ELSEIF_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$84			; CB	132
	DB	$42			; ISNE
	DB	$4C,<C0654,>C0654	; SKPFLS	C0654
; 2252:                     break
	DB	$50,<C0650,>C0650	; SKIP	C0650
; 2253:                 fin
C0654:
C0655:
; 2254:                 emit_jump_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2255:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2256:                 if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0656,>C0656	; SKPFLS	C0656
; 2257:                     return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2258:                 fin
C0656:
C0657:
; 2259:                 tag_else = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$06			; SLW	6
; 2260:                 emit_brfls_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0204,>C0204	; CALL	C0204
; 2261:             until FALSE
	DB	$00			; ZERO
	DB	$4C,<C0651,>C0651	; SKPFLS	C0651
C0650:
; 2262:             if token == ELSE_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$85			; CB	133
	DB	$40			; ISEQ
	DB	$4C,<C0658,>C0658	; SKPFLS	C0658
; 2263:                 emit_jump_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2264:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2265:                 drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 2266:                 while parse_stmnt_01()
C0660:
	DB	$54,<C0644,>C0644	; CALL	C0644
	DB	$4C,<C0661,>C0661	; SKPFLS	C0661
; 2267:                     drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2268:                 loop
	DB	$50,<C0660,>C0660	; SKIP	C0660
C0661:
; 2269:                 emit_codetag_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2270:             else
	DB	$50,<C0659,>C0659	; SKIP	C0659
C0658:
; 2271:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2272:                 emit_codetag_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2273:             fin
C0659:
; 2274:             if token <> FIN_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$86			; CB	134
	DB	$42			; ISNE
	DB	$4C,<C0662,>C0662	; SKPFLS	C0662
; 2275:                 return parse_err_11(@no_fin)
	DB	$30			; DROP
	DB	$26,<D0726,>D0726	; LA	D0726
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2276:             fin
C0662:
C0663:
; 2277:         is FOR_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0649:
	DB	$2A,$8E			; CB	142
	DB	$3E,<C0664,>C0664	; SKPNE	C0664
; 2278:             stack_loop  = stack_loop + 1
	DB	$68,<D0903,>D0903	; LAB	D0903
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0903,>D0903	; SAB	D0903
; 2279:             tag_for     = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$10			; SLW	16
; 2280:             tag_prevbrk = break_tag
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$76,$04			; SLW	4
; 2281:             break_tag   = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2282:             if scan_01() <> ID_TKN
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$2A,$D6			; CB	214
	DB	$42			; ISNE
	DB	$4C,<C0665,>C0665	; SKPFLS	C0665
; 2283:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2284:             fin
C0665:
C0666:
; 2285:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0368,>D0368	; LAW	D0368
	DB	$68,<D0365,>D0365	; LAB	D0365
	DB	$54,<C0337,>C0337	; CALL	C0337
	DB	$76,$14			; SLW	20
; 2286:             if idptr
	DB	$66,$14			; LLW	20
	DB	$4C,<C0667,>C0667	; SKPFLS	C0667
; 2287:                 type  = (idptr).idtype
	DB	$66,$14			; LLW	20
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$02			; SLB	2
; 2288:                 addr  = (idptr):idval
	DB	$66,$14			; LLW	20
	DB	$62			; LW
	DB	$76,$18			; SLW	24
; 2289:             else
	DB	$50,<C0668,>C0668	; SKIP	C0668
C0667:
; 2290:                 return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2291:             fin
C0668:
; 2292:             if scan_01() <> SET_TKN
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$2A,$BD			; CB	189
	DB	$42			; ISNE
	DB	$4C,<C0669,>C0669	; SKPFLS	C0669
; 2293:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2294:             fin
C0669:
C0670:
; 2295:             if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0671,>C0671	; SKPFLS	C0671
; 2296:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2297:             fin
C0671:
C0672:
; 2298:             emit_codetag_10(tag_for)
	DB	$66,$10			; LLW	16
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2299:             if type & LOCAL_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0673,>C0673	; SKPFLS	C0673
; 2300:                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0675,>C0675	; SKPFLS	C0675
; 2301:                     emit_dlb_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0130,>C0130	; CALL	C0130
; 2302:                 else
	DB	$50,<C0676,>C0676	; SKIP	C0676
C0675:
; 2303:                     emit_dlw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0132,>C0132	; CALL	C0132
; 2304:                 fin
C0676:
; 2305:             else
	DB	$50,<C0674,>C0674	; SKIP	C0674
C0673:
; 2306:                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0677,>C0677	; SKPFLS	C0677
; 2307:                     emit_dab_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0138,>C0138	; CALL	C0138
; 2308:                 else
	DB	$50,<C0678,>C0678	; SKIP	C0678
C0677:
; 2309:                     emit_daw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0140,>C0140	; CALL	C0140
; 2310:                 fin
C0678:
; 2311:             fin
C0674:
; 2312:             stepdir = 1
	DB	$2A,$01			; CB	1
	DB	$76,$1A			; SLW	26
; 2313:             if token == TO_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$8F			; CB	143
	DB	$40			; ISEQ
	DB	$4C,<C0679,>C0679	; SKPFLS	C0679
; 2314:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2315:             elsif token == DOWNTO_TKN
	DB	$50,<C0680,>C0680	; SKIP	C0680
C0679:
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$90			; CB	144
	DB	$40			; ISEQ
	DB	$4C,<C0681,>C0681	; SKPFLS	C0681
; 2316:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2317:                 stepdir = -1
	DB	$2C,$FF,$FF		; CW	-1
	DB	$76,$1A			; SLW	26
; 2318:             fin
C0681:
C0680:
; 2319:             if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0682,>C0682	; SKPFLS	C0682
; 2320:                 emit_brgt_10(break_tag)
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$54,<C0206,>C0206	; CALL	C0206
; 2321:             else
	DB	$50,<C0683,>C0683	; SKIP	C0683
C0682:
; 2322:                 emit_brlt_10(break_tag)
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$54,<C0208,>C0208	; CALL	C0208
; 2323:             fin
C0683:
; 2324:             if token == STEP_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$91			; CB	145
	DB	$40			; ISEQ
	DB	$4C,<C0684,>C0684	; SKPFLS	C0684
; 2325:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2326:                 if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0686,>C0686	; SKPFLS	C0686
; 2327:                     drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0171,>C0171	; CALL	C0171
	DB	$30			; DROP
; 2328:                 else
	DB	$50,<C0687,>C0687	; SKIP	C0687
C0686:
; 2329:                     drop emit_binaryop_11(SUB_TKN)
	DB	$2A,$AD			; CB	173
	DB	$54,<C0171,>C0171	; CALL	C0171
	DB	$30			; DROP
; 2330:                 fin
C0687:
; 2331:             else
	DB	$50,<C0685,>C0685	; SKIP	C0685
C0684:
; 2332:                 if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0688,>C0688	; SKPFLS	C0688
; 2333:                     drop emit_unaryop_11(INC_TKN)
	DB	$2A,$C1			; CB	193
	DB	$54,<C0160,>C0160	; CALL	C0160
	DB	$30			; DROP
; 2334:                 else
	DB	$50,<C0689,>C0689	; SKIP	C0689
C0688:
; 2335:                     drop emit_unaryop_11(DEC_TKN)
	DB	$2A,$C4			; CB	196
	DB	$54,<C0160,>C0160	; CALL	C0160
	DB	$30			; DROP
; 2336:                 fin
C0689:
; 2337:             fin
C0685:
; 2338:             while parse_stmnt_01()
C0690:
	DB	$54,<C0644,>C0644	; CALL	C0644
	DB	$4C,<C0691,>C0691	; SKPFLS	C0691
; 2339:                 drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2340:             loop
	DB	$50,<C0690,>C0690	; SKIP	C0690
C0691:
; 2341:             if token <> NEXT_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$92			; CB	146
	DB	$42			; ISNE
	DB	$4C,<C0692,>C0692	; SKPFLS	C0692
; 2342:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2343:             fin
C0692:
C0693:
; 2344:             emit_jump_10(tag_for)
	DB	$66,$10			; LLW	16
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2345:             emit_codetag_10(break_tag)
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2346:             emit_drop()
	DB	$54,<C0214,>C0214	; CALL	C0214
; 2347:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2348:             stack_loop = stack_loop - 1
	DB	$68,<D0903,>D0903	; LAB	D0903
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0903,>D0903	; SAB	D0903
; 2349:         is WHILE_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0664:
	DB	$2A,$88			; CB	136
	DB	$3E,<C0694,>C0694	; SKPNE	C0694
; 2350:             tag_while   = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$0A			; SLW	10
; 2351:             tag_wend    = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$0C			; SLW	12
; 2352:             tag_prevbrk = break_tag
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$76,$04			; SLW	4
; 2353:             break_tag   = tag_wend
	DB	$66,$0C			; LLW	12
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2354:             emit_codetag_10(tag_while)
	DB	$66,$0A			; LLW	10
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2355:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2356:             emit_brfls_10(tag_wend)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0204,>C0204	; CALL	C0204
; 2357:             while parse_stmnt_01()
C0695:
	DB	$54,<C0644,>C0644	; CALL	C0644
	DB	$4C,<C0696,>C0696	; SKPFLS	C0696
; 2358:                 drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2359:             loop
	DB	$50,<C0695,>C0695	; SKIP	C0695
C0696:
; 2360:             if token <> LOOP_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$89			; CB	137
	DB	$42			; ISNE
	DB	$4C,<C0697,>C0697	; SKPFLS	C0697
; 2361:                 return parse_err_11(@no_loop)
	DB	$30			; DROP
	DB	$26,<D0738,>D0738	; LA	D0738
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2362:             fin
C0697:
C0698:
; 2363:             emit_jump_10(tag_while)
	DB	$66,$0A			; LLW	10
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2364:             emit_codetag_10(tag_wend)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2365:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2366:         is REPEAT_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0694:
	DB	$2A,$93			; CB	147
	DB	$3E,<C0699,>C0699	; SKPNE	C0699
; 2367:             tag_repeat  = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$0E			; SLW	14
; 2368:             tag_prevbrk = break_tag
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$76,$04			; SLW	4
; 2369:             break_tag   = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2370:             emit_codetag_10(tag_repeat)
	DB	$66,$0E			; LLW	14
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2371:             drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 2372:             while parse_stmnt_01()
C0700:
	DB	$54,<C0644,>C0644	; CALL	C0644
	DB	$4C,<C0701,>C0701	; SKPFLS	C0701
; 2373:                 drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2374:             loop
	DB	$50,<C0700,>C0700	; SKIP	C0700
C0701:
; 2375:             if token <> UNTIL_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$94			; CB	148
	DB	$42			; ISNE
	DB	$4C,<C0702,>C0702	; SKPFLS	C0702
; 2376:                 return parse_err_11(@no_until)
	DB	$30			; DROP
	DB	$26,<D0751,>D0751	; LA	D0751
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2377:             fin
C0702:
C0703:
; 2378:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2379:             emit_brfls_10(tag_repeat)
	DB	$66,$0E			; LLW	14
	DB	$54,<C0204,>C0204	; CALL	C0204
; 2380:             emit_codetag_10(break_tag)
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2381:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2382:         is CASE_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0699:
	DB	$2A,$8A			; CB	138
	DB	$3E,<C0704,>C0704	; SKPNE	C0704
; 2383:             stack_loop  = stack_loop + 1
	DB	$68,<D0903,>D0903	; LAB	D0903
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0903,>D0903	; SAB	D0903
; 2384:             tag_choice  = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$12			; SLW	18
; 2385:             tag_prevbrk = break_tag
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$76,$04			; SLW	4
; 2386:             break_tag   = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2387:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2388:             drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2389:             while token <> ENDCASE_TKN
C0705:
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$8D			; CB	141
	DB	$42			; ISNE
	DB	$4C,<C0706,>C0706	; SKPFLS	C0706
; 2390:                 when token
	DB	$68,<D0364,>D0364	; LAB	D0364
; 2391:                     is OF_TKN
	DB	$2A,$8B			; CB	139
	DB	$3E,<C0708,>C0708	; SKPNE	C0708
; 2392:                         if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0709,>C0709	; SKPFLS	C0709
; 2393:                             return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2394:                         fin
C0709:
C0710:
; 2395:                         emit_brne_10(tag_choice)
	DB	$66,$12			; LLW	18
	DB	$54,<C0210,>C0210	; CALL	C0210
; 2396:                         while parse_stmnt_01()
C0711:
	DB	$54,<C0644,>C0644	; CALL	C0644
	DB	$4C,<C0712,>C0712	; SKPFLS	C0712
; 2397:                             drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2398:                         loop
	DB	$50,<C0711,>C0711	; SKIP	C0711
C0712:
; 2399:                         emit_jump_10(break_tag)
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2400:                         emit_codetag_10(tag_choice)
	DB	$66,$12			; LLW	18
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2401:                         tag_choice = ctag_new_01()
	DB	$54,<C0068,>C0068	; CALL	C0068
	DB	$76,$12			; SLW	18
; 2402:                     is DEFAULT_TKN
	DB	$50,<C0707,>C0707	; SKIP	C0707
C0708:
	DB	$2A,$8C			; CB	140
	DB	$3E,<C0713,>C0713	; SKPNE	C0713
; 2403:                         drop scan_01()
	DB	$54,<C0248,>C0248	; CALL	C0248
	DB	$30			; DROP
; 2404:                         while parse_stmnt_01()
C0714:
	DB	$54,<C0644,>C0644	; CALL	C0644
	DB	$4C,<C0715,>C0715	; SKPFLS	C0715
; 2405:                             drop nextln_01()
	DB	$54,<C0303,>C0303	; CALL	C0303
	DB	$30			; DROP
; 2406:                         loop
	DB	$50,<C0714,>C0714	; SKIP	C0714
C0715:
; 2407:                         if token <> ENDCASE_TKN
	DB	$68,<D0364,>D0364	; LAB	D0364
	DB	$2A,$8D			; CB	141
	DB	$42			; ISNE
	DB	$4C,<C0716,>C0716	; SKPFLS	C0716
; 2408:                             return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2409:                         fin
C0716:
C0717:
; 2410:                     otherwise
	DB	$50,<C0707,>C0707	; SKIP	C0707
C0713:
; 2411:                         return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2412:                 wend
C0707:
	DB	$30			; DROP
; 2413:             loop
	DB	$50,<C0705,>C0705	; SKIP	C0705
C0706:
; 2414:             emit_codetag_10(break_tag)
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$54,<C0084,>C0084	; CALL	C0084
; 2415:             emit_drop()
	DB	$54,<C0214,>C0214	; CALL	C0214
; 2416:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0907,>D0907	; SAW	D0907
; 2417:             stack_loop = stack_loop - 1
	DB	$68,<D0903,>D0903	; LAB	D0903
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0903,>D0903	; SAB	D0903
; 2418:         is BREAK_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0704:
	DB	$2A,$9A			; CB	154
	DB	$3E,<C0719,>C0719	; SKPNE	C0719
; 2419:             if break_tag
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$4C,<C0720,>C0720	; SKPFLS	C0720
; 2420:                 emit_jump_10(break_tag)
	DB	$6A,<D0907,>D0907	; LAW	D0907
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2421:             else
	DB	$50,<C0721,>C0721	; SKIP	C0721
C0720:
; 2422:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2423:             fin
C0721:
; 2424:         is RETURN_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0719:
	DB	$2A,$99			; CB	153
	DB	$3E,<C0722,>C0722	; SKPNE	C0722
; 2425:             if infunc
	DB	$68,<D0902,>D0902	; LAB	D0902
	DB	$4C,<C0723,>C0723	; SKPFLS	C0723
; 2426:                 for i = 1 to stack_loop
	DB	$2A,$01			; CB	1
C0726:
	DB	$6C,$03			; DLB	3
	DB	$68,<D0903,>D0903	; LAB	D0903
	DB	$3A,<C0725,>C0725	; SKPGT	C0725
	DB	$0C			; INCR
; 2427:                     emit_drop()
	DB	$54,<C0214,>C0214	; CALL	C0214
; 2428:                 next
	DB	$50,<C0726,>C0726	; SKIP	C0726
C0725:
	DB	$30			; DROP
; 2429:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2430:                 emit_leave_10(framesize)
	DB	$6A,<D0016,>D0016	; LAW	D0016
	DB	$54,<C0218,>C0218	; CALL	C0218
; 2431:             else
	DB	$50,<C0724,>C0724	; SKIP	C0724
C0723:
; 2432:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0486,>D0486	; LA	D0486
	DB	$54,<C0058,>C0058	; CALL	C0058
	DB	$5A			; LEAVE
; 2433:             fin
C0724:
; 2434:         is EXIT_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0722:
	DB	$2A,$9C			; CB	156
	DB	$3E,<C0727,>C0727	; SKPNE	C0727
; 2435:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2436:             emit_exit()
	DB	$54,<C0228,>C0228	; CALL	C0228
; 2437:         is DROP_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0727:
	DB	$2A,$97			; CB	151
	DB	$3E,<C0728,>C0728	; SKPNE	C0728
; 2438:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2439:             emit_drop()
	DB	$54,<C0214,>C0214	; CALL	C0214
; 2440:         is ELSE_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0728:
	DB	$2A,$85			; CB	133
	DB	$3E,<C0729,>C0729	; SKPNE	C0729
; 2441:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2442:         is ELSEIF_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0729:
	DB	$2A,$84			; CB	132
	DB	$3E,<C0730,>C0730	; SKPNE	C0730
; 2443:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2444:         is FIN_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0730:
	DB	$2A,$86			; CB	134
	DB	$3E,<C0731,>C0731	; SKPNE	C0731
; 2445:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2446:         is LOOP_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0731:
	DB	$2A,$89			; CB	137
	DB	$3E,<C0732,>C0732	; SKPNE	C0732
; 2447:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2448:         is UNTIL_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0732:
	DB	$2A,$94			; CB	148
	DB	$3E,<C0733,>C0733	; SKPNE	C0733
; 2449:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2450:         is NEXT_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0733:
	DB	$2A,$92			; CB	146
	DB	$3E,<C0734,>C0734	; SKPNE	C0734
; 2451:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2452:         is OF_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0734:
	DB	$2A,$8B			; CB	139
	DB	$3E,<C0735,>C0735	; SKPNE	C0735
; 2453:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2454:         is DEFAULT_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0735:
	DB	$2A,$8C			; CB	140
	DB	$3E,<C0736,>C0736	; SKPNE	C0736
; 2455:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2456:         is ENDCASE_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0736:
	DB	$2A,$8D			; CB	141
	DB	$3E,<C0737,>C0737	; SKPNE	C0737
; 2457:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2458:         is END_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0737:
	DB	$2A,$87			; CB	135
	DB	$3E,<C0738,>C0738	; SKPNE	C0738
; 2459:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2460:         is DONE_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0738:
	DB	$2A,$98			; CB	152
	DB	$3E,<C0739,>C0739	; SKPNE	C0739
; 2461:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2462:         is IFUNC_TKN
	DB	$50,<C0648,>C0648	; SKIP	C0648
C0739:
Bad ID type
START:	; JSR	INTERP
Bad ID type
	DB	$5C			; RET
