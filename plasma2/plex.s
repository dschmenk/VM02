	.INCLUDE	"plstub.s"
;    1: ;
;    2: ; Global constants
;    3: ;
;    4: const FALSE             = 0
					; FALSE = 0
;    5: const TRUE              = !FALSE
					; TRUE = -1
;    6: ;
;    7: ; Data and text buffer constants
;    8: ;
;    9: const iobuffer          = $0800
					; iobuffer = 2048
;   10: const codebuff          = $6000
					; codebuff = 24576
;   11: const codebuffsz        = $5000
					; codebuffsz = 20480
;   12: const argbuff           = $2006
					; argbuff = 8198
;   13: const inbuff            = $0200
					; inbuff = 512
;   14: const instr             = $01FF
					; instr = 511
;   15: byte  inref
D0000:	DS	1			; inref
;   16: ;byte  emptystk
;   17: ;
;   18: ; Symbol table variables
;   19: ;
;   20: const idglobal_tblsz    = $0800
					; idglobal_tblsz = 2048
;   21: const idlocal_tblsz     = $0200
					; idlocal_tblsz = 512
;   22: const idglobal_tbl      = $1000
					; idglobal_tbl = 4096
;   23: const idlocal_tbl       = $1800
					; idlocal_tbl = 6144
;   24: const ctag_max          = 768
					; ctag_max = 768
;   25: const ctag_value        = $1A00
					; ctag_value = 6656
;   26: const ctag_flags        = $0D00
					; ctag_flags = 3328
;   27: const idval             = 0
					; idval = 0
;   28: const idtype            = 2
					; idtype = 2
;   29: const idname            = 3
					; idname = 3
;   30: const idrecsz           = 4
					; idrecsz = 4
;   31: word globals            = 0
D0001:					; globals
	DW	$0000
;   32: word datasize           = 0
D0003:					; datasize
	DW	$0000
;   33: word lastglobal
D0005:	DS	2			; lastglobal
;   34: byte locals             = 0
D0007:					; locals
	DB	$00
;   35: word framesize          = 0
D0008:					; framesize
	DW	$0000
;   36: word lastlocal
D0010:	DS	2			; lastlocal
;   37: const resolved          = 1
					; resolved = 1
;   38: const is_ctag           = $8000
					; is_ctag = 32768
;   39: const mask_ctag         = $7FFF
					; mask_ctag = 32767
;   40: word codetag            = -1
D0012:					; codetag
	DW	$FFFF
;   41: word codeptr
D0014:	DS	2			; codeptr
;   42: word entrypoint         = 0
D0016:					; entrypoint
	DW	$0000
;   43: byte lastop             = $FF
D0018:					; lastop
	DB	$FF
;   44: byte perr
D0019:	DS	1			; perr
;   45: ;
;   46: ; String variables
;   47: ;
;   48: byte version[]          = "PLASMA ][ EXECUTIVE VERSION 0.8 "
D0020:					; version
	DB	$20
	DB	$50,$4C,$41,$53,$4D,$41,$20,$5D
	DB	$5B,$20,$45,$58,$45,$43,$55,$54
	DB	$49,$56,$45,$20,$56,$45,$52,$53
	DB	$49,$4F,$4E,$20,$30,$2E,$38,$20
;   49: byte donemsg[]          = "EXECUTION COMPLETE.  PRESS A KEY..."
D0053:					; donemsg
	DB	$23
	DB	$45,$58,$45,$43,$55,$54,$49,$4F
	DB	$4E,$20,$43,$4F,$4D,$50,$4C,$45
	DB	$54,$45,$2E,$20,$20,$50,$52,$45
	DB	$53,$53,$20,$41,$20,$4B,$45,$59
	DB	$2E,$2E,$2E
;   50: byte badfile[]          = "FILE NOT FOUND"
D0089:					; badfile
	DB	$0E
	DB	$46,$49,$4C,$45,$20,$4E,$4F,$54
	DB	$20,$46,$4F,$55,$4E,$44
;   51: byte brkmsg[] 			= "CTRL-C BREAK"
D0104:					; brkmsg
	DB	$0C
	DB	$43,$54,$52,$4C,$2D,$43,$20,$42
	DB	$52,$45,$41,$4B
;   52: byte stkovflwmsg[] 		= "STACK OVERFLOW/UNDERFLOW ERROR"
D0117:					; stkovflwmsg
	DB	$1E
	DB	$53,$54,$41,$43,$4B,$20,$4F,$56
	DB	$45,$52,$46,$4C,$4F,$57,$2F,$55
	DB	$4E,$44,$45,$52,$46,$4C,$4F,$57
	DB	$20,$45,$52,$52,$4F,$52
;   53: ;
;   54: ; Tokens
;   55: ;
;   56: const ID_TKN            = $D6 ; V
					; ID_TKN = 214
;   57: const CHR_TKN           = $C3 ; C
					; CHR_TKN = 195
;   58: const INT_TKN           = $C9 ; I
					; INT_TKN = 201
;   59: const STR_TKN           = $D3 ; S
					; STR_TKN = 211
;   60: const EOL_TKN           = $02
					; EOL_TKN = 2
;   61: const EOF_TKN           = $01
					; EOF_TKN = 1
;   62: const ERR_TKN           = $00
					; ERR_TKN = 0
;   63: ;
;   64: ; Binary operand operators
;   65: ;
;   66: const SET_TKN           = $BD ; =
					; SET_TKN = 189
;   67: const SETLIST_TKN       = $B9 ; =,
					; SETLIST_TKN = 185
;   68: const ADD_TKN           = $AB ; +
					; ADD_TKN = 171
;   69: const SUB_TKN           = $AD ; -
					; SUB_TKN = 173
;   70: const MUL_TKN           = $AA ; *
					; MUL_TKN = 170
;   71: const DIV_TKN           = $AF ; /
					; DIV_TKN = 175
;   72: const MOD_TKN           = $A5 ; %
					; MOD_TKN = 165
;   73: const OR_TKN            = $BF ; ?
					; OR_TKN = 191
;   74: const EOR_TKN           = $DE ; ^
					; EOR_TKN = 222
;   75: const AND_TKN           = $A6 ; &
					; AND_TKN = 166
;   76: const SHR_TKN           = $D2 ; R
					; SHR_TKN = 210
;   77: const SHL_TKN           = $CC ; L
					; SHL_TKN = 204
;   78: const GT_TKN            = $BE ; >
					; GT_TKN = 190
;   79: const GE_TKN            = $C8 ; H
					; GE_TKN = 200
;   80: const LT_TKN            = $BC ; <
					; LT_TKN = 188
;   81: const LE_TKN            = $C2 ; B
					; LE_TKN = 194
;   82: const NE_TKN            = $D5 ; U
					; NE_TKN = 213
;   83: const EQ_TKN            = $C5 ; E
					; EQ_TKN = 197
;   84: const LOGIC_AND_TKN     = $CE ; N
					; LOGIC_AND_TKN = 206
;   85: const LOGIC_OR_TKN      = $CF ; O
					; LOGIC_OR_TKN = 207
;   86: ;
;   87: ; Unary operand operators
;   88: ;
;   89: const AT_TKN            = $C0 ; @
					; AT_TKN = 192
;   90: const DOT_TKN           = $AE ; .
					; DOT_TKN = 174
;   91: const COLON_TKN         = $BA ; :
					; COLON_TKN = 186
;   92: const NEG_TKN           = $AD ; -
					; NEG_TKN = 173
;   93: const COMP_TKN          = $A3 ; #
					; COMP_TKN = 163
;   94: const LOGIC_NOT_TKN     = $A1 ; !
					; LOGIC_NOT_TKN = 161
;   95: const BPTR_TKN          = $DE ; ^
					; BPTR_TKN = 222
;   96: const WPTR_TKN          = $AA ; *
					; WPTR_TKN = 170
;   97: const INC_TKN           = $C1 ; A
					; INC_TKN = 193
;   98: const DEC_TKN           = $C4 ; D
					; DEC_TKN = 196
;   99: ;
;  100: ; Enclosure tokens
;  101: ;
;  102: const OPEN_PAREN_TKN    = $A8 ; (
					; OPEN_PAREN_TKN = 168
;  103: const CLOSE_PAREN_TKN   = $A9 ; )
					; CLOSE_PAREN_TKN = 169
;  104: const OPEN_BRACKET_TKN  = $DB ; [
					; OPEN_BRACKET_TKN = 219
;  105: const CLOSE_BRACKET_TKN = $DD ; ]
					; CLOSE_BRACKET_TKN = 221
;  106: ;
;  107: ; Misc. tokens
;  108: ;
;  109: const COMMA_TKN         = $AC ; ,
					; COMMA_TKN = 172
;  110: const COMMENT_TKN       = $BB ; ;
					; COMMENT_TKN = 187
;  111: ;
;  112: ; Keyword tokens
;  113: ;
;  114: const CONST_TKN         = $80
					; CONST_TKN = 128
;  115: const BYTE_TKN          = $81
					; BYTE_TKN = 129
;  116: const WORD_TKN          = $82
					; WORD_TKN = 130
;  117: const IF_TKN            = $83
					; IF_TKN = 131
;  118: const ELSEIF_TKN        = $84
					; ELSEIF_TKN = 132
;  119: const ELSE_TKN          = $85
					; ELSE_TKN = 133
;  120: const FIN_TKN           = $86
					; FIN_TKN = 134
;  121: const END_TKN           = $87
					; END_TKN = 135
;  122: const WHILE_TKN         = $88
					; WHILE_TKN = 136
;  123: const LOOP_TKN          = $89
					; LOOP_TKN = 137
;  124: const CASE_TKN          = $8A
					; CASE_TKN = 138
;  125: const OF_TKN            = $8B
					; OF_TKN = 139
;  126: const DEFAULT_TKN       = $8C
					; DEFAULT_TKN = 140
;  127: const ENDCASE_TKN       = $8D
					; ENDCASE_TKN = 141
;  128: const FOR_TKN           = $8E
					; FOR_TKN = 142
;  129: const TO_TKN            = $8F
					; TO_TKN = 143
;  130: const DOWNTO_TKN        = $90
					; DOWNTO_TKN = 144
;  131: const STEP_TKN          = $91
					; STEP_TKN = 145
;  132: const NEXT_TKN          = $92
					; NEXT_TKN = 146
;  133: const REPEAT_TKN        = $93
					; REPEAT_TKN = 147
;  134: const UNTIL_TKN         = $94
					; UNTIL_TKN = 148
;  135: const IFUNC_TKN         = $95
					; IFUNC_TKN = 149
;  136: const NFUNC_TKN         = $96
					; NFUNC_TKN = 150
;  137: const DROP_TKN          = $97
					; DROP_TKN = 151
;  138: const DONE_TKN          = $98
					; DONE_TKN = 152
;  139: const RETURN_TKN        = $99
					; RETURN_TKN = 153
;  140: const BREAK_TKN         = $9A
					; BREAK_TKN = 154
;  141: const START_TKN         = $9B
					; START_TKN = 155
;  142: const EXIT_TKN          = $9C
					; EXIT_TKN = 156
;  143: const EVAL_TKN          = $9D
					; EVAL_TKN = 157
;  144: const FUNC_TKN          = $9E
					; FUNC_TKN = 158
;  145: ;
;  146: ; Types
;  147: ;
;  148: const CONST_TYPE        = $01
					; CONST_TYPE = 1
;  149: const BYTE_TYPE         = $02
					; BYTE_TYPE = 2
;  150: const WORD_TYPE         = $04
					; WORD_TYPE = 4
;  151: const VAR_TYPE          = $06 ; (WORD_TYPE | BYTE_TYPE)
					; VAR_TYPE = 6
;  152: const FUNC_TYPE         = $08
					; FUNC_TYPE = 8
;  153: const FUNC_CONST_TYPE   = $09
					; FUNC_CONST_TYPE = 9
;  154: const ADDR_TYPE         = $0E ; (VAR_TYPE | FUNC_TYPE)
					; ADDR_TYPE = 14
;  155: const LOCAL_TYPE        = $10
					; LOCAL_TYPE = 16
;  156: const BPTR_TYPE         = $20
					; BPTR_TYPE = 32
;  157: const WPTR_TYPE         = $40
					; WPTR_TYPE = 64
;  158: const PTR_TYPE          = $60 ; (BPTR_TYPE | WPTR_TYPE)
					; PTR_TYPE = 96
;  159: const XBYTE_TYPE        = $22 ; (BPTR_TYPE | BYTE_TYPE)
					; XBYTE_TYPE = 34
;  160: const XWORD_TYPE        = $44 ; (WPTR_TYPE | WORD_TYPE)
					; XWORD_TYPE = 68
;  161: const STR_TYPE          = $80
					; STR_TYPE = 128
;  162: ;
;  163: ; Keywords
;  164: ;
;  165: byte keywrds[]
D0148:					; keywrds
;  166: byte                    = "IF",     IF_TKN
	DB	$02
	DB	$49,$46
	DB	$83
;  167: byte                    = "TO",     TO_TKN
	DB	$02
	DB	$54,$4F
	DB	$8F
;  168: byte                    = "IS",     OF_TKN
	DB	$02
	DB	$49,$53
	DB	$8B
;  169: byte                    = "OR",     LOGIC_OR_TKN
	DB	$02
	DB	$4F,$52
	DB	$CF
;  170: byte                    = "FOR",    FOR_TKN
	DB	$03
	DB	$46,$4F,$52
	DB	$8E
;  171: byte                    = "FIN",    FIN_TKN
	DB	$03
	DB	$46,$49,$4E
	DB	$86
;  172: byte                    = "DEF",    IFUNC_TKN
	DB	$03
	DB	$44,$45,$46
	DB	$95
;  173: byte                    = "END",    END_TKN
	DB	$03
	DB	$45,$4E,$44
	DB	$87
;  174: byte                    = "AND",    LOGIC_AND_TKN
	DB	$03
	DB	$41,$4E,$44
	DB	$CE
;  175: byte                    = "NOT",    LOGIC_NOT_TKN
	DB	$03
	DB	$4E,$4F,$54
	DB	$A1
;  176: byte                    = "BYTE",   BYTE_TKN
	DB	$04
	DB	$42,$59,$54,$45
	DB	$81
;  177: byte                    = "WORD",   WORD_TKN
	DB	$04
	DB	$57,$4F,$52,$44
	DB	$82
;  178: byte                    = "DROP",   DROP_TKN
	DB	$04
	DB	$44,$52,$4F,$50
	DB	$97
;  179: byte                    = "ELSE",   ELSE_TKN
	DB	$04
	DB	$45,$4C,$53,$45
	DB	$85
;  180: byte                    = "NEXT",   NEXT_TKN
	DB	$04
	DB	$4E,$45,$58,$54
	DB	$92
;  181: byte                    = "WHEN",   CASE_TKN
	DB	$04
	DB	$57,$48,$45,$4E
	DB	$8A
;  182: byte                    = "LOOP",   LOOP_TKN
	DB	$04
	DB	$4C,$4F,$4F,$50
	DB	$89
;  183: byte                    = "FUNC",   FUNC_TKN
	DB	$04
	DB	$46,$55,$4E,$43
	DB	$9E
;  184: byte                    = "STEP",   STEP_TKN
	DB	$04
	DB	$53,$54,$45,$50
	DB	$91
;  185: byte                    = "EXIT",   EXIT_TKN
	DB	$04
	DB	$45,$58,$49,$54
	DB	$9C
;  186: byte                    = "DONE",   DONE_TKN
	DB	$04
	DB	$44,$4F,$4E,$45
	DB	$98
;  187: byte                    = "WEND",   ENDCASE_TKN
	DB	$04
	DB	$57,$45,$4E,$44
	DB	$8D
;  188: byte                    = "CONST",  CONST_TKN
	DB	$05
	DB	$43,$4F,$4E,$53,$54
	DB	$80
;  189: byte                    = "ELSIF",  ELSEIF_TKN
	DB	$05
	DB	$45,$4C,$53,$49,$46
	DB	$84
;  190: byte                    = "WHILE",  WHILE_TKN
	DB	$05
	DB	$57,$48,$49,$4C,$45
	DB	$88
;  191: byte                    = "UNTIL",  UNTIL_TKN
	DB	$05
	DB	$55,$4E,$54,$49,$4C
	DB	$94
;  192: byte                    = "BREAK",  BREAK_TKN
	DB	$05
	DB	$42,$52,$45,$41,$4B
	DB	$9A
;  193: byte                    = "OTHER",  DEFAULT_TKN
	DB	$05
	DB	$4F,$54,$48,$45,$52
	DB	$8C
;  194: byte                    = "DOWNTO", DOWNTO_TKN
	DB	$06
	DB	$44,$4F,$57,$4E,$54,$4F
	DB	$90
;  195: byte                    = "REPEAT", REPEAT_TKN
	DB	$06
	DB	$52,$45,$50,$45,$41,$54
	DB	$93
;  196: byte                    = "DEFOPT", NFUNC_TKN
	DB	$06
	DB	$44,$45,$46,$4F,$50,$54
	DB	$96
;  197: byte                    = "RETURN", RETURN_TKN
	DB	$06
	DB	$52,$45,$54,$55,$52,$4E
	DB	$99
;  198: byte                    = $FF
	DB	$FF
;  199: ;
;  200: ; Mathematical ops
;  201: ;
;  202: const bops_tblsz        = 18 ; minus 1
					; bops_tblsz = 18
;  203: byte bops_tbl[]         ; Highest precedence
D0341:					; bops_tbl
;  204: byte                    = MUL_TKN, DIV_TKN, MOD_TKN
	DB	$AA
	DB	$AF
	DB	$A5
;  205: byte                    = ADD_TKN, SUB_TKN
	DB	$AB
	DB	$AD
;  206: byte                    = SHR_TKN, SHL_TKN
	DB	$D2
	DB	$CC
;  207: byte                    = AND_TKN
	DB	$A6
;  208: byte                    = EOR_TKN
	DB	$DE
;  209: byte                    = OR_TKN
	DB	$BF
;  210: byte                    = GT_TKN, GE_TKN, LT_TKN, LE_TKN
	DB	$BE
	DB	$C8
	DB	$BC
	DB	$C2
;  211: byte                    = EQ_TKN, NE_TKN
	DB	$C5
	DB	$D5
;  212: byte                    = LOGIC_AND_TKN
	DB	$CE
;  213: byte                    = LOGIC_OR_TKN
	DB	$CF
;  214: byte                    = COMMA_TKN
	DB	$AC
;  215:                         ; Lowest precedence
;  216: byte bops_prec[]        ; Highest precedence
D0360:					; bops_prec
;  217: byte                    = 1, 1, 1
	DB	$01
	DB	$01
	DB	$01
;  218: byte                    = 2, 2
	DB	$02
	DB	$02
;  219: byte                    = 3, 3
	DB	$03
	DB	$03
;  220: byte                    = 4
	DB	$04
;  221: byte                    = 5
	DB	$05
;  222: byte                    = 6
	DB	$06
;  223: byte                    = 7, 7, 7, 7
	DB	$07
	DB	$07
	DB	$07
	DB	$07
;  224: byte                    = 8, 8
	DB	$08
	DB	$08
;  225: byte                    = 9
	DB	$09
;  226: byte                    = 10
	DB	$0A
;  227: byte                    = 11
	DB	$0B
;  228:                         ; Lowest precedence
;  229: byte opstack[16]
D0379:	DS	16			; opstack
;  230: byte precstack[16]
D0395:	DS	16			; precstack
;  231: word opsp = -1
D0411:					; opsp
	DW	$FFFF
;  232: ;
;  233: ; Scanner variables
;  234: ;
;  235: byte  token, tknlen
D0413:	DS	1			; token
D0414:	DS	1			; tknlen
;  236: word  scanptr, tknptr
D0415:	DS	2			; scanptr
D0417:	DS	2			; tknptr
;  237: word  constval
D0419:	DS	2			; constval
;  238: word  lineno = 0
D0421:					; lineno
	DW	$0000
;  239: ;
;  240: ; Compiler output messages
;  241: ;
;  242: byte entrypt_str[]      = "START: "
D0423:					; entrypt_str
	DB	$07
	DB	$53,$54,$41,$52,$54,$3A,$20
;  243: byte dup_id[]           = "DUPLICATE IDENTIFIER"
D0431:					; dup_id
	DB	$14
	DB	$44,$55,$50,$4C,$49,$43,$41,$54
	DB	$45,$20,$49,$44,$45,$4E,$54,$49
	DB	$46,$49,$45,$52
;  244: byte undecl_id[]        = "UNDECLARED IDENTIFIER"
D0452:					; undecl_id
	DB	$15
	DB	$55,$4E,$44,$45,$43,$4C,$41,$52
	DB	$45,$44,$20,$49,$44,$45,$4E,$54
	DB	$49,$46,$49,$45,$52
;  245: byte bad_cnst[]         = "BAD CONSTANT"
D0474:					; bad_cnst
	DB	$0C
	DB	$42,$41,$44,$20,$43,$4F,$4E,$53
	DB	$54,$41,$4E,$54
;  246: byte bad_offset[]       = "BAD STRUCT OFFSET"
D0487:					; bad_offset
	DB	$11
	DB	$42,$41,$44,$20,$53,$54,$52,$55
	DB	$43,$54,$20,$4F,$46,$46,$53,$45
	DB	$54
;  247: byte bad_decl[]         = "BAD DECLARATION"
D0505:					; bad_decl
	DB	$0F
	DB	$42,$41,$44,$20,$44,$45,$43,$4C
	DB	$41,$52,$41,$54,$49,$4F,$4E
;  248: byte bad_op[]           = "BAD OPERATION"
D0521:					; bad_op
	DB	$0D
	DB	$42,$41,$44,$20,$4F,$50,$45,$52
	DB	$41,$54,$49,$4F,$4E
;  249: byte bad_stmnt[]        = "BAD STATMENT"
D0535:					; bad_stmnt
	DB	$0C
	DB	$42,$41,$44,$20,$53,$54,$41,$54
	DB	$4D,$45,$4E,$54
;  250: byte bad_expr[]         = "BAD EXPRESSION"
D0548:					; bad_expr
	DB	$0E
	DB	$42,$41,$44,$20,$45,$58,$50,$52
	DB	$45,$53,$53,$49,$4F,$4E
;  251: byte bad_syntax[]       = "BAD SYNTAX"
D0563:					; bad_syntax
	DB	$0A
	DB	$42,$41,$44,$20,$53,$59,$4E,$54
	DB	$41,$58
;  252: byte estk_overflw[]     = "EVAL STACK OVERFLOW"
D0574:					; estk_overflw
	DB	$13
	DB	$45,$56,$41,$4C,$20,$53,$54,$41
	DB	$43,$4B,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  253: byte estk_underflw[]    = "EVAL STACK UNDERFLOW"
D0594:					; estk_underflw
	DB	$14
	DB	$45,$56,$41,$4C,$20,$53,$54,$41
	DB	$43,$4B,$20,$55,$4E,$44,$45,$52
	DB	$46,$4C,$4F,$57
;  254: byte local_overflw[]    = "LOCAL FRAME OVERFLOW"
D0615:					; local_overflw
	DB	$14
	DB	$4C,$4F,$43,$41,$4C,$20,$46,$52
	DB	$41,$4D,$45,$20,$4F,$56,$45,$52
	DB	$46,$4C,$4F,$57
;  255: byte global_sym_overflw[] = "GLOBAL SYMBOL TABLE OVERFLOW"
D0636:					; global_sym_overflw
	DB	$1C
	DB	$47,$4C,$4F,$42,$41,$4C,$20,$53
	DB	$59,$4D,$42,$4F,$4C,$20,$54,$41
	DB	$42,$4C,$45,$20,$4F,$56,$45,$52
	DB	$46,$4C,$4F,$57
;  256: byte local_sym_overflw[] = "LOCAL SYMBOL TABLE OVERFLOW"
D0665:					; local_sym_overflw
	DB	$1B
	DB	$4C,$4F,$43,$41,$4C,$20,$53,$59
	DB	$4D,$42,$4F,$4C,$20,$54,$41,$42
	DB	$4C,$45,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  257: byte ctag_full[]        = "CODE LABEL OVERFLOW"
D0693:					; ctag_full
	DB	$13
	DB	$43,$4F,$44,$45,$20,$4C,$41,$42
	DB	$45,$4C,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  258: byte no_close_paren[]   = "MISSING CLOSING PAREN"
D0713:					; no_close_paren
	DB	$15
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$43,$4C,$4F,$53,$49,$4E,$47,$20
	DB	$50,$41,$52,$45,$4E
;  259: byte no_close_bracket[] = "MISSING CLOSING BRACKET"
D0735:					; no_close_bracket
	DB	$17
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$43,$4C,$4F,$53,$49,$4E,$47,$20
	DB	$42,$52,$41,$43,$4B,$45,$54
;  260: byte missing_op[]       = "MISSING OPERAND"
D0759:					; missing_op
	DB	$0F
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$4F,$50,$45,$52,$41,$4E,$44
;  261: byte no_fin[]           = "MISSING FIN"
D0775:					; no_fin
	DB	$0B
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$46,$49,$4E
;  262: byte no_loop[]          = "MISSING LOOP"
D0787:					; no_loop
	DB	$0C
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$4C,$4F,$4F,$50
;  263: byte no_until[]         = "MISSING UNTIL"
D0800:					; no_until
	DB	$0D
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$55,$4E,$54,$49,$4C
;  264: byte no_done[]          = "MISSING DONE"
D0814:					; no_done
	DB	$0C
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$44,$4F,$4E,$45
;  265: byte no_local_init[]    = "NO INITIALIZED LOCALS"
D0827:					; no_local_init
	DB	$15
	DB	$4E,$4F,$20,$49,$4E,$49,$54,$49
	DB	$41,$4C,$49,$5A,$45,$44,$20,$4C
	DB	$4F,$43,$41,$4C,$53
;  266: ;
;  267: ; Runtime functions
;  268: ;
;  269: byte runtime0[]         = "romcall"
D0849:					; runtime0
	DB	$07
	DB	$72,$6F,$6D,$63,$61,$6C,$6C
;  270: byte RUNTIME0[]         = "ROMCALL"
D0857:					; RUNTIME0
	DB	$07
	DB	$52,$4F,$4D,$43,$41,$4C,$4C
;  271: byte runtime1[]         = "syscall"
D0865:					; runtime1
	DB	$07
	DB	$73,$79,$73,$63,$61,$6C,$6C
;  272: byte RUNTIME1[]         = "SYSCALL"
D0873:					; RUNTIME1
	DB	$07
	DB	$53,$59,$53,$43,$41,$4C,$4C
;  273: byte runtime2[]         = "memset"
D0881:					; runtime2
	DB	$06
	DB	$6D,$65,$6D,$73,$65,$74
;  274: byte RUNTIME2[]         = "MEMSET"
D0888:					; RUNTIME2
	DB	$06
	DB	$4D,$45,$4D,$53,$45,$54
;  275: byte runtime3[]         = "memcpy"
D0895:					; runtime3
	DB	$06
	DB	$6D,$65,$6D,$63,$70,$79
;  276: byte RUNTIME3[]         = "MEMCPY"
D0902:					; RUNTIME3
	DB	$06
	DB	$4D,$45,$4D,$43,$50,$59
;  277: byte runtime4[]         = "cout"
D0909:					; runtime4
	DB	$04
	DB	$63,$6F,$75,$74
;  278: byte RUNTIME4[]         = "COUT"
D0914:					; RUNTIME4
	DB	$04
	DB	$43,$4F,$55,$54
;  279: byte runtime5[]         = "cin"
D0919:					; runtime5
	DB	$03
	DB	$63,$69,$6E
;  280: byte RUNTIME5[]         = "CIN"
D0923:					; RUNTIME5
	DB	$03
	DB	$43,$49,$4E
;  281: byte runtime6[]         = "prstr"
D0927:					; runtime6
	DB	$05
	DB	$70,$72,$73,$74,$72
;  282: byte RUNTIME6[]         = "PRSTR"
D0933:					; RUNTIME6
	DB	$05
	DB	$50,$52,$53,$54,$52
;  283: byte runtime7[]         = "rdstr"
D0939:					; runtime7
	DB	$05
	DB	$72,$64,$73,$74,$72
;  284: byte RUNTIME7[]         = "RDSTR"
D0945:					; RUNTIME7
	DB	$05
	DB	$52,$44,$53,$54,$52
;  285: ;
;  286: ; Parser variables
;  287: ;
;  288: byte infunc             = 0
D0951:					; infunc
	DB	$00
;  289: byte stack_loop         = 0
D0952:					; stack_loop
	DB	$00
;  290: byte prevstmnt          = 0
D0953:					; prevstmnt
	DB	$00
;  291: word retfunc_tag        = 0
D0954:					; retfunc_tag
	DW	$0000
;  292: word break_tag          = 0
D0956:					; break_tag
	DW	$0000
;  293: func parse_expr_01, parse_module_01
;  294: ;
;  295: ; Defines for ASM routines
;  296: ;
;  297: asm equates
C0002:					; equates()
;  298:         TMP     EQU     $F0
        TMP     EQU     $F0
;  299:         TMPL    EQU     TMP
        TMPL    EQU     TMP
;  300:         TMPH    EQU     TMP+1
        TMPH    EQU     TMP+1
;  301:         SRC     EQU     TMP
        SRC     EQU     TMP
;  302:         SRCL    EQU     SRC
        SRCL    EQU     SRC
;  303:         SRCH    EQU     SRC+1
        SRCH    EQU     SRC+1
;  304:         DST     EQU     SRC+2
        DST     EQU     SRC+2
;  305:         DSTL    EQU     DST
        DSTL    EQU     DST
;  306:         DSTH    EQU     DST+1
        DSTH    EQU     DST+1
;  307:         ESP     EQU     DST+2
        ESP     EQU     DST+2
;  308: 		SAVEESP	EQU		ESP+1
		SAVEESP	EQU		ESP+1
;  309: 		SAVESP	EQU		SAVEESP+1
		SAVESP	EQU		SAVEESP+1
;  310: 		SAVEFP	EQU		SAVESP+1
		SAVEFP	EQU		SAVESP+1
;  311: 		SAVETMR	EQU		SAVEFP+2
		SAVETMR	EQU		SAVEFP+2
;  312: 		SAVEINT	EQU		SAVETMR+2
		SAVEINT	EQU		SAVETMR+2
;  313: 		TMRVEC	EQU		$03E8
		TMRVEC	EQU		$03E8
;  314: 		INTVEC	EQU		$03EA
		INTVEC	EQU		$03EA
;  315: JMPTMP:	JMP		(TMP)
JMPTMP:	JMP		(TMP)
;  316: STKOVFLW:
STKOVFLW:
;  317: 		LDY		#$02
		LDY		#$02
;  318: 		JMP		EXECRET
		JMP		EXECRET
;  319: BRKCHK:
BRKCHK:
;  320: 		LDA		$C000
		LDA		$C000
;  321: 		CMP		#$83		; CTRL-C
		CMP		#$83		; CTRL-C
;  322: 		BNE		:+
		BNE		:+
;  323: 		BIT		$C010
		BIT		$C010
;  324: 		LDY		#$01
		LDY		#$01
;  325: 		JMP		EXECRET
		JMP		EXECRET
;  326: :
:
;  327: end
	RTS
;  328: ;
;  329: ; ENTER MODULE UNDER TEST
;  330: ;
;  331: asm execentry
C0004:					; execentry()
;  332:         LDA     ESTKL,X
        LDA     ESTKL,X
;  333:         STA     TMPL
        STA     TMPL
;  334:         LDA     ESTKH,X
        LDA     ESTKH,X
;  335:         STA     TMPH
        STA     TMPH
;  336: 		STX		SAVEESP
		STX		SAVEESP
;  337: 		TSX
		TSX
;  338: 		STX		SAVESP
		STX		SAVESP
;  339: 		LDA		FRMPL
		LDA		FRMPL
;  340: 		STA		SAVEFP
		STA		SAVEFP
;  341: 		LDA		FRMPH
		LDA		FRMPH
;  342: 		STA		SAVEFP+1
		STA		SAVEFP+1
;  343: 		LDA		TMRVEC
		LDA		TMRVEC
;  344: 		STA		SAVETMR
		STA		SAVETMR
;  345: 		LDA		TMRVEC+1
		LDA		TMRVEC+1
;  346: 		STA		SAVETMR+1
		STA		SAVETMR+1
;  347: 		LDA		INTVEC
		LDA		INTVEC
;  348: 		STA		SAVEINT
		STA		SAVEINT
;  349: 		LDA		INTVEC+1
		LDA		INTVEC+1
;  350: 		STA		SAVEINT+1
		STA		SAVEINT+1
;  351: 		LDA		#<BRKCHK
		LDA		#<BRKCHK
;  352: 		STA		TMRVEC
		STA		TMRVEC
;  353: 		LDA		#>BRKCHK
		LDA		#>BRKCHK
;  354: 		STA		TMRVEC+1
		STA		TMRVEC+1
;  355: 		LDA		#<STKOVFLW
		LDA		#<STKOVFLW
;  356: 		STA		INTVEC
		STA		INTVEC
;  357: 		LDA		#>STKOVFLW
		LDA		#>STKOVFLW
;  358: 		STA		INTVEC+1
		STA		INTVEC+1
;  359: 		LDX		#ESTKSZ/2
		LDX		#ESTKSZ/2
;  360: 		JSR		JMPTMP
		JSR		JMPTMP
;  361: 		LDY		#$00
		LDY		#$00
;  362: EXECRET:
EXECRET:
;  363: 		STY		TMP
		STY		TMP
;  364: 		BIT		ROMIN
		BIT		ROMIN
;  365: 		BIT		$C054
		BIT		$C054
;  366: 		BIT		$C051
		BIT		$C051
;  367: 		BIT		$C058
		BIT		$C058
;  368: 		JSR		$FB39		; SET TEXT MODE
		JSR		$FB39		; SET TEXT MODE
;  369: 		BIT		LCBNK2
		BIT		LCBNK2
;  370: 		LDA		SAVEFP
		LDA		SAVEFP
;  371: 		STA		FRMPL
		STA		FRMPL
;  372: 		LDA		SAVEFP+1
		LDA		SAVEFP+1
;  373: 		STA		FRMPH
		STA		FRMPH
;  374: 		LDA		SAVETMR
		LDA		SAVETMR
;  375: 		STA		TMRVEC
		STA		TMRVEC
;  376: 		LDA		SAVETMR+1
		LDA		SAVETMR+1
;  377: 		STA		TMRVEC+1
		STA		TMRVEC+1
;  378: 		LDA		SAVEINT
		LDA		SAVEINT
;  379: 		STA		INTVEC
		STA		INTVEC
;  380: 		LDA		SAVEINT+1
		LDA		SAVEINT+1
;  381: 		STA		INTVEC+1
		STA		INTVEC+1
;  382: 		LDX		SAVESP
		LDX		SAVESP
;  383: 		TXS
		TXS
;  384: 		LDX		SAVEESP
		LDX		SAVEESP
;  385: 		LDY		TMP
		LDY		TMP
;  386: 		STY		ESTKL,X
		STY		ESTKL,X
;  387: 		LDY		#$00
		LDY		#$00
;  388: 		STY		ESTKH,X
		STY		ESTKH,X
;  389: end
	RTS
;  390: ;
;  391: ; CALL 6502 ROUTINE
;  392: ; ROMCALL(AREG, XREG, YREG, STATUS, ADDR)
;  393: ;
;  394: asm romcall
C0006:					; romcall()
;  395:         PHP
        PHP
;  396:         LDA     ESTKL,X
        LDA     ESTKL,X
;  397:         STA     TMPL
        STA     TMPL
;  398:         LDA     ESTKH,X
        LDA     ESTKH,X
;  399:         STA     TMPH
        STA     TMPH
;  400:         INX
        INX
;  401:         LDA     ESTKL,X
        LDA     ESTKL,X
;  402:         PHA
        PHA
;  403:         INX
        INX
;  404:         LDA     ESTKL,X
        LDA     ESTKL,X
;  405:         TAY
        TAY
;  406:         INX
        INX
;  407:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  408:         PHA
        PHA
;  409:         LDA     ESTKL,X
        LDA     ESTKL,X
;  410:         INX
        INX
;  411:         STX     ESP
        STX     ESP
;  412:         TAX
        TAX
;  413:         PLA
        PLA
;  414:         BIT     ROMIN
        BIT     ROMIN
;  415:         PLP
        PLP
;  416:         JSR     JMPTMP
        JSR     JMPTMP
;  417:         PHP
        PHP
;  418:         BIT     LCBNK2
        BIT     LCBNK2
;  419:         STA     REGVALS+0
        STA     REGVALS+0
;  420:         STX     REGVALS+1
        STX     REGVALS+1
;  421:         STY     REGVALS+2
        STY     REGVALS+2
;  422:         PLA
        PLA
;  423:         STA     REGVALS+3
        STA     REGVALS+3
;  424:         LDX     ESP
        LDX     ESP
;  425:         LDA     #<REGVALS
        LDA     #<REGVALS
;  426:         LDY     #>REGVALS
        LDY     #>REGVALS
;  427:         STA     ESTKL,X
        STA     ESTKL,X
;  428:         STY     ESTKH,X
        STY     ESTKH,X
;  429:         PLP
        PLP
;  430:         RTS
        RTS
;  431: REGVALS: DS 4
REGVALS: DS 4
;  432: end
	RTS
;  433: ;
;  434: ; BREAK INTO MONITOR
;  435: ;
;  436: asm monitor
C0008:					; monitor()
;  437: 		STX		ESP
		STX		ESP
;  438: 		LDA		#$4C
		LDA		#$4C
;  439: 		STA		$03F8
		STA		$03F8
;  440: 		LDA		#<REENTER
		LDA		#<REENTER
;  441: 		STA		$03F9
		STA		$03F9
;  442: 		LDA		#>REENTER
		LDA		#>REENTER
;  443: 		STA		$03FA
		STA		$03FA
;  444: 		TSX
		TSX
;  445: 		TXA
		TXA
;  446: 		PHA
		PHA
;  447: 		BIT		ROMIN
		BIT		ROMIN
;  448: 		JMP		$FF69
		JMP		$FF69
;  449: REENTER: PLA
REENTER: PLA
;  450: 		TAX
		TAX
;  451: 		TXS
		TXS
;  452: 		LDX		ESP
		LDX		ESP
;  453: 		BIT		LCBNK2
		BIT		LCBNK2
;  454: end
	RTS
;  455: ;
;  456: ; RETURN EVAL STACK POINTER
;  457: ;
;  458: ;asm estk
;  459: ;		TXA
;  460: ;		DEX
;  461: ;		STA		ESTKL,X
;  462: ;		LDA		#$00
;  463: ;		STA		ESTKH,X
;  464: ;end
;  465: ;
;  466: ; ASSERT EVAL STACK POINTER VALUE
;  467: ;
;  468: ;asm assert_estk
;  469: ;		INX
;  470: ;		TXA
;  471: ;		CMP		ESTKL-1,X
;  472: ;		BEQ		:+
;  473: ;		BRK
;  474: ;:
;  475: ;end
;  476: ;
;  477: ; CALL PRODOS
;  478: ; SYSCALL(CMD, PARAMS)
;  479: ;
;  480: asm syscall
C0010:					; syscall()
;  481:         LDA     ESTKL,X
        LDA     ESTKL,X
;  482:         LDY     ESTKH,X
        LDY     ESTKH,X
;  483:         STA     PARAMS
        STA     PARAMS
;  484:         STY     PARAMS+1
        STY     PARAMS+1
;  485:         INX
        INX
;  486:         LDA     ESTKL,X
        LDA     ESTKL,X
;  487:         STA     CMD
        STA     CMD
;  488:         STX     ESP
        STX     ESP
;  489:         BIT     ROMIN
        BIT     ROMIN
;  490:         JSR     $BF00
        JSR     $BF00
;  491: CMD:    DB      00
CMD:    DB      00
;  492: PARAMS: DW      0000
PARAMS: DW      0000
;  493:         BIT     LCBNK2
        BIT     LCBNK2
;  494:         LDX     ESP
        LDX     ESP
;  495:         STA     ESTKL,X
        STA     ESTKL,X
;  496:         LDY     #$00
        LDY     #$00
;  497:         STY     ESTKH,X
        STY     ESTKH,X
;  498: end
	RTS
;  499: ;
;  500: ; SET MEMORY TO VALUE
;  501: ; MEMSET(VALUE, ADDR, SIZE)
;  502: ;
;  503: asm memset
C0012:					; memset()
;  504:         LDY     #$00
        LDY     #$00
;  505:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  506:         STA     DSTL
        STA     DSTL
;  507:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  508:         STA     DSTH
        STA     DSTH
;  509:         INC     ESTKL,X
        INC     ESTKL,X
;  510:         INC     ESTKH,X
        INC     ESTKH,X
;  511: SETMEM: DEC     ESTKL,X
SETMEM: DEC     ESTKL,X
;  512:         BNE     :+
        BNE     :+
;  513:         DEC     ESTKH,X
        DEC     ESTKH,X
;  514:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  515: :       LDA     ESTKL+2,X
:       LDA     ESTKL+2,X
;  516:         STA     (DST),Y
        STA     (DST),Y
;  517:         INY
        INY
;  518:         BNE     :+
        BNE     :+
;  519:         INC     DSTH
        INC     DSTH
;  520: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  521:         BNE     :+
        BNE     :+
;  522:         DEC     ESTKH,X
        DEC     ESTKH,X
;  523:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  524: :       LDA     ESTKH+2,X
:       LDA     ESTKH+2,X
;  525:         STA     (DST),Y
        STA     (DST),Y
;  526:         INY
        INY
;  527:         BNE     SETMEM
        BNE     SETMEM
;  528:         INC     DSTH
        INC     DSTH
;  529:         BNE     SETMEM
        BNE     SETMEM
;  530: MEMEXIT: INX
MEMEXIT: INX
;  531:         INX
        INX
;  532:         INX
        INX
;  533: end
	RTS
;  534: ;
;  535: ; COPY MEMORY
;  536: ; MEMCPY(SRCADDR, DSTADDR, SIZE)
;  537: ;
;  538: asm memcpy
C0014:					; memcpy()
;  539:         LDY     #$00
        LDY     #$00
;  540:         LDA     ESTKL,X
        LDA     ESTKL,X
;  541:         BNE     :+
        BNE     :+
;  542:         LDA     ESTKH,X
        LDA     ESTKH,X
;  543:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  544: :       LDA     ESTKL+1,X
:       LDA     ESTKL+1,X
;  545:         STA     DSTL
        STA     DSTL
;  546:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  547:         STA     DSTH
        STA     DSTH
;  548:         LDA     ESTKL+2,X
        LDA     ESTKL+2,X
;  549:         STA     SRCL
        STA     SRCL
;  550:         LDA     ESTKH+2,X
        LDA     ESTKH+2,X
;  551:         STA     SRCH
        STA     SRCH
;  552:         CMP     DSTH
        CMP     DSTH
;  553:         BCC     REVCPY
        BCC     REVCPY
;  554:         BNE     FORCPY
        BNE     FORCPY
;  555:         LDA     SRCL
        LDA     SRCL
;  556:         CMP     DSTL
        CMP     DSTL
;  557:         BCS     FORCPY
        BCS     FORCPY
;  558: REVCPY:             ; REVERSE DIRECTION COPY
REVCPY:             ; REVERSE DIRECTION COPY
;  559: ;       CLC
;  560:         LDA     ESTKL,X
        LDA     ESTKL,X
;  561:         ADC     DSTL
        ADC     DSTL
;  562:         STA     DSTL
        STA     DSTL
;  563:         LDA     ESTKH,X
        LDA     ESTKH,X
;  564:         ADC     DSTH
        ADC     DSTH
;  565:         STA     DSTH
        STA     DSTH
;  566:         CLC
        CLC
;  567:         LDA     ESTKL,X
        LDA     ESTKL,X
;  568:         ADC     SRCL
        ADC     SRCL
;  569:         STA     SRCL
        STA     SRCL
;  570:         LDA     ESTKH,X
        LDA     ESTKH,X
;  571:         ADC     SRCH
        ADC     SRCH
;  572:         STA     SRCH
        STA     SRCH
;  573:         INC     ESTKH,X
        INC     ESTKH,X
;  574: REVCPYLP:
REVCPYLP:
;  575:         LDA     DSTL
        LDA     DSTL
;  576:         BNE     :+
        BNE     :+
;  577:         DEC     DSTH
        DEC     DSTH
;  578: :       DEC     DSTL
:       DEC     DSTL
;  579:         LDA     SRCL
        LDA     SRCL
;  580:         BNE     :+
        BNE     :+
;  581:         DEC     SRCH
        DEC     SRCH
;  582: :       DEC     SRCL
:       DEC     SRCL
;  583:         LDA     (SRC),Y
        LDA     (SRC),Y
;  584:         STA     (DST),Y
        STA     (DST),Y
;  585:         DEC     ESTKL,X
        DEC     ESTKL,X
;  586:         BNE     REVCPYLP
        BNE     REVCPYLP
;  587:         DEC     ESTKH,X
        DEC     ESTKH,X
;  588:         BNE     REVCPYLP
        BNE     REVCPYLP
;  589:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  590: FORCPY: INC     ESTKH,X
FORCPY: INC     ESTKH,X
;  591: FORCPYLP:
FORCPYLP:
;  592:         LDA     (SRC),Y
        LDA     (SRC),Y
;  593:         STA     (DST),Y
        STA     (DST),Y
;  594:         INC     DSTL
        INC     DSTL
;  595:         BNE     :+
        BNE     :+
;  596:         INC     DSTH
        INC     DSTH
;  597: :       INC     SRCL
:       INC     SRCL
;  598:         BNE     :+
        BNE     :+
;  599:         INC     SRCH
        INC     SRCH
;  600: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  601:         BNE     FORCPYLP
        BNE     FORCPYLP
;  602:         DEC     ESTKH,X
        DEC     ESTKH,X
;  603:         BNE     FORCPYLP
        BNE     FORCPYLP
;  604:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  605: end
	RTS
;  606: ;
;  607: ; CHAR OUT
;  608: ; COUT(CHAR)
;  609: ;
;  610: asm cout
C0016:					; cout()
;  611:         LDA     ESTKL,X
        LDA     ESTKL,X
;  612:         INX
        INX
;  613:         ORA     #$80
        ORA     #$80
;  614:         BIT     ROMIN
        BIT     ROMIN
;  615:         JSR     $FDED
        JSR     $FDED
;  616:         BIT     LCBNK2
        BIT     LCBNK2
;  617: end
	RTS
;  618: ;
;  619: ; CHAR IN
;  620: ; RDKEY()
;  621: ;
;  622: asm cin
C0018:					; cin()
;  623:         BIT     ROMIN
        BIT     ROMIN
;  624:         STX     ESP
        STX     ESP
;  625:         JSR     $FD0C
        JSR     $FD0C
;  626:         LDX     ESP
        LDX     ESP
;  627:         BIT     LCBNK2
        BIT     LCBNK2
;  628:         DEX
        DEX
;  629:         AND     #$7F
        AND     #$7F
;  630:         STA     ESTKL,X
        STA     ESTKL,X
;  631:         LDY     #$00
        LDY     #$00
;  632:         STY     ESTKH,X
        STY     ESTKH,X
;  633: end
	RTS
;  634: ;
;  635: ; PRINT STRING
;  636: ; PRSTR(STR)
;  637: ;
;  638: asm prstr
C0020:					; prstr()
;  639:         LDY     #$00
        LDY     #$00
;  640:         LDA     ESTKL,X
        LDA     ESTKL,X
;  641:         STA     SRCL
        STA     SRCL
;  642:         LDA     ESTKH,X
        LDA     ESTKH,X
;  643:         STA     SRCH
        STA     SRCH
;  644:         BIT     ROMIN
        BIT     ROMIN
;  645:         LDA     (SRC),Y
        LDA     (SRC),Y
;  646:         STA     ESTKL,X
        STA     ESTKL,X
;  647:         BEQ     :+
        BEQ     :+
;  648: _PRS1:  INY
_PRS1:  INY
;  649:         LDA     (SRC),Y
        LDA     (SRC),Y
;  650:         ORA     #$80
        ORA     #$80
;  651:         JSR     $FDED
        JSR     $FDED
;  652:         TYA
        TYA
;  653:         CMP     ESTKL,X
        CMP     ESTKL,X
;  654:         BNE     _PRS1
        BNE     _PRS1
;  655: :       INX
:       INX
;  656:         BIT     LCBNK2
        BIT     LCBNK2
;  657: end
	RTS
;  658: ;
;  659: ; READ STRING
;  660: ; STR = RDSTR(PROMPTCHAR)
;  661: ;
;  662: asm rdstr
C0022:					; rdstr()
;  663:         LDA     ESTKL,X
        LDA     ESTKL,X
;  664:         STA     $33
        STA     $33
;  665:         STX     ESP
        STX     ESP
;  666:         BIT     ROMIN
        BIT     ROMIN
;  667:         JSR     $FD6A
        JSR     $FD6A
;  668:         BIT     LCBNK2
        BIT     LCBNK2
;  669:         STX     $01FF
        STX     $01FF
;  670: :       LDA     $01FF,X
:       LDA     $01FF,X
;  671:         AND     #$7F
        AND     #$7F
;  672:         STA     $01FF,X
        STA     $01FF,X
;  673:         DEX
        DEX
;  674:         BPL     :-
        BPL     :-
;  675:         LDX     ESP
        LDX     ESP
;  676:         LDA     #$FF
        LDA     #$FF
;  677:         STA     ESTKL,X
        STA     ESTKL,X
;  678:         LDA     #$01
        LDA     #$01
;  679:         STA     ESTKH,X
        STA     ESTKH,X
;  680: end
	RTS
;  681: ;def toupper_11(c)
;  682: ;   if c >= 'a'
;  683: ;       if c <= 'z'
;  684: ;           return c - $20
;  685: ;       fin
;  686: ;   fin
;  687: ;   return c
;  688: ;end
;  689: asm toupper_11
C0024:					; toupper_11()
;  690:         LDA     ESTKL,X
        LDA     ESTKL,X
;  691:         CMP     #'a'
        CMP     #'a'
;  692:         BCC     :+
        BCC     :+
;  693:         CMP     #'z'+1
        CMP     #'z'+1
;  694:         BCS     :+
        BCS     :+
;  695:         SEC
        SEC
;  696:         SBC     #$20
        SBC     #$20
;  697:         STA     ESTKL,X
        STA     ESTKL,X
;  698: :
:
;  699: end
	RTS
;  700: ;
;  701: ; EXIT
;  702: ;
;  703: asm exit
C0026:					; exit()
;  704:         JSR $BF00
        JSR $BF00
;  705:         DB  $65
        DB  $65
;  706:         DW  EXITTBL
        DW  EXITTBL
;  707: EXITTBL:
EXITTBL:
;  708:         DB  4
        DB  4
;  709:         DB  0
        DB  0
;  710: end
	RTS
;  711: ;
;  712: ; ProDOS routines
;  713: ;
;  714: def getpfx_11(path)
C0028:					; getpfx_11()
					; path = 2
;  715:     byte params[3]
					; params = 4
;  716: 
;  717:     ^path    = 0
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$70			; SB
;  718:     params.0 = 1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  719:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  720:     perr     = syscall($C7, @params)
	DB	$2A,$C7			; CB	199
	DB	$28,$04			; LLA	4
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  721:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  722: end
;  723: def setpfx_11(path)
C0030:					; setpfx_11()
					; path = 2
;  724:     byte params[3]
					; params = 4
;  725: 
;  726:     params.0 = 1
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  727:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  728:     perr     = syscall($C6, @params)
	DB	$2A,$C6			; CB	198
	DB	$28,$04			; LLA	4
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  729:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  730: end
;  731: def open_21(path, buff)
C0032:					; open_21()
					; path = 2
					; buff = 4
;  732:     byte params[6]
					; params = 6
;  733: 
;  734:     params.0 = 3
	JSR	INTERP
	DB	$58,$0C,$02		; ENTER	12,2
	DB	$28,$06			; LLA	6
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  735:     params:1 = path
	DB	$28,$07			; LLA	7
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  736:     params:3 = buff
	DB	$28,$09			; LLA	9
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  737:     params.5 = 0
	DB	$28,$0B			; LLA	11
	DB	$00			; ZERO
	DB	$70			; SB
;  738:     perr     = syscall($C8, @params)
	DB	$2A,$C8			; CB	200
	DB	$28,$06			; LLA	6
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  739:     return params.5
	DB	$28,$0B			; LLA	11
	DB	$60			; LB
	DB	$5A			; LEAVE
;  740: end
;  741: def close_11(refnum)
C0034:					; close_11()
					; refnum = 2
;  742:     byte params[2]
					; params = 4
;  743: 
;  744:     params.0 = 1
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  745:     params.1 = refnum
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  746:     perr     = syscall($CC, @params)
	DB	$2A,$CC			; CB	204
	DB	$28,$04			; LLA	4
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  747:     return perr
	DB	$68,<D0019,>D0019	; LAB	D0019
	DB	$5A			; LEAVE
;  748: end
;  749: def read_31(refnum, buff, len)
C0036:					; read_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  750:     byte params[8]
					; params = 8
;  751: 
;  752:     params.0 = 4
	JSR	INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  753:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  754:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  755:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  756:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  757:     perr     = syscall($CA, @params)
	DB	$2A,$CA			; CB	202
	DB	$28,$08			; LLA	8
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  758:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  759: end
;  760: def write_31(refnum, buff, len)
C0038:					; write_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  761:     byte params[8]
					; params = 8
;  762: 
;  763:     params.0 = 4
	JSR	INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  764:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  765:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  766:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  767:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  768:     perr     = syscall($CB, @params)
	DB	$2A,$CB			; CB	203
	DB	$28,$08			; LLA	8
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  769:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  770: end
;  771: def create_41(path, access, type, aux)
C0040:					; create_41()
					; path = 2
					; access = 4
					; type = 6
					; aux = 8
;  772:     byte params[12]
					; params = 10
;  773: 
;  774:     params.0  = 7
	JSR	INTERP
	DB	$58,$16,$04		; ENTER	22,4
	DB	$28,$0A			; LLA	10
	DB	$2A,$07			; CB	7
	DB	$70			; SB
;  775:     params:1  = path
	DB	$28,$0B			; LLA	11
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  776:     params.3  = access
	DB	$28,$0D			; LLA	13
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  777:     params.4  = type
	DB	$28,$0E			; LLA	14
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  778:     params:5  = aux
	DB	$28,$0F			; LLA	15
	DB	$66,$08			; LLW	8
	DB	$72			; SW
;  779:     params.7  = $1
	DB	$28,$11			; LLA	17
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  780:     params:8  = 0
	DB	$28,$12			; LLA	18
	DB	$00			; ZERO
	DB	$72			; SW
;  781:     params:10 = 0
	DB	$28,$14			; LLA	20
	DB	$00			; ZERO
	DB	$72			; SW
;  782:     perr      = syscall($C0, @params)
	DB	$2A,$C0			; CB	192
	DB	$28,$0A			; LLA	10
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  783:     return perr
	DB	$68,<D0019,>D0019	; LAB	D0019
	DB	$5A			; LEAVE
;  784: end
;  785: def destroy_11(path)
C0042:					; destroy_11()
					; path = 2
;  786:     byte params[12]
					; params = 4
;  787: 
;  788:     params.0 = 1
	JSR	INTERP
	DB	$58,$10,$01		; ENTER	16,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  789:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  790:     perr     = syscall($C1, @params)
	DB	$2A,$C1			; CB	193
	DB	$28,$04			; LLA	4
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  791:     return perr
	DB	$68,<D0019,>D0019	; LAB	D0019
	DB	$5A			; LEAVE
;  792: end
;  793: def newline_31(refnum, emask, nlchar)
C0044:					; newline_31()
					; refnum = 2
					; emask = 4
					; nlchar = 6
;  794:     byte params[4]
					; params = 8
;  795: 
;  796:     params.0 = 3
	JSR	INTERP
	DB	$58,$0C,$03		; ENTER	12,3
	DB	$28,$08			; LLA	8
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  797:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  798:     params.2 = emask
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  799:     params.3 = nlchar
	DB	$28,$0B			; LLA	11
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  800:     perr     = syscall($C9, @params)
	DB	$2A,$C9			; CB	201
	DB	$28,$08			; LLA	8
	DB	$54,<C0010,>C0010	; CALL	C0010
	DB	$78,<D0019,>D0019	; SAB	D0019
;  801:     return perr
	DB	$68,<D0019,>D0019	; LAB	D0019
	DB	$5A			; LEAVE
;  802: end
;  803: def crout
C0046:					; crout()
;  804:     cout($0D)
	JSR	INTERP
	DB	$2A,$0D			; CB	13
	DB	$54,<C0016,>C0016	; CALL	C0016
;  805: end
	DB	$5C			; RET
;  806: def prbyte_10(h)
C0048:					; prbyte_10()
					; h = 2
;  807:     cout('$')
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0016,>C0016	; CALL	C0016
;  808:     drop romcall(h, 0, 0, 0, $FDDA)
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DA,$FD		; CW	64986
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$30			; DROP
;  809: end
	DB	$5A			; LEAVE
;  810: def prword_10(h)
C0050:					; prword_10()
					; h = 2
;  811:     cout('$')
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0016,>C0016	; CALL	C0016
;  812:     drop romcall(h >> 8, h, 0, 0, $F941)
	DB	$66,$02			; LLW	2
	DB	$2A,$08			; CB	8
	DB	$1C			; SHR
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$41,$F9		; CW	63809
	DB	$54,<C0006,>C0006	; CALL	C0006
	DB	$30			; DROP
;  813: end
	DB	$5A			; LEAVE
;  814: def print_10(i)
C0052:					; print_10()
					; i = 2
;  815:     byte numstr[7]
					; numstr = 4
;  816:     byte place, sign
					; place = 11
					; sign = 12
;  817: 
;  818:     place = 6
	JSR	INTERP
	DB	$58,$0D,$01		; ENTER	13,1
	DB	$2A,$06			; CB	6
	DB	$74,$0B			; SLB	11
;  819:     if i < 0
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0054,>C0054	; SKPFLS	C0054
;  820:         sign = 1
	DB	$2A,$01			; CB	1
	DB	$74,$0C			; SLB	12
;  821:         i    = -i
	DB	$66,$02			; LLW	2
	DB	$10			; NEG
	DB	$76,$02			; SLW	2
;  822:     else
	DB	$50,<C0055,>C0055	; SKIP	C0055
C0054:
;  823:         sign = 0
	DB	$00			; ZERO
	DB	$74,$0C			; SLB	12
;  824:     fin
C0055:
;  825:     while i >= 10
C0056:
	DB	$66,$02			; LLW	2
	DB	$2A,$0A			; CB	10
	DB	$48			; ISGE
	DB	$4C,<C0057,>C0057	; SKPFLS	C0057
;  826:         i =, numstr[place] = i % 10 + '0'
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
;  827:         place              = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  828:     loop
	DB	$50,<C0056,>C0056	; SKIP	C0056
C0057:
;  829:     numstr[place] = i + '0'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$2A,$30			; CB	48
	DB	$02			; ADD
	DB	$70			; SB
;  830:     place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  831:     if sign
	DB	$64,$0C			; LLB	12
	DB	$4C,<C0058,>C0058	; SKPFLS	C0058
;  832:         numstr[place] = '-'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$2D			; CB	45
	DB	$70			; SB
;  833:         place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  834:     fin
C0058:
C0059:
;  835:     numstr[place] = 6 - place
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$06			; CB	6
	DB	$64,$0B			; LLB	11
	DB	$04			; SUB
	DB	$70			; SB
;  836:     prstr(@numstr[place])
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$54,<C0020,>C0020	; CALL	C0020
;  837: end
	DB	$5A			; LEAVE
;  838: def nametostr_30(namestr, len, strptr)
C0060:					; nametostr_30()
					; namestr = 2
					; len = 4
					; strptr = 6
;  839:     ^strptr = len
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$06			; LLW	6
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  840:     memcpy(namestr, strptr + 1, len)
	DB	$66,$02			; LLW	2
	DB	$66,$06			; LLW	6
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$54,<C0014,>C0014	; CALL	C0014
;  841: end
	DB	$5A			; LEAVE
;  842: 
;  843: ;=====================================
;  844: ;
;  845: ;           PLASMA Compiler
;  846: ;
;  847: ;=====================================
;  848: 
;  849: ;
;  850: ; Error handler
;  851: ;
;  852: def parse_err_11(err)
C0062:					; parse_err_11()
					; err = 2
;  853:     word i
					; i = 4
;  854: 
;  855:     drop close_11(0)
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$00			; ZERO
	DB	$54,<C0034,>C0034	; CALL	C0034
	DB	$30			; DROP
;  856:     crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
;  857:     print_10(lineno)
	DB	$6A,<D0421,>D0421	; LAW	D0421
	DB	$54,<C0052,>C0052	; CALL	C0052
;  858:     cout(':')
	DB	$2A,$3A			; CB	58
	DB	$54,<C0016,>C0016	; CALL	C0016
;  859:     prstr(err)
	DB	$66,$02			; LLW	2
	DB	$54,<C0020,>C0020	; CALL	C0020
;  860:     crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
;  861:     prstr(instr)
	DB	$2C,$FF,$01		; CW	511
	DB	$54,<C0020,>C0020	; CALL	C0020
;  862:     crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
;  863:     for i = inbuff to tknptr - 1
	DB	$2C,$00,$02		; CW	512
C0065:
	DB	$6E,$04			; DLW	4
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$3A,<C0064,>C0064	; SKPGT	C0064
	DB	$0C			; INCR
;  864:         cout(' ')
	DB	$2A,$20			; CB	32
	DB	$54,<C0016,>C0016	; CALL	C0016
;  865:     next
	DB	$50,<C0065,>C0065	; SKIP	C0065
C0064:
	DB	$30			; DROP
;  866:     cout('^')
	DB	$2A,$5E			; CB	94
	DB	$54,<C0016,>C0016	; CALL	C0016
;  867:     cin()
	DB	$54,<C0018,>C0018	; CALL	C0018
;  868:     exit()
	DB	$54,<C0026,>C0026	; CALL	C0026
;  869:     return ERR_TKN
	DB	$00			; ZERO
	DB	$5A			; LEAVE
;  870: end
;  871: ;
;  872: ; Emit bytecode
;  873: ;
;  874: def ctag_new_01
C0066:					; ctag_new_01()
;  875:     if codetag >= ctag_max
	JSR	INTERP
	DB	$6A,<D0012,>D0012	; LAW	D0012
	DB	$2C,$00,$03		; CW	768
	DB	$48			; ISGE
	DB	$4C,<C0068,>C0068	; SKPFLS	C0068
;  876:         return parse_err_11(@ctag_full)
	DB	$26,<D0693,>D0693	; LA	D0693
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5C			; RET
;  877:     fin
C0068:
C0069:
;  878:     codetag = codetag + 1
	DB	$6A,<D0012,>D0012	; LAW	D0012
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0012,>D0012	; SAW	D0012
;  879:     ctag_value:[codetag] = 0
	DB	$2C,$00,$1A		; CW	6656
	DB	$6A,<D0012,>D0012	; LAW	D0012
	DB	$1E			; IDXW
	DB	$00			; ZERO
	DB	$72			; SW
;  880:     ctag_flags.[codetag] = 0
	DB	$2C,$00,$0D		; CW	3328
	DB	$6A,<D0012,>D0012	; LAW	D0012
	DB	$02			; IDXB
	DB	$00			; ZERO
	DB	$70			; SB
;  881:     return codetag ? is_ctag
	DB	$6A,<D0012,>D0012	; LAW	D0012
	DB	$2C,$00,$80		; CW	32768
	DB	$16			; IOR
	DB	$5C			; RET
;  882: end
;  883: defopt ctag_resolve_21(tag, addr)
C0070:					; ctag_resolve_21()
					; tag = 2
					; addr = 4
;  884:     word updtptr, nextptr
					; updtptr = 6
					; nextptr = 8
;  885: 
;  886:     tag = tag & mask_ctag
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
;  887:     if ctag_flags.[tag] & resolved
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
	JMP	C0072
:
;  888:         return parse_err_11(@dup_id)
	DEX
	LDA	#<D0431
	STA	ESTKL,X
	LDA	#>D0431
	STA	ESTKH,X
	JSR	C0062
	JMP	LEAVE
;  889:     fin
C0072:
C0073:
;  890:     updtptr = ctag_value:[tag]
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
;  891:     while updtptr
	INX
C0074:
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
	JMP	C0075
:
;  892:         ;
;  893:         ; Update list of addresses needing resolution
;  894:         ;
;  895:         nextptr  = *updtptr
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
;  896:         *updtptr = addr
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
;  897:         updtptr  = nextptr
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
;  898:     loop
	INX
	JMP	C0074
C0075:
;  899:     ctag_value:[tag] = addr
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
;  900:     ctag_flags.[tag] = ctag_flags.[tag] ? resolved
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
;  901:     return 0
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
;  902: end
;  903: defopt emit_byte_10(bval)
C0076:					; emit_byte_10()
					; bval = 2
;  904:     ^codeptr = bval
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
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
;  905:     codeptr  = codeptr + 1
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0014
	LDA	ESTKH,X
	STA	D0014+1
;  906: end
	INX
	JMP	LEAVE
;  907: defopt emit_word_10(wval)
C0078:					; emit_word_10()
					; wval = 2
;  908:     *codeptr = wval
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
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
;  909:     codeptr  = codeptr + 2
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0014
	LDA	ESTKH,X
	STA	D0014+1
;  910: end
	INX
	JMP	LEAVE
;  911: def emit_fill_10(size)
C0080:					; emit_fill_10()
					; size = 2
;  912:     memset(0, codeptr, size)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$00			; ZERO
	DB	$6A,<D0014,>D0014	; LAW	D0014
	DB	$66,$02			; LLW	2
	DB	$54,<C0012,>C0012	; CALL	C0012
;  913:     codeptr = codeptr + size
	DB	$6A,<D0014,>D0014	; LAW	D0014
	DB	$66,$02			; LLW	2
	DB	$02			; ADD
	DB	$7A,<D0014,>D0014	; SAW	D0014
;  914: end
	DB	$5A			; LEAVE
;  915: def emit_codetag_10(tag)
C0082:					; emit_codetag_10()
					; tag = 2
;  916:     drop ctag_resolve_21(tag, codeptr)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$6A,<D0014,>D0014	; LAW	D0014
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
;  917: end
	DB	$5A			; LEAVE
;  918: defopt emit_op_10(op)
C0084:					; emit_op_10()
					; op = 2
;  919:     lastop   = op
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
	STA	D0018
;  920:     ^codeptr = op
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
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
;  921:     codeptr  = codeptr + 1
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0014
	LDA	ESTKH,X
	STA	D0014+1
;  922: end
	INX
	JMP	LEAVE
;  923: def emit_tag_10(tag)
C0086:					; emit_tag_10()
					; tag = 2
;  924:     word updtptr
					; updtptr = 4
;  925: 
;  926:     if tag & is_ctag
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$80		; CW	32768
	DB	$14			; BAND
	DB	$4C,<C0088,>C0088	; SKPFLS	C0088
;  927:         tag = tag & mask_ctag
	DB	$66,$02			; LLW	2
	DB	$2C,$FF,$7F		; CW	32767
	DB	$14			; BAND
	DB	$76,$02			; SLW	2
;  928:         updtptr = ctag_value:[tag]
	DB	$2C,$00,$1A		; CW	6656
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$76,$04			; SLW	4
;  929:         if !(ctag_flags.[tag] & resolved)
	DB	$2C,$00,$0D		; CW	3328
	DB	$66,$02			; LLW	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0090,>C0090	; SKPFLS	C0090
;  930:             ;
;  931:             ; Add to list of tags needing resolution
;  932:             ;
;  933:             ctag_value:[tag] = codeptr
	DB	$2C,$00,$1A		; CW	6656
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$6A,<D0014,>D0014	; LAW	D0014
	DB	$72			; SW
;  934:         fin
C0090:
C0091:
;  935:         emit_word_10(updtptr)
	DB	$66,$04			; LLW	4
	DB	$54,<C0078,>C0078	; CALL	C0078
;  936:     else
	DB	$50,<C0089,>C0089	; SKIP	C0089
C0088:
;  937:         emit_word_10(tag + codebuff)
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$60		; CW	24576
	DB	$02			; ADD
	DB	$54,<C0078,>C0078	; CALL	C0078
;  938:     fin
C0089:
;  939: end
	DB	$5A			; LEAVE
;  940: def emit_iddata_30(value, size, namestr)
C0092:					; emit_iddata_30()
					; value = 2
					; size = 4
					; namestr = 6
;  941:     emit_fill_10(size)
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$04			; LLW	4
	DB	$54,<C0080,>C0080	; CALL	C0080
;  942: end
	DB	$5A			; LEAVE
;  943: def emit_data_41(vartype, consttype, constval, constsize)
C0094:					; emit_data_41()
					; vartype = 2
					; consttype = 4
					; constval = 6
					; constsize = 8
;  944:     byte i
					; i = 10
;  945:     word size, chrptr
					; size = 11
					; chrptr = 13
;  946: 
;  947:     if consttype == 0
	JSR	INTERP
	DB	$58,$0F,$04		; ENTER	15,4
	DB	$66,$04			; LLW	4
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0096,>C0096	; SKPFLS	C0096
;  948:         size = constsize
	DB	$66,$08			; LLW	8
	DB	$76,$0B			; SLW	11
;  949:         emit_fill_10(constsize)
	DB	$66,$08			; LLW	8
	DB	$54,<C0080,>C0080	; CALL	C0080
;  950:     elsif consttype == STR_TYPE
	DB	$50,<C0097,>C0097	; SKIP	C0097
C0096:
	DB	$66,$04			; LLW	4
	DB	$2A,$80			; CB	128
	DB	$40			; ISEQ
	DB	$4C,<C0098,>C0098	; SKPFLS	C0098
;  951:         size = constsize
	DB	$66,$08			; LLW	8
	DB	$76,$0B			; SLW	11
;  952:         chrptr = constval
	DB	$66,$06			; LLW	6
	DB	$76,$0D			; SLW	13
;  953:         constsize = constsize - 1
	DB	$66,$08			; LLW	8
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$08			; SLW	8
;  954:         emit_byte_10(constsize)
	DB	$66,$08			; LLW	8
	DB	$54,<C0076,>C0076	; CALL	C0076
;  955:         while constsize > 0
C0099:
	DB	$66,$08			; LLW	8
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0100,>C0100	; SKPFLS	C0100
;  956:             emit_byte_10(^chrptr)
	DB	$66,$0D			; LLW	13
	DB	$60			; LB
	DB	$54,<C0076,>C0076	; CALL	C0076
;  957:             chrptr    = chrptr + 1
	DB	$66,$0D			; LLW	13
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$76,$0D			; SLW	13
;  958:             constsize = constsize - 1
	DB	$66,$08			; LLW	8
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$08			; SLW	8
;  959:         loop
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0100:
;  960:     else
	DB	$50,<C0097,>C0097	; SKIP	C0097
C0098:
;  961:         if vartype == WORD_TYPE
	DB	$66,$02			; LLW	2
	DB	$2A,$04			; CB	4
	DB	$40			; ISEQ
	DB	$4C,<C0101,>C0101	; SKPFLS	C0101
;  962:             size = 2
	DB	$2A,$02			; CB	2
	DB	$76,$0B			; SLW	11
;  963:             emit_word_10(constval)
	DB	$66,$06			; LLW	6
	DB	$54,<C0078,>C0078	; CALL	C0078
;  964:         else
	DB	$50,<C0102,>C0102	; SKIP	C0102
C0101:
;  965:             size = 1
	DB	$2A,$01			; CB	1
	DB	$76,$0B			; SLW	11
;  966:             emit_byte_10(constval)
	DB	$66,$06			; LLW	6
	DB	$54,<C0076,>C0076	; CALL	C0076
;  967:         fin
C0102:
;  968:     fin
C0097:
;  969:     return size
	DB	$66,$0B			; LLW	11
	DB	$5A			; LEAVE
;  970: end
;  971: def emit_const_10(cval)
C0103:					; emit_const_10()
					; cval = 2
;  972:     if cval == 0
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0105,>C0105	; SKPFLS	C0105
;  973:         emit_op_10($00)
	DB	$00			; ZERO
	DB	$54,<C0084,>C0084	; CALL	C0084
;  974:     elsif cval > 0 and cval < 256
	DB	$50,<C0106,>C0106	; SKIP	C0106
C0105:
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$01		; CW	256
	DB	$46			; ISLT
	DB	$24			; LAND
	DB	$4C,<C0107,>C0107	; SKPFLS	C0107
;  975:         emit_op_10($2A)
	DB	$2A,$2A			; CB	42
	DB	$54,<C0084,>C0084	; CALL	C0084
;  976:         emit_byte_10(cval)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
;  977:     else
	DB	$50,<C0106,>C0106	; SKIP	C0106
C0107:
;  978:         emit_op_10($2C)
	DB	$2A,$2C			; CB	44
	DB	$54,<C0084,>C0084	; CALL	C0084
;  979:         emit_word_10(cval)
	DB	$66,$02			; LLW	2
	DB	$54,<C0078,>C0078	; CALL	C0078
;  980:     fin
C0106:
;  981: end
	DB	$5A			; LEAVE
;  982: def emit_lb
C0108:					; emit_lb()
;  983:     emit_op_10($60)
	JSR	INTERP
	DB	$2A,$60			; CB	96
	DB	$54,<C0084,>C0084	; CALL	C0084
;  984: end
	DB	$5C			; RET
;  985: def emit_lw
C0110:					; emit_lw()
;  986:     emit_op_10($62)
	JSR	INTERP
	DB	$2A,$62			; CB	98
	DB	$54,<C0084,>C0084	; CALL	C0084
;  987: end
	DB	$5C			; RET
;  988: def emit_llb_10(index)
C0112:					; emit_llb_10()
					; index = 2
;  989:     emit_op_10($64)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$64			; CB	100
	DB	$54,<C0084,>C0084	; CALL	C0084
;  990:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
;  991: end
	DB	$5A			; LEAVE
;  992: def emit_llw_10(index)
C0114:					; emit_llw_10()
					; index = 2
;  993:     emit_op_10($66)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$66			; CB	102
	DB	$54,<C0084,>C0084	; CALL	C0084
;  994:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
;  995: end
	DB	$5A			; LEAVE
;  996: def emit_lab_10(tag)
C0116:					; emit_lab_10()
					; tag = 2
;  997:     emit_op_10($68)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$68			; CB	104
	DB	$54,<C0084,>C0084	; CALL	C0084
;  998:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
;  999: end
	DB	$5A			; LEAVE
; 1000: def emit_law_10(tag)
C0118:					; emit_law_10()
					; tag = 2
; 1001:     emit_op_10($6A)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6A			; CB	106
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1002:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1003: end
	DB	$5A			; LEAVE
; 1004: def emit_sb
C0120:					; emit_sb()
; 1005:     emit_op_10($70)
	JSR	INTERP
	DB	$2A,$70			; CB	112
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1006: end
	DB	$5C			; RET
; 1007: def emit_sw
C0122:					; emit_sw()
; 1008:     emit_op_10($72)
	JSR	INTERP
	DB	$2A,$72			; CB	114
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1009: end
	DB	$5C			; RET
; 1010: def emit_slb_10(index)
C0124:					; emit_slb_10()
					; index = 2
; 1011:     emit_op_10($74)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$74			; CB	116
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1012:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1013: end
	DB	$5A			; LEAVE
; 1014: def emit_slw_10(index)
C0126:					; emit_slw_10()
					; index = 2
; 1015:     emit_op_10($76)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$76			; CB	118
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1016:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1017: end
	DB	$5A			; LEAVE
; 1018: def emit_dlb_10(index)
C0128:					; emit_dlb_10()
					; index = 2
; 1019:     emit_op_10($6C)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6C			; CB	108
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1020:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1021: end
	DB	$5A			; LEAVE
; 1022: def emit_dlw_10(index)
C0130:					; emit_dlw_10()
					; index = 2
; 1023:     emit_op_10($6E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6E			; CB	110
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1024:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1025: end
	DB	$5A			; LEAVE
; 1026: def emit_sab_10(tag)
C0132:					; emit_sab_10()
					; tag = 2
; 1027:     emit_op_10($78)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$78			; CB	120
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1028:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1029: end
	DB	$5A			; LEAVE
; 1030: def emit_saw_10(tag)
C0134:					; emit_saw_10()
					; tag = 2
; 1031:     emit_op_10($7A)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7A			; CB	122
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1032:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1033: end
	DB	$5A			; LEAVE
; 1034: def emit_dab_10(tag)
C0136:					; emit_dab_10()
					; tag = 2
; 1035:     emit_op_10($7C)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7C			; CB	124
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1036:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1037: end
	DB	$5A			; LEAVE
; 1038: def emit_daw_10(tag)
C0138:					; emit_daw_10()
					; tag = 2
; 1039:     emit_op_10($7E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7E			; CB	126
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1040:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1041: end
	DB	$5A			; LEAVE
; 1042: def emit_call_10(tag)
C0140:					; emit_call_10()
					; tag = 2
; 1043:     emit_op_10($54)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$54			; CB	84
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1044:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1045: end
	DB	$5A			; LEAVE
; 1046: def emit_ical
C0142:					; emit_ical()
; 1047:     emit_op_10($56)
	JSR	INTERP
	DB	$2A,$56			; CB	86
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1048: end
	DB	$5C			; RET
; 1049: def emit_push
C0144:					; emit_push()
; 1050:     emit_op_10($34)
	JSR	INTERP
	DB	$2A,$34			; CB	52
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1051: end
	DB	$5C			; RET
; 1052: def emit_pull
C0146:					; emit_pull()
; 1053:     ;
; 1054:     ; Skip if last op was push
; 1055:     ;
; 1056:     if lastop == $34
	JSR	INTERP
	DB	$68,<D0018,>D0018	; LAB	D0018
	DB	$2A,$34			; CB	52
	DB	$40			; ISEQ
	DB	$4C,<C0148,>C0148	; SKPFLS	C0148
; 1057:         codeptr = codeptr - 1
	DB	$6A,<D0014,>D0014	; LAW	D0014
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0014,>D0014	; SAW	D0014
; 1058:         lastop = $FF
	DB	$2A,$FF			; CB	255
	DB	$78,<D0018,>D0018	; SAB	D0018
; 1059:     else
	DB	$50,<C0149,>C0149	; SKIP	C0149
C0148:
; 1060:         emit_op_10($36)
	DB	$2A,$36			; CB	54
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1061:     fin
C0149:
; 1062: end
	DB	$5C			; RET
; 1063: def emit_localaddr_10(index)
C0150:					; emit_localaddr_10()
					; index = 2
; 1064:     emit_op_10($28)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$28			; CB	40
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1065:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1066: end
	DB	$5A			; LEAVE
; 1067: def emit_globaladdr_10(tag)
C0152:					; emit_globaladdr_10()
					; tag = 2
; 1068:     emit_op_10($26)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$26			; CB	38
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1069:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1070: end
	DB	$5A			; LEAVE
; 1071: def emit_indexbyte
C0154:					; emit_indexbyte()
; 1072:     emit_op_10($02)
	JSR	INTERP
	DB	$2A,$02			; CB	2
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1073: end
	DB	$5C			; RET
; 1074: def emit_indexword
C0156:					; emit_indexword()
; 1075:     emit_op_10($1E)
	JSR	INTERP
	DB	$2A,$1E			; CB	30
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1076: end
	DB	$5C			; RET
; 1077: defopt emit_unaryop_11(op)
C0158:					; emit_unaryop_11()
					; op = 2
; 1078:     when op
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
; 1079:         is NEG_TKN
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
:	JMP	C0161
:
; 1080:             emit_op_10($10)
	DEX
	LDA	#$10
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1081:         is COMP_TKN
	JMP	C0160
C0161:
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
:	JMP	C0162
:
; 1082:             emit_op_10($12)
	DEX
	LDA	#$12
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1083:         is LOGIC_NOT_TKN
	JMP	C0160
C0162:
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
:	JMP	C0163
:
; 1084:             emit_op_10($20)
	DEX
	LDA	#$20
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1085:         is INC_TKN
	JMP	C0160
C0163:
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
:	JMP	C0164
:
; 1086:             emit_op_10($0C)
	DEX
	LDA	#$0C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1087:         is DEC_TKN
	JMP	C0160
C0164:
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
:	JMP	C0165
:
; 1088:             emit_op_10($0E)
	DEX
	LDA	#$0E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1089:         is BPTR_TKN
	JMP	C0160
C0165:
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
:	JMP	C0166
:
; 1090:             emit_op_10($60)
	DEX
	LDA	#$60
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1091:         is WPTR_TKN
	JMP	C0160
C0166:
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
:	JMP	C0167
:
; 1092:             emit_op_10($62)
	DEX
	LDA	#$62
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1093:         otherwise
	JMP	C0160
C0167:
; 1094:             return FALSE
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 1095:     wend
C0160:
; 1096:     return TRUE
	LDA	#$FF
	STA	ESTKL,X
	STA	ESTKH,X
	JMP	LEAVE
; 1097: end
; 1098: defopt emit_binaryop_11(op)
C0169:					; emit_binaryop_11()
					; op = 2
; 1099:     when op
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
; 1100:         is MUL_TKN
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
:	JMP	C0172
:
; 1101:             ;
; 1102:             ; Replace MUL 2 with SHL 1
; 1103:             ;
; 1104:             if lastop == $2A and ^(codeptr - 1) == 2 ; CB 2
	DEX
	LDA	D0018
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
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
	JMP	C0173
:
; 1105:                 codeptr = codeptr - 1
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0014
	LDA	ESTKH,X
	STA	D0014+1
; 1106:                 emit_byte_10(1) ; CB 1
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0076
; 1107:                 emit_op_10($1A) ; SHL
	DEX
	LDA	#$1A
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0084
; 1108:             else
	JMP	C0174
C0173:
; 1109:                 emit_op_10($06)
	DEX
	LDA	#$06
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0084
; 1110:             fin
C0174:
; 1111:         is DIV_TKN
	JMP	C0171
C0172:
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
:	JMP	C0175
:
; 1112:             ;
; 1113:             ; Replace DIV 2 with SHR 1
; 1114:             ;
; 1115:             if lastop == $2A and ^(codeptr - 1) == 2 ; CB 2
	DEX
	LDA	D0018
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
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
	JMP	C0176
:
; 1116:                 codeptr = codeptr - 1
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0014
	LDA	ESTKH,X
	STA	D0014+1
; 1117:                 emit_byte_10(1) ; CB 1
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0076
; 1118:                 emit_op_10($1C) ; SHR
	DEX
	LDA	#$1C
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0084
; 1119:             else
	JMP	C0177
C0176:
; 1120:                 emit_op_10($08)
	DEX
	LDA	#$08
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0084
; 1121:             fin
C0177:
; 1122:         is MOD_TKN
	JMP	C0171
C0175:
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
:	JMP	C0178
:
; 1123:             emit_op_10($0A)
	DEX
	LDA	#$0A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1124:         is ADD_TKN
	JMP	C0171
C0178:
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
:	JMP	C0179
:
; 1125:             ;
; 1126:             ; Replace ADD 1 with INCR
; 1127:             ;
; 1128:             if lastop == $2A and ^(codeptr - 1) == 1 ; CB 1
	DEX
	LDA	D0018
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
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
	JMP	C0180
:
; 1129:                 codeptr = codeptr - 2
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0014
	LDA	ESTKH,X
	STA	D0014+1
; 1130:                 emit_op_10($0C) ; INC_OP
	LDA	#$0C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1131:             else
	JMP	C0181
C0180:
; 1132:                 emit_op_10($02)
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0084
; 1133:             fin
C0181:
; 1134:         is SUB_TKN
	JMP	C0171
C0179:
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
:	JMP	C0182
:
; 1135:             ;
; 1136:             ; Replace SUB 1 with DECR
; 1137:             ;
; 1138:             if lastop == $2A and ^(codeptr - 1)  == 1 ; CB 1
	DEX
	LDA	D0018
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
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
	JMP	C0183
:
; 1139:                 codeptr = codeptr - 2
	DEX
	LDA	D0014
	STA	ESTKL,X
	LDA	D0014+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0014
	LDA	ESTKH,X
	STA	D0014+1
; 1140:                 emit_op_10($0E) ; DEC_OP
	LDA	#$0E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1141:             else
	JMP	C0184
C0183:
; 1142:                 emit_op_10($04)
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0084
; 1143:             fin
C0184:
; 1144:         is SHL_TKN
	JMP	C0171
C0182:
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
:	JMP	C0185
:
; 1145:             emit_op_10($1A)
	DEX
	LDA	#$1A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1146:         is SHR_TKN
	JMP	C0171
C0185:
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
:	JMP	C0186
:
; 1147:             emit_op_10($1C)
	DEX
	LDA	#$1C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1148:         is AND_TKN
	JMP	C0171
C0186:
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
:	JMP	C0187
:
; 1149:             emit_op_10($14)
	DEX
	LDA	#$14
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1150:         is OR_TKN
	JMP	C0171
C0187:
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
:	JMP	C0188
:
; 1151:             emit_op_10($16)
	DEX
	LDA	#$16
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1152:         is EOR_TKN
	JMP	C0171
C0188:
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
:	JMP	C0189
:
; 1153:             emit_op_10($18)
	DEX
	LDA	#$18
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1154:         is EQ_TKN
	JMP	C0171
C0189:
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
:	JMP	C0190
:
; 1155:             emit_op_10($40)
	DEX
	LDA	#$40
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1156:         is NE_TKN
	JMP	C0171
C0190:
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
:	JMP	C0191
:
; 1157:             emit_op_10($42)
	DEX
	LDA	#$42
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1158:         is GE_TKN
	JMP	C0171
C0191:
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
:	JMP	C0192
:
; 1159:             emit_op_10($48)
	DEX
	LDA	#$48
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1160:         is LT_TKN
	JMP	C0171
C0192:
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
:	JMP	C0193
:
; 1161:             emit_op_10($46)
	DEX
	LDA	#$46
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1162:         is GT_TKN
	JMP	C0171
C0193:
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
:	JMP	C0194
:
; 1163:             emit_op_10($44)
	DEX
	LDA	#$44
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1164:         is LE_TKN
	JMP	C0171
C0194:
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
:	JMP	C0195
:
; 1165:             emit_op_10($4A)
	DEX
	LDA	#$4A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1166:         is LOGIC_OR_TKN
	JMP	C0171
C0195:
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
:	JMP	C0196
:
; 1167:             emit_op_10($22)
	DEX
	LDA	#$22
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1168:         is LOGIC_AND_TKN
	JMP	C0171
C0196:
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
:	JMP	C0197
:
; 1169:             emit_op_10($24)
	DEX
	LDA	#$24
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0084
; 1170:         is COMMA_TKN
	JMP	C0171
C0197:
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
:	JMP	C0198
:
; 1171:             ; Do nothing except move to next stanza in expression
; 1172:         otherwise
	JMP	C0171
C0198:
; 1173:             return FALSE
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 1174:     wend
C0171:
; 1175:     return TRUE
	LDA	#$FF
	STA	ESTKL,X
	STA	ESTKH,X
	JMP	LEAVE
; 1176: end
; 1177: def emit_brtru_10(tag)
C0200:					; emit_brtru_10()
					; tag = 2
; 1178:     emit_op_10($4E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$4E			; CB	78
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1179:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1180: end
	DB	$5A			; LEAVE
; 1181: def emit_brfls_10(tag)
C0202:					; emit_brfls_10()
					; tag = 2
; 1182:     emit_op_10($4C)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$4C			; CB	76
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1183:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1184: end
	DB	$5A			; LEAVE
; 1185: def emit_brgt_10(tag)
C0204:					; emit_brgt_10()
					; tag = 2
; 1186:     emit_op_10($3A)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$3A			; CB	58
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1187:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1188: end
	DB	$5A			; LEAVE
; 1189: def emit_brlt_10(tag)
C0206:					; emit_brlt_10()
					; tag = 2
; 1190:     emit_op_10($38)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$38			; CB	56
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1191:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1192: end
	DB	$5A			; LEAVE
; 1193: def emit_brne_10(tag)
C0208:					; emit_brne_10()
					; tag = 2
; 1194:     emit_op_10($3E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$3E			; CB	62
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1195:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1196: end
	DB	$5A			; LEAVE
; 1197: def emit_jump_10(tag)
C0210:					; emit_jump_10()
					; tag = 2
; 1198:     emit_op_10($50)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$50			; CB	80
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1199:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0086,>C0086	; CALL	C0086
; 1200: end
	DB	$5A			; LEAVE
; 1201: def emit_drop
C0212:					; emit_drop()
; 1202:     emit_op_10($30)
	JSR	INTERP
	DB	$2A,$30			; CB	48
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1203: end
	DB	$5C			; RET
; 1204: def emit_swap
C0214:					; emit_swap()
; 1205:     emit_op_10($2E)
	JSR	INTERP
	DB	$2A,$2E			; CB	46
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1206: end
	DB	$5C			; RET
; 1207: def emit_leave_10(framesize)
C0216:					; emit_leave_10()
					; framesize = 2
; 1208:     if framesize > 2
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$44			; ISGT
	DB	$4C,<C0218,>C0218	; SKPFLS	C0218
; 1209:         emit_op_10($5A)
	DB	$2A,$5A			; CB	90
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1210:     else
	DB	$50,<C0219,>C0219	; SKIP	C0219
C0218:
; 1211:         emit_op_10($5C)
	DB	$2A,$5C			; CB	92
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1212:     fin
C0219:
; 1213: end
	DB	$5A			; LEAVE
; 1214: def emit_enter_20(framesize, cparams)
C0220:					; emit_enter_20()
					; framesize = 2
					; cparams = 4
; 1215:     emit_byte_10(emit_enter_20.[0])
	JSR	INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$26,<C0220,>C0220	; LA	C0220
	DB	$00			; ZERO
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1216:     emit_byte_10(emit_enter_20.[1])
	DB	$26,<C0220,>C0220	; LA	C0220
	DB	$2A,$01			; CB	1
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1217:     emit_byte_10(emit_enter_20.[2])
	DB	$26,<C0220,>C0220	; LA	C0220
	DB	$2A,$02			; CB	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1218:     if framesize > 2
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$44			; ISGT
	DB	$4C,<C0222,>C0222	; SKPFLS	C0222
; 1219:         emit_op_10($58)
	DB	$2A,$58			; CB	88
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1220:         emit_byte_10(framesize)
	DB	$66,$02			; LLW	2
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1221:         emit_byte_10(cparams)
	DB	$66,$04			; LLW	4
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1222:     fin
C0222:
C0223:
; 1223: end
	DB	$5A			; LEAVE
; 1224: def emit_start
C0224:					; emit_start()
; 1225:     ;
; 1226:     ; Save address
; 1227:     ;
; 1228:     entrypoint = codeptr
	JSR	INTERP
	DB	$6A,<D0014,>D0014	; LAW	D0014
	DB	$7A,<D0016,>D0016	; SAW	D0016
; 1229:     emit_byte_10(emit_start.[0])
	DB	$26,<C0224,>C0224	; LA	C0224
	DB	$00			; ZERO
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1230:     emit_byte_10(emit_start.[1])
	DB	$26,<C0224,>C0224	; LA	C0224
	DB	$2A,$01			; CB	1
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1231:     emit_byte_10(emit_start.[2])
	DB	$26,<C0224,>C0224	; LA	C0224
	DB	$2A,$02			; CB	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0076,>C0076	; CALL	C0076
; 1232: end
	DB	$5C			; RET
; 1233: def emit_exit
C0226:					; emit_exit()
; 1234:     emit_op_10($00)
	JSR	INTERP
	DB	$00			; ZERO
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1235:     emit_op_10($5C)
	DB	$2A,$5C			; CB	92
	DB	$54,<C0084,>C0084	; CALL	C0084
; 1236: end
	DB	$5C			; RET
; 1237: ;
; 1238: ; Lexical anaylzer
; 1239: ;
; 1240: ;def isalpha_11(c)
; 1241: ;   if c >= 'A' and c <= 'Z'
; 1242: ;       return TRUE
; 1243: ;   elsif c >= 'a' and c <= 'z'
; 1244: ;       return TRUE
; 1245: ;   elsif c == '_'
; 1246: ;       return TRUE
; 1247: ;   fin
; 1248: ;   return FALSE
; 1249: ;end
; 1250: asm isalpha_11
C0228:					; isalpha_11()
; 1251:         LDY     #$00
        LDY     #$00
; 1252:         LDA     ESTKL,X
        LDA     ESTKL,X
; 1253:         CMP     #'A'
        CMP     #'A'
; 1254:         BCC     ISALRET
        BCC     ISALRET
; 1255:         CMP     #'Z'+1
        CMP     #'Z'+1
; 1256:         BCS     :+
        BCS     :+
; 1257:         DEY
        DEY
; 1258:         BNE     ISALRET
        BNE     ISALRET
; 1259: :       CMP     #'a'
:       CMP     #'a'
; 1260:         BCC     ISALRET
        BCC     ISALRET
; 1261:         CMP     #'z'+1
        CMP     #'z'+1
; 1262:         BCS     :+
        BCS     :+
; 1263:         DEY
        DEY
; 1264:         BNE     ISALRET
        BNE     ISALRET
; 1265: :       CMP     #'_'
:       CMP     #'_'
; 1266:         BNE     ISALRET
        BNE     ISALRET
; 1267:         DEY
        DEY
; 1268: ISALRET:
ISALRET:
; 1269:         STY     ESTKL,X
        STY     ESTKL,X
; 1270:         STY     ESTKH,X
        STY     ESTKH,X
; 1271:         RTS
        RTS
; 1272: end
	RTS
; 1273: ;def isnum_11(c)
; 1274: ;   if c >= '0' and c <= '9'
; 1275: ;       return TRUE
; 1276: ;   fin
; 1277: ;   return FALSE
; 1278: ;end
; 1279: asm isnum_11
C0230:					; isnum_11()
; 1280:         LDY     #$00
        LDY     #$00
; 1281:         LDA     ESTKL,X
        LDA     ESTKL,X
; 1282:         CMP     #'0'
        CMP     #'0'
; 1283:         BCC     :+
        BCC     :+
; 1284:         CMP     #'9'+1
        CMP     #'9'+1
; 1285:         BCS     :+
        BCS     :+
; 1286:         DEY
        DEY
; 1287: :       STY     ESTKL,X
:       STY     ESTKL,X
; 1288:         STY     ESTKH,X
        STY     ESTKH,X
; 1289:         RTS
        RTS
; 1290: end
	RTS
; 1291: ;def isalphanum_11(c)
; 1292: ;   if c >= 'A' and c <= 'Z'
; 1293: ;       return TRUE
; 1294: ;   elsif c >= '0' and c <= '9'
; 1295: ;       return TRUE
; 1296: ;   elsif c >= 'a' and c <= 'z'
; 1297: ;       return TRUE
; 1298: ;   elsif c == '_'
; 1299: ;       return TRUE
; 1300: ;   fin
; 1301: ;   return FALSE
; 1302: ;end
; 1303: asm isalphanum_11
C0232:					; isalphanum_11()
; 1304:         LDY     #$00
        LDY     #$00
; 1305:         LDA     ESTKL,X
        LDA     ESTKL,X
; 1306:         CMP     #'0'
        CMP     #'0'
; 1307:         BCC     ISANRET
        BCC     ISANRET
; 1308:         CMP     #'9'+1
        CMP     #'9'+1
; 1309:         BCS     :+
        BCS     :+
; 1310:         DEY
        DEY
; 1311:         BNE     ISANRET
        BNE     ISANRET
; 1312: :       CMP     #'A'
:       CMP     #'A'
; 1313:         BCC     ISANRET
        BCC     ISANRET
; 1314:         CMP     #'Z'+1
        CMP     #'Z'+1
; 1315:         BCS     :+
        BCS     :+
; 1316:         DEY
        DEY
; 1317:         BNE     ISANRET
        BNE     ISANRET
; 1318: :       CMP     #'a'
:       CMP     #'a'
; 1319:         BCC     :+
        BCC     :+
; 1320:         CMP     #'z'+1
        CMP     #'z'+1
; 1321:         BCS     ISANRET
        BCS     ISANRET
; 1322:         DEY
        DEY
; 1323:         BNE     ISANRET
        BNE     ISANRET
; 1324: :       CMP     #'_'
:       CMP     #'_'
; 1325:         BNE     ISANRET
        BNE     ISANRET
; 1326:         DEY
        DEY
; 1327: ISANRET:
ISANRET:
; 1328:         STY     ESTKL,X
        STY     ESTKL,X
; 1329:         STY     ESTKH,X
        STY     ESTKH,X
; 1330:         RTS
        RTS
; 1331: end
	RTS
; 1332: defopt keymatch_21(chrptr, len)
C0234:					; keymatch_21()
					; chrptr = 2
					; len = 4
; 1333:     byte i, keypos
					; i = 6
					; keypos = 7
; 1334: 
; 1335:     keypos = 0
	LDY	#8
	LDA	#2
	JSR	ENTER
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 1336:     while keywrds[keypos] < len
	INX
C0236:
	DEX
	LDA	#<D0148
	STA	ESTKL,X
	LDA	#>D0148
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
	JMP	C0237
:
; 1337:         keypos = keypos + keywrds[keypos] + 2
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0148
	STA	ESTKL,X
	LDA	#>D0148
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
; 1338:     loop
	INX
	JMP	C0236
C0237:
; 1339:     while keywrds[keypos] == len
C0238:
	DEX
	LDA	#<D0148
	STA	ESTKL,X
	LDA	#>D0148
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
	JMP	C0239
:
; 1340:         for i = 1 to len
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
C0241:
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
	JMP	C0240
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
; 1341:             if toupper_11((chrptr).[i - 1]) <> keywrds[keypos + i]
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
	JSR	C0024
	DEX
	LDA	#<D0148
	STA	ESTKL,X
	LDA	#>D0148
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
	JMP	C0242
:
; 1342:                 break
	JMP	C0240
; 1343:             fin
C0242:
C0243:
; 1344:         next
	JMP	C0241
C0240:
; 1345:         if i > len
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
	JMP	C0244
:
; 1346:             return keywrds[keypos + keywrds[keypos] + 1]
	DEX
	LDA	#<D0148
	STA	ESTKL,X
	LDA	#>D0148
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0148
	STA	ESTKL,X
	LDA	#>D0148
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
; 1347:         fin
C0244:
C0245:
; 1348:         keypos = keypos + keywrds[keypos] + 2
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0148
	STA	ESTKL,X
	LDA	#>D0148
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
; 1349:     loop
	INX
	JMP	C0238
C0239:
; 1350:     return ID_TKN
	DEX
	LDA	#$D6
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
; 1351: end
; 1352: defopt scan_01
C0246:					; scan_01()
; 1353:     ;
; 1354:     ; Scan for token based on first character
; 1355:     ;
; 1356:     while ^scanptr and ^scanptr <= ' '
C0248:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0249
:
; 1357:         scanptr = scanptr + 1
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1358:     loop
	INX
	JMP	C0248
C0249:
; 1359:     tknptr = scanptr
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDA	ESTKL,X
	STA	D0417
	LDA	ESTKH,X
	STA	D0417+1
; 1360:     if !^scanptr or ^scanptr == ';'
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	NOT
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0250
:
; 1361:         if token <> EOF_TKN
	DEX
	LDA	D0413
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
	JMP	C0252
:
; 1362:             token = EOL_TKN
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1363:         fin
	INX
C0252:
C0253:
; 1364:     elsif isalpha_11(^scanptr)
	JMP	C0251
C0250:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	C0228
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0254
:
; 1365:         ;
; 1366:         ; ID,    either variable name or reserved word
; 1367:         ;
; 1368:         repeat
C0256:
; 1369:             scanptr = scanptr + 1
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1370:         until !isalphanum_11(^scanptr)
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	JSR	LB
	JSR	C0232
	LDY	#$00
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0256
:
C0255:
; 1371:         tknlen = scanptr - tknptr;
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	D0417
	STA	ESTKL,X
	LDA	D0417+1
	STA	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0414
; 1372:         token = keymatch_21(tknptr, tknlen)
	LDA	D0417
	STA	ESTKL,X
	LDA	D0417+1
	STA	ESTKH,X
	DEX
	LDA	D0414
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0234
	LDA	ESTKL,X
	STA	D0413
; 1373:     elsif isnum_11(^scanptr)
	INX
	JMP	C0251
C0254:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	C0230
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0257
:
; 1374:         ;
; 1375:         ; Number constant
; 1376:         ;
; 1377:         token       = INT_TKN
	DEX
	LDA	#$C9
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1378:         constval = 0
	STY	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1379:         repeat
	INX
C0259:
; 1380:             constval = constval * 10 + ^scanptr - '0'
	DEX
	LDA	D0419
	STA	ESTKL,X
	LDA	D0419+1
	STA	ESTKH,X
	DEX
	LDA	#$0A
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	MUL
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$30
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1381:             scanptr  = scanptr + 1
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1382:         until !isnum_11(^scanptr)
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	JSR	LB
	JSR	C0230
	LDY	#$00
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0259
:
C0258:
; 1383:     elsif ^scanptr == '$'
	JMP	C0251
C0257:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0260
:
; 1384:         ;
; 1385:         ; Hexadecimal constant
; 1386:         ;
; 1387:         token    = INT_TKN;
	DEX
	LDA	#$C9
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1388:         constval = 0
	STY	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1389:         repeat
	INX
C0262:
; 1390:             scanptr = scanptr + 1
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1391:             if ^scanptr >= '0' and ^scanptr <= '9'
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	JSR	LB
	DEX
	LDA	#$30
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0263
:
; 1392:                 constval = (constval << 4) + ^scanptr - '0'
	DEX
	LDA	D0419
	STA	ESTKL,X
	LDA	D0419+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SHL
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$30
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1393:             elsif ^scanptr >= 'A' and ^scanptr <= 'F'
	INX
	JMP	C0264
C0263:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$41
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0265
:
; 1394:                 constval = (constval << 4) + ^scanptr - '7'; 'A'-10
	DEX
	LDA	D0419
	STA	ESTKL,X
	LDA	D0419+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SHL
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$37
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1395:             elsif ^scanptr >= 'a' and ^scanptr <= 'f'
	INX
	JMP	C0264
C0265:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$61
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0266
:
; 1396:                 constval = (constval << 4) + ^scanptr - 'W'; 'a'-10
	DEX
	LDA	D0419
	STA	ESTKL,X
	LDA	D0419+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SHL
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	JSR	LB
	JSR	ADD
	DEX
	LDA	#$57
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1397:             else
	INX
	JMP	C0264
C0266:
; 1398:                 break;
	JMP	C0261
; 1399:             fin
C0264:
; 1400:         until !^scanptr
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0262
:
C0261:
; 1401:     elsif ^scanptr == $27 ; '
	JMP	C0251
C0260:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0267
:
; 1402:         ;
; 1403:         ; Character constant
; 1404:         ;
; 1405:         token = CHR_TKN
	DEX
	LDA	#$C3
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1406:         if ^(scanptr + 1) <> $5C ; \
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0268
:
; 1407:             constval = ^(scanptr + 1)
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1408:             if ^(scanptr + 2) <> $27 ; '
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0270
:
; 1409:                 return parse_err_11(@bad_cnst)
	DEX
	LDA	#<D0474
	STA	ESTKL,X
	LDA	#>D0474
	STA	ESTKH,X
	JSR	C0062
	RTS
; 1410:             fin
C0270:
C0271:
; 1411:             scanptr = scanptr + 3
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1412:         else
	INX
	JMP	C0269
C0268:
; 1413:             when ^(scanptr + 2)
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
; 1414:                 is 'n'
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
:	JMP	C0273
:
; 1415:                     constval = $0D
	DEX
	LDA	#$0D
	STA	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1416:                 is 'r'
	INX
	JMP	C0272
C0273:
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
:	JMP	C0274
:
; 1417:                     constval = $0A
	DEX
	LDA	#$0A
	STA	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1418:                 is 't'
	INX
	JMP	C0272
C0274:
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
:	JMP	C0275
:
; 1419:                     constval = $09
	DEX
	LDA	#$09
	STA	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1420:                 otherwise
	INX
	JMP	C0272
C0275:
; 1421:                     constval = ^(scanptr + 2)
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1422:             wend
	INX
C0272:
; 1423:             if ^(scanptr + 3) <> $27 ; '
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0277
:
; 1424:                 return parse_err_11(@bad_cnst)
	DEX
	LDA	#<D0474
	STA	ESTKL,X
	LDA	#>D0474
	STA	ESTKH,X
	JSR	C0062
	RTS
; 1425:             fin
C0277:
C0278:
; 1426:             scanptr = scanptr + 4
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1427:         fin
	INX
C0269:
; 1428:     elsif ^scanptr == '"'
	JMP	C0251
C0267:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0279
:
; 1429:         ;
; 1430:         ; String constant
; 1431:         ;
; 1432:         token    = STR_TKN
	DEX
	LDA	#$D3
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1433:         scanptr  = scanptr + 1
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1434:         constval = scanptr
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDA	ESTKL,X
	STA	D0419
	LDA	ESTKH,X
	STA	D0419+1
; 1435:         while ^scanptr and ^scanptr <> '"'
	INX
C0280:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0281
:
; 1436:             scanptr = scanptr + 1
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1437:         loop
	INX
	JMP	C0280
C0281:
; 1438:         if !^scanptr
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0282
:
; 1439:             return parse_err_11(@bad_cnst)
	DEX
	LDA	#<D0474
	STA	ESTKL,X
	LDA	#>D0474
	STA	ESTKH,X
	JSR	C0062
	RTS
; 1440:         fin
C0282:
C0283:
; 1441:         scanptr = scanptr + 1
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1442:     else
	INX
	JMP	C0251
C0279:
; 1443:         ;
; 1444:         ; Potential two and three character tokens
; 1445:         ;
; 1446:         when ^scanptr
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
; 1447:             is '>'
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
:	JMP	C0285
:
; 1448:                 if ^(scanptr + 1) == '>'
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0286
:
; 1449:                     token   = SHR_TKN
	DEX
	LDA	#$D2
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1450:                     scanptr = scanptr + 2
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1451:                 elsif ^(scanptr + 1) == '='
	INX
	JMP	C0287
C0286:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0288
:
; 1452:                     token   = GE_TKN
	DEX
	LDA	#$C8
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1453:                     scanptr = scanptr + 2
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1454:                 else
	INX
	JMP	C0287
C0288:
; 1455:                     token   = GT_TKN
	DEX
	LDA	#$BE
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1456:                     scanptr = scanptr + 1
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1457:                 fin
	INX
C0287:
; 1458:             is '<'
	JMP	C0284
C0285:
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
:	JMP	C0289
:
; 1459:                 if ^(scanptr + 1) == '<'
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0290
:
; 1460:                     token   = SHL_TKN
	DEX
	LDA	#$CC
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1461:                     scanptr = scanptr + 2
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1462:                 elsif ^(scanptr + 1) == '='
	INX
	JMP	C0291
C0290:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0292
:
; 1463:                     token   = LE_TKN
	DEX
	LDA	#$C2
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1464:                     scanptr = scanptr + 2
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1465:                 elsif ^(scanptr + 1) == '>'
	INX
	JMP	C0291
C0292:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0293
:
; 1466:                     token   = NE_TKN
	DEX
	LDA	#$D5
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1467:                     scanptr = scanptr + 2
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1468:                 else
	INX
	JMP	C0291
C0293:
; 1469:                     token   = LT_TKN
	DEX
	LDA	#$BC
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1470:                     scanptr = scanptr + 1
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1471:                 fin
	INX
C0291:
; 1472:             is '='
	JMP	C0284
C0289:
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
:	JMP	C0294
:
; 1473:                 if ^(scanptr + 1) == '='
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0295
:
; 1474:                     token   = EQ_TKN
	DEX
	LDA	#$C5
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1475:                     scanptr = scanptr + 2;
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1476:                 elsif ^(scanptr + 1) == ','
	INX
	JMP	C0296
C0295:
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
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
	JMP	C0297
:
; 1477:                     token   = SETLIST_TKN
	DEX
	LDA	#$B9
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1478:                     scanptr = scanptr + 2;
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1479:                 else
	INX
	JMP	C0296
C0297:
; 1480:                     token   = SET_TKN;
	DEX
	LDA	#$BD
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0413
; 1481:                     scanptr = scanptr + 1
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1482:                 fin
	INX
C0296:
; 1483:             otherwise
	JMP	C0284
C0294:
; 1484:                 ;
; 1485:                 ; Simple single character tokens
; 1486:                 ;
; 1487:                 token   = ^scanptr ? $80
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	#$80
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	IOR
	LDA	ESTKL,X
	STA	D0413
; 1488:                 scanptr = scanptr + 1
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0415
	LDA	ESTKH,X
	STA	D0415+1
; 1489:         wend
	INX
C0284:
; 1490:     fin
	INX
C0251:
; 1491:     tknlen = scanptr - tknptr
	DEX
	LDA	D0415
	STA	ESTKL,X
	LDA	D0415+1
	STA	ESTKH,X
	DEX
	LDA	D0417
	STA	ESTKL,X
	LDA	D0417+1
	STA	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0414
; 1492:     return token
	LDA	D0413
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	RTS
; 1493: end
; 1494: def rewind_10(ptr)
C0299:					; rewind_10()
					; ptr = 2
; 1495:     scanptr = ptr
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$7A,<D0415,>D0415	; SAW	D0415
; 1496: end
	DB	$5A			; LEAVE
; 1497: ;
; 1498: ; Get next line of input
; 1499: ;
; 1500: def nextln_01
C0301:					; nextln_01()
; 1501:     byte i, chr
					; i = 2
					; chr = 3
; 1502: 
; 1503:     scanptr = inbuff
	JSR	INTERP
	DB	$58,$04,$00		; ENTER	4,0
	DB	$2C,$00,$02		; CW	512
	DB	$7A,<D0415,>D0415	; SAW	D0415
; 1504:     ^instr = read_31(inref, inbuff, $7F)
	DB	$2C,$FF,$01		; CW	511
	DB	$68,<D0000,>D0000	; LAB	D0000
	DB	$2C,$00,$02		; CW	512
	DB	$2A,$7F			; CB	127
	DB	$54,<C0036,>C0036	; CALL	C0036
	DB	$70			; SB
; 1505:     inbuff[^instr] = $00
	DB	$2C,$00,$02		; CW	512
	DB	$2C,$FF,$01		; CW	511
	DB	$60			; LB
	DB	$02			; IDXB
	DB	$00			; ZERO
	DB	$70			; SB
; 1506:     if ^instr
	DB	$2C,$FF,$01		; CW	511
	DB	$60			; LB
	DB	$4C,<C0303,>C0303	; SKPFLS	C0303
; 1507:         lineno = lineno + 1
	DB	$6A,<D0421,>D0421	; LAW	D0421
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0421,>D0421	; SAW	D0421
; 1508:         if !(lineno & $0F)
	DB	$6A,<D0421,>D0421	; LAW	D0421
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0305,>C0305	; SKPFLS	C0305
; 1509:             cout('.')
	DB	$2A,$2E			; CB	46
	DB	$54,<C0016,>C0016	; CALL	C0016
; 1510:         fin
C0305:
C0306:
; 1511: ;        cout('>')
; 1512: ;        prstr(instr)
; 1513: ;        crout
; 1514:         drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 1515:     else
	DB	$50,<C0304,>C0304	; SKIP	C0304
C0303:
; 1516:         ^instr  = 0
	DB	$2C,$FF,$01		; CW	511
	DB	$00			; ZERO
	DB	$70			; SB
; 1517:         ^inbuff = $00
	DB	$2C,$00,$02		; CW	512
	DB	$00			; ZERO
	DB	$70			; SB
; 1518:         token   = DONE_TKN
	DB	$2A,$98			; CB	152
	DB	$78,<D0413,>D0413	; SAB	D0413
; 1519:     fin
C0304:
; 1520:     return ^instr
	DB	$2C,$FF,$01		; CW	511
	DB	$60			; LB
	DB	$5A			; LEAVE
; 1521: end
; 1522: ;
; 1523: ; Alebraic op to stack op
; 1524: ;
; 1525: def push_op_21(op, prec)
C0307:					; push_op_21()
					; op = 2
					; prec = 4
; 1526:     opsp = opsp + 1
	JSR	INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0411,>D0411	; SAW	D0411
; 1527:     if opsp == 16
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$2A,$10			; CB	16
	DB	$40			; ISEQ
	DB	$4C,<C0309,>C0309	; SKPFLS	C0309
; 1528:         return parse_err_11(@estk_overflw)
	DB	$26,<D0574,>D0574	; LA	D0574
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1529:     fin
C0309:
C0310:
; 1530:     opstack[opsp]   = op
	DB	$26,<D0379,>D0379	; LA	D0379
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$70			; SB
; 1531:     precstack[opsp] = prec
	DB	$26,<D0395,>D0395	; LA	D0395
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$02			; IDXB
	DB	$66,$04			; LLW	4
	DB	$70			; SB
; 1532:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1533: end
; 1534: def pop_op_01
C0311:					; pop_op_01()
; 1535:     if opsp < 0
	JSR	INTERP
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0313,>C0313	; SKPFLS	C0313
; 1536:         return parse_err_11(@estk_underflw)
	DB	$26,<D0594,>D0594	; LA	D0594
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5C			; RET
; 1537:     fin
C0313:
C0314:
; 1538:     opsp = opsp - 1
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0411,>D0411	; SAW	D0411
; 1539:     return opstack[opsp + 1]
	DB	$26,<D0379,>D0379	; LA	D0379
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5C			; RET
; 1540: end
; 1541: def tos_op_01
C0315:					; tos_op_01()
; 1542:     if opsp < 0
	JSR	INTERP
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0317,>C0317	; SKPFLS	C0317
; 1543:         return 0
	DB	$00			; ZERO
	DB	$5C			; RET
; 1544:     fin
C0317:
C0318:
; 1545:     return opstack[opsp]
	DB	$26,<D0379,>D0379	; LA	D0379
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5C			; RET
; 1546: end
; 1547: def tos_op_prec_11(tos)
C0319:					; tos_op_prec_11()
					; tos = 2
; 1548:     if opsp <= tos
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$66,$02			; LLW	2
	DB	$4A			; ISLE
	DB	$4C,<C0321,>C0321	; SKPFLS	C0321
; 1549:         return 100
	DB	$2A,$64			; CB	100
	DB	$5A			; LEAVE
; 1550:     fin
C0321:
C0322:
; 1551:     return precstack[opsp]
	DB	$26,<D0395,>D0395	; LA	D0395
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5A			; LEAVE
; 1552: end
; 1553: ;
; 1554: ; Symbol table
; 1555: ;
; 1556: defopt idmatch_41(nameptr, len, idptr, idcnt)
C0323:					; idmatch_41()
					; nameptr = 2
					; len = 4
					; idptr = 6
					; idcnt = 8
; 1557:     byte i
					; i = 10
; 1558: 
; 1559:     while idcnt
	LDY	#11
	LDA	#4
	JSR	ENTER
C0325:
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
	JMP	C0326
:
; 1560:         if len == (idptr).idname
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
	JMP	C0327
:
; 1561:             for i = 1 to len
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
C0330:
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
	JMP	C0329
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
; 1562:                 if (nameptr).[i - 1] <> (idptr).idname.[i]
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
	JMP	C0331
:
; 1563:                     break
	JMP	C0329
; 1564:                 fin
C0331:
C0332:
; 1565:             next
	JMP	C0330
C0329:
; 1566:             if i > len
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
	JMP	C0333
:
; 1567:                 return idptr
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JMP	LEAVE
; 1568:             fin
C0333:
C0334:
; 1569:         fin
C0327:
C0328:
; 1570:         idptr = idptr + (idptr).idname + idrecsz
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
; 1571:         idcnt = idcnt - 1
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
; 1572:     loop
	INX
	JMP	C0325
C0326:
; 1573:     return 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 1574: end
; 1575: ;def dumpsym_20(idptr, idcnt)
; 1576: ;   while idcnt
; 1577: ;       prword_10((idptr):idval)
; 1578: ;       cout(' ')
; 1579: ;       prbyte_10((idptr).idtype)
; 1580: ;       cout(' ')
; 1581: ;       prstr(@(idptr).idname)
; 1582: ;       cout('=')
; 1583: ;       if (idptr).idtype & ADDR_TYPE
; 1584: ;            if (idptr):idval & is_ctag
; 1585: ;                prword_10(ctag_value:[(idptr):idval & mask_ctag])
; 1586: ;            else
; 1587: ;                prword_10((idptr):idval + codebuff)
; 1588: ;            fin
; 1589: ;        else
; 1590: ;            prword_10((idptr):idval)
; 1591: ;        fin
; 1592: ;       crout()
; 1593: ;       idptr = idptr + (idptr).idname + idrecsz
; 1594: ;       idcnt = idcnt - 1
; 1595: ;   loop
; 1596: ;end
; 1597: def id_lookup_21(nameptr, len)
C0335:					; id_lookup_21()
					; nameptr = 2
					; len = 4
; 1598:     word idptr
					; idptr = 6
; 1599: 
; 1600:     idptr = idmatch_41(nameptr, len, idlocal_tbl, locals)
	JSR	INTERP
	DB	$58,$08,$02		; ENTER	8,2
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$18		; CW	6144
	DB	$68,<D0007,>D0007	; LAB	D0007
	DB	$54,<C0323,>C0323	; CALL	C0323
	DB	$76,$06			; SLW	6
; 1601:     if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C0337,>C0337	; SKPFLS	C0337
; 1602:         return idptr
	DB	$66,$06			; LLW	6
	DB	$5A			; LEAVE
; 1603:     fin
C0337:
C0338:
; 1604:     idptr = idmatch_41(nameptr, len, idglobal_tbl, globals)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0001,>D0001	; LAW	D0001
	DB	$54,<C0323,>C0323	; CALL	C0323
	DB	$76,$06			; SLW	6
; 1605:     if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C0339,>C0339	; SKPFLS	C0339
; 1606:         return idptr
	DB	$66,$06			; LLW	6
	DB	$5A			; LEAVE
; 1607:     fin
C0339:
C0340:
; 1608:     return parse_err_11(@undecl_id)
	DB	$26,<D0452,>D0452	; LA	D0452
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1609: end
; 1610: def idglobal_lookup_21(nameptr, len)
C0341:					; idglobal_lookup_21()
					; nameptr = 2
					; len = 4
; 1611:     return idmatch_41(nameptr, len, idglobal_tbl, globals)
	JSR	INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0001,>D0001	; LAW	D0001
	DB	$54,<C0323,>C0323	; CALL	C0323
	DB	$5A			; LEAVE
; 1612: end
; 1613: def idlocal_add_41(namestr, len, type, size)
C0343:					; idlocal_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; size = 8
; 1614:     if idmatch_41(namestr, len, @idlocal_tbl, locals)
	JSR	INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$18		; CW	6144
	DB	$68,<D0007,>D0007	; LAB	D0007
	DB	$54,<C0323,>C0323	; CALL	C0323
	DB	$4C,<C0345,>C0345	; SKPFLS	C0345
; 1615:         return parse_err_11(@dup_id)
	DB	$26,<D0431,>D0431	; LA	D0431
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1616:     fin
C0345:
C0346:
; 1617:     (lastlocal):idval  = framesize
	DB	$6A,<D0010,>D0010	; LAW	D0010
	DB	$6A,<D0008,>D0008	; LAW	D0008
	DB	$72			; SW
; 1618:     (lastlocal).idtype = type ? LOCAL_TYPE
	DB	$6A,<D0010,>D0010	; LAW	D0010
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$2A,$10			; CB	16
	DB	$16			; IOR
	DB	$70			; SB
; 1619:     nametostr_30(namestr, len, lastlocal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0010,>D0010	; LAW	D0010
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0060,>C0060	; CALL	C0060
; 1620:     locals    = locals + 1
	DB	$68,<D0007,>D0007	; LAB	D0007
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0007,>D0007	; SAB	D0007
; 1621:     lastlocal = lastlocal + idrecsz + len
	DB	$6A,<D0010,>D0010	; LAW	D0010
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0010,>D0010	; SAW	D0010
; 1622:     if lastlocal > idlocal_tbl + idlocal_tblsz
	DB	$6A,<D0010,>D0010	; LAW	D0010
	DB	$2C,$00,$18		; CW	6144
	DB	$2C,$00,$02		; CW	512
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0347,>C0347	; SKPFLS	C0347
; 1623:         prstr(@local_sym_overflw)
	DB	$26,<D0665,>D0665	; LA	D0665
	DB	$54,<C0020,>C0020	; CALL	C0020
; 1624:         exit
	DB	$54,<C0026,>C0026	; CALL	C0026
; 1625:     fin
C0347:
C0348:
; 1626:     framesize = framesize + size
	DB	$6A,<D0008,>D0008	; LAW	D0008
	DB	$66,$08			; LLW	8
	DB	$02			; ADD
	DB	$7A,<D0008,>D0008	; SAW	D0008
; 1627:     if framesize > 255
	DB	$6A,<D0008,>D0008	; LAW	D0008
	DB	$2A,$FF			; CB	255
	DB	$44			; ISGT
	DB	$4C,<C0349,>C0349	; SKPFLS	C0349
; 1628:         prstr(@local_overflw)
	DB	$26,<D0615,>D0615	; LA	D0615
	DB	$54,<C0020,>C0020	; CALL	C0020
; 1629:         return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1630:     fin
C0349:
C0350:
; 1631:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 1632: end
; 1633: def iddata_add_41(namestr, len, type, size)
C0351:					; iddata_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; size = 8
; 1634:     if idmatch_41(namestr, len, idglobal_tbl, globals)
	JSR	INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0001,>D0001	; LAW	D0001
	DB	$54,<C0323,>C0323	; CALL	C0323
	DB	$4C,<C0353,>C0353	; SKPFLS	C0353
; 1635:         return parse_err_11(@dup_id)
	DB	$26,<D0431,>D0431	; LA	D0431
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1636:     fin
C0353:
C0354:
; 1637:     (lastglobal):idval  = datasize
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$6A,<D0003,>D0003	; LAW	D0003
	DB	$72			; SW
; 1638:     (lastglobal).idtype = type
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$70			; SB
; 1639:     nametostr_30(namestr, len, lastglobal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0060,>C0060	; CALL	C0060
; 1640:     emit_iddata_30(datasize, size, lastglobal + idname)
	DB	$6A,<D0003,>D0003	; LAW	D0003
	DB	$66,$08			; LLW	8
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0092,>C0092	; CALL	C0092
; 1641:     globals    = globals + 1
	DB	$6A,<D0001,>D0001	; LAW	D0001
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0001,>D0001	; SAW	D0001
; 1642:     lastglobal = lastglobal + idrecsz + len
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0005,>D0005	; SAW	D0005
; 1643:     if lastglobal > idglobal_tbl + idglobal_tblsz
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2C,$00,$10		; CW	4096
	DB	$2C,$00,$08		; CW	2048
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0355,>C0355	; SKPFLS	C0355
; 1644:         prstr(@global_sym_overflw)
	DB	$26,<D0636,>D0636	; LA	D0636
	DB	$54,<C0020,>C0020	; CALL	C0020
; 1645:         exit
	DB	$54,<C0026,>C0026	; CALL	C0026
; 1646:     fin
C0355:
C0356:
; 1647:     datasize = datasize + size
	DB	$6A,<D0003,>D0003	; LAW	D0003
	DB	$66,$08			; LLW	8
	DB	$02			; ADD
	DB	$7A,<D0003,>D0003	; SAW	D0003
; 1648:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 1649: end
; 1650: def iddata_size_30(type, varsize, initsize)
C0357:					; iddata_size_30()
					; type = 2
					; varsize = 4
					; initsize = 6
; 1651:     if varsize > initsize
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$04			; LLW	4
	DB	$66,$06			; LLW	6
	DB	$44			; ISGT
	DB	$4C,<C0359,>C0359	; SKPFLS	C0359
; 1652:         datasize = datasize + emit_data_41(0, 0, 0, varsize - initsize)
	DB	$6A,<D0003,>D0003	; LAW	D0003
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$66,$04			; LLW	4
	DB	$66,$06			; LLW	6
	DB	$04			; SUB
	DB	$54,<C0094,>C0094	; CALL	C0094
	DB	$02			; ADD
	DB	$7A,<D0003,>D0003	; SAW	D0003
; 1653:     else
	DB	$50,<C0360,>C0360	; SKIP	C0360
C0359:
; 1654:         datasize = datasize + initsize
	DB	$6A,<D0003,>D0003	; LAW	D0003
	DB	$66,$06			; LLW	6
	DB	$02			; ADD
	DB	$7A,<D0003,>D0003	; SAW	D0003
; 1655:     fin
C0360:
; 1656: ;   if datasize <> codeptr - codebuff
; 1657: ;       prstr(@emiterr)
; 1658: ;       keyin_01()
; 1659: ;   fin
; 1660: end
	DB	$5A			; LEAVE
; 1661: def idglobal_add_41(namestr, len, type, value)
C0361:					; idglobal_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; value = 8
; 1662:     if idmatch_41(namestr, len, idglobal_tbl, globals)
	JSR	INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0001,>D0001	; LAW	D0001
	DB	$54,<C0323,>C0323	; CALL	C0323
	DB	$4C,<C0363,>C0363	; SKPFLS	C0363
; 1663:         return parse_err_11(@dup_id)
	DB	$26,<D0431,>D0431	; LA	D0431
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1664:     fin
C0363:
C0364:
; 1665:     (lastglobal):idval  = value
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$66,$08			; LLW	8
	DB	$72			; SW
; 1666:     (lastglobal).idtype = type
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$70			; SB
; 1667:     nametostr_30(namestr, len, lastglobal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0060,>C0060	; CALL	C0060
; 1668:     globals    = globals + 1
	DB	$6A,<D0001,>D0001	; LAW	D0001
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0001,>D0001	; SAW	D0001
; 1669:     lastglobal = lastglobal + idrecsz + len
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0005,>D0005	; SAW	D0005
; 1670:     if lastglobal > idglobal_tbl + idglobal_tblsz
	DB	$6A,<D0005,>D0005	; LAW	D0005
	DB	$2C,$00,$10		; CW	4096
	DB	$2C,$00,$08		; CW	2048
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0365,>C0365	; SKPFLS	C0365
; 1671:         prstr(@global_sym_overflw)
	DB	$26,<D0636,>D0636	; LA	D0636
	DB	$54,<C0020,>C0020	; CALL	C0020
; 1672:         exit
	DB	$54,<C0026,>C0026	; CALL	C0026
; 1673:     fin
C0365:
C0366:
; 1674:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 1675: end
; 1676: def idfunc_add_31(namestr, len, tag)
C0367:					; idfunc_add_31()
					; namestr = 2
					; len = 4
					; tag = 6
; 1677:     return idglobal_add_41(namestr, len, FUNC_TYPE, tag)
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2A,$08			; CB	8
	DB	$66,$06			; LLW	6
	DB	$54,<C0361,>C0361	; CALL	C0361
	DB	$5A			; LEAVE
; 1678: end
; 1679: def idconst_add_31(namestr, len, value)
C0369:					; idconst_add_31()
					; namestr = 2
					; len = 4
					; value = 6
; 1680:     return idglobal_add_41(namestr, len, CONST_TYPE, value)
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2A,$01			; CB	1
	DB	$66,$06			; LLW	6
	DB	$54,<C0361,>C0361	; CALL	C0361
	DB	$5A			; LEAVE
; 1681: end
; 1682: def idglobal_init
C0371:					; idglobal_init()
; 1683:     word ctag
					; ctag = 2
; 1684: 
; 1685:     lineno       = 0
	JSR	INTERP
	DB	$58,$04,$00		; ENTER	4,0
	DB	$00			; ZERO
	DB	$7A,<D0421,>D0421	; SAW	D0421
; 1686:     codeptr      = codebuff
	DB	$2C,$00,$60		; CW	24576
	DB	$7A,<D0014,>D0014	; SAW	D0014
; 1687:     lastop       = $FF
	DB	$2A,$FF			; CB	255
	DB	$78,<D0018,>D0018	; SAB	D0018
; 1688:     entrypoint   = 0
	DB	$00			; ZERO
	DB	$7A,<D0016,>D0016	; SAW	D0016
; 1689:     datasize     = 0
	DB	$00			; ZERO
	DB	$7A,<D0003,>D0003	; SAW	D0003
; 1690:     globals      = 0
	DB	$00			; ZERO
	DB	$7A,<D0001,>D0001	; SAW	D0001
; 1691:     lastglobal   = idglobal_tbl
	DB	$2C,$00,$10		; CW	4096
	DB	$7A,<D0005,>D0005	; SAW	D0005
; 1692:     codetag      = -1
	DB	$2C,$FF,$FF		; CW	-1
	DB	$7A,<D0012,>D0012	; SAW	D0012
; 1693:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1694:     drop idfunc_add_31(@runtime0 + 1, runtime0, ctag)
	DB	$26,<D0849,>D0849	; LA	D0849
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0849,>D0849	; LAB	D0849
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1695:     drop idfunc_add_31(@RUNTIME0 + 1, RUNTIME0, ctag)
	DB	$26,<D0857,>D0857	; LA	D0857
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0857,>D0857	; LAB	D0857
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1696:     drop ctag_resolve_21(ctag, @romcall)
	DB	$66,$02			; LLW	2
	DB	$26,<C0006,>C0006	; LA	C0006
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1697:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1698:     drop idfunc_add_31(@runtime1 + 1, runtime1, ctag)
	DB	$26,<D0865,>D0865	; LA	D0865
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0865,>D0865	; LAB	D0865
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1699:     drop idfunc_add_31(@RUNTIME1 + 1, RUNTIME1, ctag)
	DB	$26,<D0873,>D0873	; LA	D0873
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0873,>D0873	; LAB	D0873
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1700:     drop ctag_resolve_21(ctag, @syscall)
	DB	$66,$02			; LLW	2
	DB	$26,<C0010,>C0010	; LA	C0010
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1701:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1702:     drop idfunc_add_31(@runtime2 + 1, runtime2, ctag)
	DB	$26,<D0881,>D0881	; LA	D0881
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0881,>D0881	; LAB	D0881
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1703:     drop idfunc_add_31(@RUNTIME2 + 1, RUNTIME2, ctag)
	DB	$26,<D0888,>D0888	; LA	D0888
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0888,>D0888	; LAB	D0888
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1704:     drop ctag_resolve_21(ctag, @memset)
	DB	$66,$02			; LLW	2
	DB	$26,<C0012,>C0012	; LA	C0012
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1705:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1706:     drop idfunc_add_31(@runtime3 + 1, runtime3, ctag)
	DB	$26,<D0895,>D0895	; LA	D0895
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0895,>D0895	; LAB	D0895
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1707:     drop idfunc_add_31(@RUNTIME3 + 1, RUNTIME3, ctag)
	DB	$26,<D0902,>D0902	; LA	D0902
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0902,>D0902	; LAB	D0902
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1708:     drop ctag_resolve_21(ctag, @memcpy)
	DB	$66,$02			; LLW	2
	DB	$26,<C0014,>C0014	; LA	C0014
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1709:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1710:     drop idfunc_add_31(@runtime4 + 1, runtime4, ctag)
	DB	$26,<D0909,>D0909	; LA	D0909
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0909,>D0909	; LAB	D0909
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1711:     drop idfunc_add_31(@RUNTIME4 + 1, RUNTIME4, ctag)
	DB	$26,<D0914,>D0914	; LA	D0914
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0914,>D0914	; LAB	D0914
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1712:     drop ctag_resolve_21(ctag, @cout)
	DB	$66,$02			; LLW	2
	DB	$26,<C0016,>C0016	; LA	C0016
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1713:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1714:     drop idfunc_add_31(@runtime5 + 1, runtime5, ctag)
	DB	$26,<D0919,>D0919	; LA	D0919
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0919,>D0919	; LAB	D0919
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1715:     drop idfunc_add_31(@RUNTIME5 + 1, RUNTIME5, ctag)
	DB	$26,<D0923,>D0923	; LA	D0923
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0923,>D0923	; LAB	D0923
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1716:     drop ctag_resolve_21(ctag, @cin)
	DB	$66,$02			; LLW	2
	DB	$26,<C0018,>C0018	; LA	C0018
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1717:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1718:     drop idfunc_add_31(@runtime6 + 1, runtime6, ctag)
	DB	$26,<D0927,>D0927	; LA	D0927
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0927,>D0927	; LAB	D0927
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1719:     drop idfunc_add_31(@RUNTIME6 + 1, RUNTIME6, ctag)
	DB	$26,<D0933,>D0933	; LA	D0933
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0933,>D0933	; LAB	D0933
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1720:     drop ctag_resolve_21(ctag, @prstr)
	DB	$66,$02			; LLW	2
	DB	$26,<C0020,>C0020	; LA	C0020
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1721:     ctag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$02			; SLW	2
; 1722:     drop idfunc_add_31(@runtime7 + 1, runtime7, ctag)
	DB	$26,<D0939,>D0939	; LA	D0939
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0939,>D0939	; LAB	D0939
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1723:     drop idfunc_add_31(@RUNTIME7 + 1, RUNTIME7, ctag)
	DB	$26,<D0945,>D0945	; LA	D0945
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0945,>D0945	; LAB	D0945
	DB	$66,$02			; LLW	2
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 1724:     drop ctag_resolve_21(ctag, @rdstr)
	DB	$66,$02			; LLW	2
	DB	$26,<C0022,>C0022	; LA	C0022
	DB	$54,<C0070,>C0070	; CALL	C0070
	DB	$30			; DROP
; 1725: end
	DB	$5A			; LEAVE
; 1726: def idlocal_init
C0373:					; idlocal_init()
; 1727:     locals    = 0
	JSR	INTERP
	DB	$00			; ZERO
	DB	$78,<D0007,>D0007	; SAB	D0007
; 1728:     framesize = 2
	DB	$2A,$02			; CB	2
	DB	$7A,<D0008,>D0008	; SAW	D0008
; 1729:     lastlocal = idlocal_tbl
	DB	$2C,$00,$18		; CW	6144
	DB	$7A,<D0010,>D0010	; SAW	D0010
; 1730: end
	DB	$5C			; RET
; 1731: ;
; 1732: ; Parser
; 1733: ;
; 1734: def parse_term_01
C0375:					; parse_term_01()
; 1735:     when scan_01()
	JSR	INTERP
	DB	$54,<C0246,>C0246	; CALL	C0246
; 1736:         is ID_TKN
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0378,>C0378	; SKPNE	C0378
; 1737:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1738:         is INT_TKN
	DB	$50,<C0377,>C0377	; SKIP	C0377
C0378:
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0379,>C0379	; SKPNE	C0379
; 1739:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1740:         is CHR_TKN
	DB	$50,<C0377,>C0377	; SKIP	C0377
C0379:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0380,>C0380	; SKPNE	C0380
; 1741:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1742:         is STR_TKN
	DB	$50,<C0377,>C0377	; SKIP	C0377
C0380:
	DB	$2A,$D3			; CB	211
	DB	$3E,<C0381,>C0381	; SKPNE	C0381
; 1743:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1744:         is OPEN_PAREN_TKN
	DB	$50,<C0377,>C0377	; SKIP	C0377
C0381:
	DB	$2A,$A8			; CB	168
	DB	$3E,<C0382,>C0382	; SKPNE	C0382
; 1745:             if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0383,>C0383	; SKPFLS	C0383
; 1746:                 return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5C			; RET
; 1747:             fin
C0383:
C0384:
; 1748:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C0385,>C0385	; SKPFLS	C0385
; 1749:                 return parse_err_11(@no_close_paren)
	DB	$30			; DROP
	DB	$26,<D0713,>D0713	; LA	D0713
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5C			; RET
; 1750:             fin
C0385:
C0386:
; 1751:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1752:     wend
	DB	$50,<C0377,>C0377	; SKIP	C0377
C0382:
C0377:
	DB	$30			; DROP
; 1753:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 1754: end
; 1755: def parse_constval_21(valptr, sizeptr)
C0388:					; parse_constval_21()
					; valptr = 2
					; sizeptr = 4
; 1756:     byte mod, type
					; mod = 6
					; type = 7
; 1757:     word idptr
					; idptr = 8
; 1758: 
; 1759:     mod         = 0
	JSR	INTERP
	DB	$58,$0A,$02		; ENTER	10,2
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 1760:     type        = 0
	DB	$00			; ZERO
	DB	$74,$07			; SLB	7
; 1761:     *valptr = 0
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$72			; SW
; 1762:     while !parse_term_01()
C0390:
	DB	$54,<C0375,>C0375	; CALL	C0375
	DB	$20			; NOT
	DB	$4C,<C0391,>C0391	; SKPFLS	C0391
; 1763:         when token
	DB	$68,<D0413,>D0413	; LAB	D0413
; 1764:             is SUB_TKN
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0393,>C0393	; SKPNE	C0393
; 1765:                 mod = mod ? 1
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1766:             is COMP_TKN
	DB	$50,<C0392,>C0392	; SKIP	C0392
C0393:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0394,>C0394	; SKPNE	C0394
; 1767:                 mod = mod ? 2
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1768:             is LOGIC_NOT_TKN
	DB	$50,<C0392,>C0392	; SKIP	C0392
C0394:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0395,>C0395	; SKPNE	C0395
; 1769:                 mod = mod ? 4
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1770:             is AT_TKN
	DB	$50,<C0392,>C0392	; SKIP	C0392
C0395:
	DB	$2A,$C0			; CB	192
	DB	$3E,<C0396,>C0396	; SKPNE	C0396
; 1771:                 mod = mod ? 8
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1772:             otherwise
	DB	$50,<C0392,>C0392	; SKIP	C0392
C0396:
; 1773:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1774:         wend
C0392:
	DB	$30			; DROP
; 1775:     loop
	DB	$50,<C0390,>C0390	; SKIP	C0390
C0391:
; 1776:     when token
	DB	$68,<D0413,>D0413	; LAB	D0413
; 1777:         is STR_TKN
	DB	$2A,$D3			; CB	211
	DB	$3E,<C0399,>C0399	; SKPNE	C0399
; 1778:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0419,>D0419	; LAW	D0419
	DB	$72			; SW
; 1779:             ^sizeptr = tknlen - 1
	DB	$66,$04			; LLW	4
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
; 1780:             type         = STR_TYPE
	DB	$2A,$80			; CB	128
	DB	$74,$07			; SLB	7
; 1781:             if mod
	DB	$64,$06			; LLB	6
	DB	$4C,<C0400,>C0400	; SKPFLS	C0400
; 1782:                 return parse_err_11(@bad_op)
	DB	$30			; DROP
	DB	$26,<D0521,>D0521	; LA	D0521
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1783:             fin
C0400:
C0401:
; 1784:         is CHR_TKN
	DB	$50,<C0398,>C0398	; SKIP	C0398
C0399:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0402,>C0402	; SKPNE	C0402
; 1785:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0419,>D0419	; LAW	D0419
	DB	$72			; SW
; 1786:             ^sizeptr = 1
	DB	$66,$04			; LLW	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
; 1787:             type         = BYTE_TYPE
	DB	$2A,$02			; CB	2
	DB	$74,$07			; SLB	7
; 1788:         is INT_TKN
	DB	$50,<C0398,>C0398	; SKIP	C0398
C0402:
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0403,>C0403	; SKPNE	C0403
; 1789:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0419,>D0419	; LAW	D0419
	DB	$72			; SW
; 1790:             ^sizeptr = 2
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$70			; SB
; 1791:             type         = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$07			; SLB	7
; 1792:         is ID_TKN
	DB	$50,<C0398,>C0398	; SKIP	C0398
C0403:
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0404,>C0404	; SKPNE	C0404
; 1793:             ^sizeptr = 2
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$70			; SB
; 1794:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$54,<C0335,>C0335	; CALL	C0335
	DB	$76,$08			; SLW	8
; 1795:             if !idptr
	DB	$66,$08			; LLW	8
	DB	$20			; NOT
	DB	$4C,<C0405,>C0405	; SKPFLS	C0405
; 1796:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0474,>D0474	; LA	D0474
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1797:             fin
C0405:
C0406:
; 1798:             type    = (idptr).idtype
	DB	$66,$08			; LLW	8
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$07			; SLB	7
; 1799:             *valptr = (idptr):idval
	DB	$66,$02			; LLW	2
	DB	$66,$08			; LLW	8
	DB	$62			; LW
	DB	$72			; SW
; 1800:             if type & VAR_TYPE and !(mod & 8)
	DB	$64,$07			; LLB	7
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$24			; LAND
	DB	$4C,<C0407,>C0407	; SKPFLS	C0407
; 1801:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0474,>D0474	; LA	D0474
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1802:             fin
C0407:
C0408:
; 1803:         otherwise
	DB	$50,<C0398,>C0398	; SKIP	C0398
C0404:
; 1804:             return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0474,>D0474	; LA	D0474
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1805:     wend
C0398:
	DB	$30			; DROP
; 1806:     if mod & 1
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0410,>C0410	; SKPFLS	C0410
; 1807:         *valptr = -*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$10			; NEG
	DB	$72			; SW
; 1808:     fin
C0410:
C0411:
; 1809:     if mod & 2
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0412,>C0412	; SKPFLS	C0412
; 1810:         *valptr = #*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$12			; COMP
	DB	$72			; SW
; 1811:     fin
C0412:
C0413:
; 1812:     if mod & 4
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0414,>C0414	; SKPFLS	C0414
; 1813:         *valptr = !*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$20			; NOT
	DB	$72			; SW
; 1814:     fin
C0414:
C0415:
; 1815:     return type
	DB	$64,$07			; LLB	7
	DB	$5A			; LEAVE
; 1816: end
; 1817: def ispostop_01
C0416:					; ispostop_01()
; 1818:     when token
	JSR	INTERP
	DB	$68,<D0413,>D0413	; LAB	D0413
; 1819:         is OPEN_PAREN_TKN
	DB	$2A,$A8			; CB	168
	DB	$3E,<C0419,>C0419	; SKPNE	C0419
; 1820:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1821:         is OPEN_BRACKET_TKN
	DB	$50,<C0418,>C0418	; SKIP	C0418
C0419:
	DB	$2A,$DB			; CB	219
	DB	$3E,<C0420,>C0420	; SKPNE	C0420
; 1822:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1823:         is DOT_TKN
	DB	$50,<C0418,>C0418	; SKIP	C0418
C0420:
	DB	$2A,$AE			; CB	174
	DB	$3E,<C0421,>C0421	; SKPNE	C0421
; 1824:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1825:         is COLON_TKN
	DB	$50,<C0418,>C0418	; SKIP	C0418
C0421:
	DB	$2A,$BA			; CB	186
	DB	$3E,<C0422,>C0422	; SKPNE	C0422
; 1826:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 1827:     wend
	DB	$50,<C0418,>C0418	; SKIP	C0418
C0422:
C0418:
	DB	$30			; DROP
; 1828:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 1829: end
; 1830: def parse_value_11(rvalue)
C0424:					; parse_value_11()
					; rvalue = 2
; 1831:     byte cparams, deref, type, emit_val
					; cparams = 4
					; deref = 5
					; type = 6
					; emit_val = 7
; 1832:     word optos, idptr, value
					; optos = 8
					; idptr = 10
					; value = 12
; 1833:     byte elem_type, elem_size
					; elem_type = 14
					; elem_size = 15
; 1834:     word elem_offset
					; elem_offset = 16
; 1835: 
; 1836:     deref    = rvalue
	JSR	INTERP
	DB	$58,$12,$01		; ENTER	18,1
	DB	$66,$02			; LLW	2
	DB	$74,$05			; SLB	5
; 1837:     optos    = opsp
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$76,$08			; SLW	8
; 1838:     type     = 0
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 1839:     emit_val = 0
	DB	$00			; ZERO
	DB	$74,$07			; SLB	7
; 1840:     value    = 0
	DB	$00			; ZERO
	DB	$76,$0C			; SLW	12
; 1841: 
; 1842:     ;
; 1843:     ; Parse pre-ops
; 1844:     ;
; 1845:     while !parse_term_01()
C0426:
	DB	$54,<C0375,>C0375	; CALL	C0375
	DB	$20			; NOT
	DB	$4C,<C0427,>C0427	; SKPFLS	C0427
; 1846:         when token
	DB	$68,<D0413,>D0413	; LAB	D0413
; 1847:             is ADD_TKN
	DB	$2A,$AB			; CB	171
	DB	$3E,<C0429,>C0429	; SKPNE	C0429
; 1848:             is BPTR_TKN
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0429:
	DB	$2A,$DE			; CB	222
	DB	$3E,<C0430,>C0430	; SKPNE	C0430
; 1849:                 if deref
	DB	$64,$05			; LLB	5
	DB	$4C,<C0431,>C0431	; SKPFLS	C0431
; 1850:                     drop push_op_21(token, 0)
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$00			; ZERO
	DB	$54,<C0307,>C0307	; CALL	C0307
	DB	$30			; DROP
; 1851:                 else
	DB	$50,<C0432,>C0432	; SKIP	C0432
C0431:
; 1852:                     type = type ? BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1853:                     deref = deref + 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$05			; SLB	5
; 1854:                 fin
C0432:
; 1855:             is WPTR_TKN
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0430:
	DB	$2A,$AA			; CB	170
	DB	$3E,<C0433,>C0433	; SKPNE	C0433
; 1856:                 if deref
	DB	$64,$05			; LLB	5
	DB	$4C,<C0434,>C0434	; SKPFLS	C0434
; 1857:                     drop push_op_21(token, 0)
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$00			; ZERO
	DB	$54,<C0307,>C0307	; CALL	C0307
	DB	$30			; DROP
; 1858:                 else
	DB	$50,<C0435,>C0435	; SKIP	C0435
C0434:
; 1859:                     type = type ? WPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$40			; CB	64
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1860:                     deref = deref + 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$05			; SLB	5
; 1861:                 fin
C0435:
; 1862:             is AT_TKN
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0433:
	DB	$2A,$C0			; CB	192
	DB	$3E,<C0436,>C0436	; SKPNE	C0436
; 1863:                 deref = deref - 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$05			; SLB	5
; 1864:             is SUB_TKN
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0436:
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0437,>C0437	; SKPNE	C0437
; 1865:                 drop push_op_21(token, 0)
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$00			; ZERO
	DB	$54,<C0307,>C0307	; CALL	C0307
	DB	$30			; DROP
; 1866:             is COMP_TKN
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0437:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0438,>C0438	; SKPNE	C0438
; 1867:                 drop push_op_21(token, 0)
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$00			; ZERO
	DB	$54,<C0307,>C0307	; CALL	C0307
	DB	$30			; DROP
; 1868:             is LOGIC_NOT_TKN
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0438:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0439,>C0439	; SKPNE	C0439
; 1869:                 drop push_op_21(token, 0)
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$00			; ZERO
	DB	$54,<C0307,>C0307	; CALL	C0307
	DB	$30			; DROP
; 1870:             otherwise
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0439:
; 1871:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1872:         wend
C0428:
	DB	$30			; DROP
; 1873:     loop
	DB	$50,<C0426,>C0426	; SKIP	C0426
C0427:
; 1874:     ;
; 1875:     ; Determine terminal type
; 1876:     ;
; 1877:     when token
	DB	$68,<D0413,>D0413	; LAB	D0413
; 1878:         is INT_TKN
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0442,>C0442	; SKPNE	C0442
; 1879:             type  = type ? CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1880:             value = constval
	DB	$6A,<D0419,>D0419	; LAW	D0419
	DB	$76,$0C			; SLW	12
; 1881:         is CHR_TKN
	DB	$50,<C0441,>C0441	; SKIP	C0441
C0442:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0443,>C0443	; SKPNE	C0443
; 1882:             type  = type ? CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1883:             value = constval
	DB	$6A,<D0419,>D0419	; LAW	D0419
	DB	$76,$0C			; SLW	12
; 1884:         is ID_TKN
	DB	$50,<C0441,>C0441	; SKIP	C0441
C0443:
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0444,>C0444	; SKPNE	C0444
; 1885:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$54,<C0335,>C0335	; CALL	C0335
	DB	$76,$0A			; SLW	10
; 1886:             if !idptr
	DB	$66,$0A			; LLW	10
	DB	$20			; NOT
	DB	$4C,<C0445,>C0445	; SKPFLS	C0445
; 1887:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1888:             fin
C0445:
C0446:
; 1889:             if !(idptr).idtype
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$20			; NOT
	DB	$4C,<C0447,>C0447	; SKPFLS	C0447
; 1890:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1891:             fin
C0447:
C0448:
; 1892:             type  = type ? (idptr).idtype
	DB	$64,$06			; LLB	6
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1893:             value = (idptr):idval
	DB	$66,$0A			; LLW	10
	DB	$62			; LW
	DB	$76,$0C			; SLW	12
; 1894:         is CLOSE_PAREN_TKN
	DB	$50,<C0441,>C0441	; SKIP	C0441
C0444:
	DB	$2A,$A9			; CB	169
	DB	$3E,<C0449,>C0449	; SKPNE	C0449
; 1895:             type     = type ? WORD_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 1896:             emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1897:         otherwise
	DB	$50,<C0441,>C0441	; SKIP	C0441
C0449:
; 1898:             return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1899:     wend
C0441:
	DB	$30			; DROP
; 1900:     ;
; 1901:     ; Constant optimizations
; 1902:     ;
; 1903:     if type & CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0451,>C0451	; SKPFLS	C0451
; 1904:         cparams = TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$74,$04			; SLB	4
; 1905:         while optos < opsp and cparams
C0453:
	DB	$66,$08			; LLW	8
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$46			; ISLT
	DB	$64,$04			; LLB	4
	DB	$24			; LAND
	DB	$4C,<C0454,>C0454	; SKPFLS	C0454
; 1906:             when tos_op_01()
	DB	$54,<C0315,>C0315	; CALL	C0315
; 1907:                 is NEG_TKN
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0456,>C0456	; SKPNE	C0456
; 1908:                     drop pop_op_01()
	DB	$54,<C0311,>C0311	; CALL	C0311
	DB	$30			; DROP
; 1909:                     value = -value
	DB	$66,$0C			; LLW	12
	DB	$10			; NEG
	DB	$76,$0C			; SLW	12
; 1910:                 is COMP_TKN
	DB	$50,<C0455,>C0455	; SKIP	C0455
C0456:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0457,>C0457	; SKPNE	C0457
; 1911:                     drop pop_op_01()
	DB	$54,<C0311,>C0311	; CALL	C0311
	DB	$30			; DROP
; 1912:                     value = #value
	DB	$66,$0C			; LLW	12
	DB	$12			; COMP
	DB	$76,$0C			; SLW	12
; 1913:                 is LOGIC_NOT_TKN
	DB	$50,<C0455,>C0455	; SKIP	C0455
C0457:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0458,>C0458	; SKPNE	C0458
; 1914:                     drop pop_op_01()
	DB	$54,<C0311,>C0311	; CALL	C0311
	DB	$30			; DROP
; 1915:                     value = !value
	DB	$66,$0C			; LLW	12
	DB	$20			; NOT
	DB	$76,$0C			; SLW	12
; 1916:                 otherwise
	DB	$50,<C0455,>C0455	; SKIP	C0455
C0458:
; 1917:                     cparams = FALSE
	DB	$00			; ZERO
	DB	$74,$04			; SLB	4
; 1918:             wend
C0455:
	DB	$30			; DROP
; 1919:         loop
	DB	$50,<C0453,>C0453	; SKIP	C0453
C0454:
; 1920:     fin
C0451:
C0452:
; 1921:     ;
; 1922:     ; Parse post-ops
; 1923:     ;
; 1924:     drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 1925:     while ispostop_01()
C0460:
	DB	$54,<C0416,>C0416	; CALL	C0416
	DB	$4C,<C0461,>C0461	; SKPFLS	C0461
; 1926:         if token == OPEN_BRACKET_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C0462,>C0462	; SKPFLS	C0462
; 1927:             ;
; 1928:             ; Array
; 1929:             ;
; 1930:             if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0464,>C0464	; SKPFLS	C0464
; 1931:                 if type & ADDR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C0466,>C0466	; SKPFLS	C0466
; 1932:                     if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0468,>C0468	; SKPFLS	C0468
; 1933:                         emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0150,>C0150	; CALL	C0150
; 1934:                     else
	DB	$50,<C0469,>C0469	; SKIP	C0469
C0468:
; 1935:                         emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 1936:                     fin
C0469:
; 1937:                 elsif type & CONST_TYPE
	DB	$50,<C0467,>C0467	; SKIP	C0467
C0466:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0470,>C0470	; SKPFLS	C0470
; 1938:                     emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0103,>C0103	; CALL	C0103
; 1939:                 fin
C0470:
C0467:
; 1940:                 emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1941:             fin ; !emit_val
C0464:
C0465:
; 1942:             if type & PTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$4C,<C0471,>C0471	; SKPFLS	C0471
; 1943:                 emit_lw()
	DB	$54,<C0110,>C0110	; CALL	C0110
; 1944:             fin
C0471:
C0472:
; 1945:             if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0473,>C0473	; SKPFLS	C0473
; 1946:                 return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1947:             fin
C0473:
C0474:
; 1948:             if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C0475,>C0475	; SKPFLS	C0475
; 1949:                 return parse_err_11(@no_close_bracket)
	DB	$26,<D0735,>D0735	; LA	D0735
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 1950:             fin
C0475:
C0476:
; 1951:             if type & WORD_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0477,>C0477	; SKPFLS	C0477
; 1952:                 type = WPTR_TYPE
	DB	$2A,$40			; CB	64
	DB	$74,$06			; SLB	6
; 1953:                 emit_indexword()
	DB	$54,<C0156,>C0156	; CALL	C0156
; 1954:             else
	DB	$50,<C0478,>C0478	; SKIP	C0478
C0477:
; 1955:                 type = BPTR_TYPE
	DB	$2A,$20			; CB	32
	DB	$74,$06			; SLB	6
; 1956:                 emit_indexbyte()
	DB	$54,<C0154,>C0154	; CALL	C0154
; 1957:             fin
C0478:
; 1958:             drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 1959:         elsif token == DOT_TKN or token == COLON_TKN
	DB	$50,<C0463,>C0463	; SKIP	C0463
C0462:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$AE			; CB	174
	DB	$40			; ISEQ
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$BA			; CB	186
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0479,>C0479	; SKPFLS	C0479
; 1960:             ;
; 1961:             ; Dot and Colon
; 1962:             ;
; 1963:             if token == DOT_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$AE			; CB	174
	DB	$40			; ISEQ
	DB	$4C,<C0480,>C0480	; SKPFLS	C0480
; 1964:                 elem_type = BPTR_TYPE
	DB	$2A,$20			; CB	32
	DB	$74,$0E			; SLB	14
; 1965:             else
	DB	$50,<C0481,>C0481	; SKIP	C0481
C0480:
; 1966:                 elem_type = WPTR_TYPE
	DB	$2A,$40			; CB	64
	DB	$74,$0E			; SLB	14
; 1967:             fin
C0481:
; 1968:             if parse_constval_21(@elem_offset, @elem_size)
	DB	$28,$10			; LLA	16
	DB	$28,$0F			; LLA	15
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$4C,<C0482,>C0482	; SKPFLS	C0482
; 1969:                 ;
; 1970:                 ; Constant structure offset
; 1971:                 ;
; 1972:                 if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0484,>C0484	; SKPFLS	C0484
; 1973:                     if type & VAR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0486,>C0486	; SKPFLS	C0486
; 1974:                         if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0488,>C0488	; SKPFLS	C0488
; 1975:                             emit_localaddr_10(value + elem_offset)
	DB	$66,$0C			; LLW	12
	DB	$66,$10			; LLW	16
	DB	$02			; ADD
	DB	$54,<C0150,>C0150	; CALL	C0150
; 1976:                         else
	DB	$50,<C0489,>C0489	; SKIP	C0489
C0488:
; 1977:                             ; emit_globaladdr_10(value + elem_offset)
; 1978:                             emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 1979:                             emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0103,>C0103	; CALL	C0103
; 1980:                             drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0169,>C0169	; CALL	C0169
	DB	$30			; DROP
; 1981:                         fin
C0489:
; 1982:                     elsif type & CONST_TYPE
	DB	$50,<C0487,>C0487	; SKIP	C0487
C0486:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0490,>C0490	; SKPFLS	C0490
; 1983:                         value = value + elem_offset
	DB	$66,$0C			; LLW	12
	DB	$66,$10			; LLW	16
	DB	$02			; ADD
	DB	$76,$0C			; SLW	12
; 1984:                         emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0103,>C0103	; CALL	C0103
; 1985:                     else ; FUNC_TYPE
	DB	$50,<C0487,>C0487	; SKIP	C0487
C0490:
; 1986:                         emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 1987:                         emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0103,>C0103	; CALL	C0103
; 1988:                         drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0169,>C0169	; CALL	C0169
	DB	$30			; DROP
; 1989:                     fin
C0487:
; 1990:                     emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 1991:                 else
	DB	$50,<C0485,>C0485	; SKIP	C0485
C0484:
; 1992:                     if elem_offset <> 0
	DB	$66,$10			; LLW	16
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0491,>C0491	; SKPFLS	C0491
; 1993:                         emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0103,>C0103	; CALL	C0103
; 1994:                         drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0169,>C0169	; CALL	C0169
	DB	$30			; DROP
; 1995:                     fin
C0491:
C0492:
; 1996:                 fin ; !emit_val
C0485:
; 1997:                 drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 1998:             elsif token == OPEN_BRACKET_TKN
	DB	$50,<C0483,>C0483	; SKIP	C0483
C0482:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C0493,>C0493	; SKPFLS	C0493
; 1999:                 ;
; 2000:                 ; Array of arrays
; 2001:                 ;
; 2002:                 if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0494,>C0494	; SKPFLS	C0494
; 2003:                     if type & ADDR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C0496,>C0496	; SKPFLS	C0496
; 2004:                         if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0498,>C0498	; SKPFLS	C0498
; 2005:                             emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0150,>C0150	; CALL	C0150
; 2006:                         else
	DB	$50,<C0499,>C0499	; SKIP	C0499
C0498:
; 2007:                             emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 2008:                         fin
C0499:
; 2009:                     elsif type & CONST_TYPE
	DB	$50,<C0497,>C0497	; SKIP	C0497
C0496:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0500,>C0500	; SKPFLS	C0500
; 2010:                         emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0103,>C0103	; CALL	C0103
; 2011:                     fin
C0500:
C0497:
; 2012:                     emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 2013:                 fin ; !emit_val
C0494:
C0495:
; 2014:                 repeat
C0502:
; 2015:                         if emit_val > 1
	DB	$64,$07			; LLB	7
	DB	$2A,$01			; CB	1
	DB	$44			; ISGT
	DB	$4C,<C0503,>C0503	; SKPFLS	C0503
; 2016:                                 emit_indexword()
	DB	$54,<C0156,>C0156	; CALL	C0156
; 2017:                                 emit_lw()
	DB	$54,<C0110,>C0110	; CALL	C0110
; 2018:                         fin
C0503:
C0504:
; 2019:                         emit_val = emit_val + 1
	DB	$64,$07			; LLB	7
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$07			; SLB	7
; 2020:                         if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0505,>C0505	; SKPFLS	C0505
; 2021:                                 return parse_err_11(@bad_expr)
	DB	$26,<D0548,>D0548	; LA	D0548
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2022:                         fin
C0505:
C0506:
; 2023:                         if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C0507,>C0507	; SKPFLS	C0507
; 2024:                             return parse_err_11(@no_close_bracket)
	DB	$26,<D0735,>D0735	; LA	D0735
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2025:                         fin
C0507:
C0508:
; 2026:                 until scan_01() <> OPEN_BRACKET_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$DB			; CB	219
	DB	$42			; ISNE
	DB	$4C,<C0502,>C0502	; SKPFLS	C0502
C0501:
; 2027:                 if elem_type & WPTR_TYPE
	DB	$64,$0E			; LLB	14
	DB	$2A,$40			; CB	64
	DB	$14			; BAND
	DB	$4C,<C0509,>C0509	; SKPFLS	C0509
; 2028:                     emit_indexword()
	DB	$54,<C0156,>C0156	; CALL	C0156
; 2029:                 else
	DB	$50,<C0510,>C0510	; SKIP	C0510
C0509:
; 2030:                     emit_indexbyte()
	DB	$54,<C0154,>C0154	; CALL	C0154
; 2031:                 fin
C0510:
; 2032:             else
	DB	$50,<C0483,>C0483	; SKIP	C0483
C0493:
; 2033:                 return parse_err_11(@bad_offset)
	DB	$26,<D0487,>D0487	; LA	D0487
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2034:             fin
C0483:
; 2035:             type = elem_type
	DB	$64,$0E			; LLB	14
	DB	$74,$06			; SLB	6
; 2036:         elsif token == OPEN_PAREN_TKN
	DB	$50,<C0463,>C0463	; SKIP	C0463
C0479:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$A8			; CB	168
	DB	$40			; ISEQ
	DB	$4C,<C0511,>C0511	; SKPFLS	C0511
; 2037:             ;
; 2038:             ; Function call
; 2039:             ;
; 2040:             if !emit_val and type & VAR_TYPE
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0512,>C0512	; SKPFLS	C0512
; 2041:                 if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0514,>C0514	; SKPFLS	C0514
; 2042:                     emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0150,>C0150	; CALL	C0150
; 2043:                 else
	DB	$50,<C0515,>C0515	; SKIP	C0515
C0514:
; 2044:                     emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 2045:                 fin
C0515:
; 2046:             fin
C0512:
C0513:
; 2047:             if !(type & FUNC_CONST_TYPE)
	DB	$64,$06			; LLB	6
	DB	$2A,$09			; CB	9
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0516,>C0516	; SKPFLS	C0516
; 2048:                     emit_push()
	DB	$54,<C0144,>C0144	; CALL	C0144
; 2049:             fin
C0516:
C0517:
; 2050:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2051:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C0518,>C0518	; SKPFLS	C0518
; 2052:                 return parse_err_11(@no_close_paren)
	DB	$26,<D0713,>D0713	; LA	D0713
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2053:             fin
C0518:
C0519:
; 2054:             if type & FUNC_CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$09			; CB	9
	DB	$14			; BAND
	DB	$4C,<C0520,>C0520	; SKPFLS	C0520
; 2055:                 emit_call_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0140,>C0140	; CALL	C0140
; 2056:             else
	DB	$50,<C0521,>C0521	; SKIP	C0521
C0520:
; 2057:                 emit_pull()
	DB	$54,<C0146,>C0146	; CALL	C0146
; 2058:                 emit_ical()
	DB	$54,<C0142,>C0142	; CALL	C0142
; 2059:             fin
C0521:
; 2060:             emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 2061:             type = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$06			; SLB	6
; 2062:             drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2063:         fin
C0511:
C0463:
; 2064:     loop
	DB	$50,<C0460,>C0460	; SKIP	C0460
C0461:
; 2065:     if emit_val
	DB	$64,$07			; LLB	7
	DB	$4C,<C0522,>C0522	; SKPFLS	C0522
; 2066:         if rvalue
	DB	$66,$02			; LLW	2
	DB	$4C,<C0524,>C0524	; SKPFLS	C0524
; 2067:             if deref and type & PTR_TYPE
	DB	$64,$05			; LLB	5
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0526,>C0526	; SKPFLS	C0526
; 2068:                 if type & BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C0528,>C0528	; SKPFLS	C0528
; 2069:                     emit_lb()
	DB	$54,<C0108,>C0108	; CALL	C0108
; 2070:                 else
	DB	$50,<C0529,>C0529	; SKIP	C0529
C0528:
; 2071:                     emit_lw()
	DB	$54,<C0110,>C0110	; CALL	C0110
; 2072:                 fin
C0529:
; 2073:             fin
C0526:
C0527:
; 2074:         fin
C0524:
C0525:
; 2075:     else ; emit_val
	DB	$50,<C0523,>C0523	; SKIP	C0523
C0522:
; 2076:         if type & CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0530,>C0530	; SKPFLS	C0530
; 2077:             emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0103,>C0103	; CALL	C0103
; 2078:         elsif deref
	DB	$50,<C0531,>C0531	; SKIP	C0531
C0530:
	DB	$64,$05			; LLB	5
	DB	$4C,<C0532,>C0532	; SKPFLS	C0532
; 2079:             if type & FUNC_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0533,>C0533	; SKPFLS	C0533
; 2080:                 emit_call_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0140,>C0140	; CALL	C0140
; 2081:             elsif type & VAR_TYPE
	DB	$50,<C0534,>C0534	; SKIP	C0534
C0533:
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0535,>C0535	; SKPFLS	C0535
; 2082:                 if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0536,>C0536	; SKPFLS	C0536
; 2083:                     if type & BYTE_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0538,>C0538	; SKPFLS	C0538
; 2084:                         emit_llb_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0112,>C0112	; CALL	C0112
; 2085:                     else
	DB	$50,<C0539,>C0539	; SKIP	C0539
C0538:
; 2086:                         emit_llw_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0114,>C0114	; CALL	C0114
; 2087:                     fin
C0539:
; 2088:                 else
	DB	$50,<C0537,>C0537	; SKIP	C0537
C0536:
; 2089:                     if type & BYTE_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0540,>C0540	; SKPFLS	C0540
; 2090:                         emit_lab_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0116,>C0116	; CALL	C0116
; 2091:                     else
	DB	$50,<C0541,>C0541	; SKIP	C0541
C0540:
; 2092:                         emit_law_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0118,>C0118	; CALL	C0118
; 2093:                     fin
C0541:
; 2094:                 fin
C0537:
; 2095:             elsif type & PTR_TYPE
	DB	$50,<C0534,>C0534	; SKIP	C0534
C0535:
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$4C,<C0542,>C0542	; SKPFLS	C0542
; 2096:                 if type & BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C0543,>C0543	; SKPFLS	C0543
; 2097:                     emit_lb()
	DB	$54,<C0108,>C0108	; CALL	C0108
; 2098:                 else
	DB	$50,<C0544,>C0544	; SKIP	C0544
C0543:
; 2099:                     emit_lw()
	DB	$54,<C0110,>C0110	; CALL	C0110
; 2100:                 fin
C0544:
; 2101:             fin
C0542:
C0534:
; 2102:         else
	DB	$50,<C0531,>C0531	; SKIP	C0531
C0532:
; 2103:             if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0545,>C0545	; SKPFLS	C0545
; 2104:                 emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0150,>C0150	; CALL	C0150
; 2105:             else
	DB	$50,<C0546,>C0546	; SKIP	C0546
C0545:
; 2106:                 emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0152,>C0152	; CALL	C0152
; 2107:             fin
C0546:
; 2108:         fin
C0531:
; 2109:     fin ; emit_val
C0523:
; 2110:     while optos < opsp
C0547:
	DB	$66,$08			; LLW	8
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$46			; ISLT
	DB	$4C,<C0548,>C0548	; SKPFLS	C0548
; 2111:         if !emit_unaryop_11(pop_op_01())
	DB	$54,<C0311,>C0311	; CALL	C0311
	DB	$54,<C0158,>C0158	; CALL	C0158
	DB	$20			; NOT
	DB	$4C,<C0549,>C0549	; SKPFLS	C0549
; 2112:             return parse_err_11(@bad_op)
	DB	$26,<D0521,>D0521	; LA	D0521
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2113:         fin
C0549:
C0550:
; 2114:     loop
	DB	$50,<C0547,>C0547	; SKIP	C0547
C0548:
; 2115:     return type
	DB	$64,$06			; LLB	6
	DB	$5A			; LEAVE
; 2116: end
; 2117: def parse_constexpr_21(valptr, sizeptr)
C0551:					; parse_constexpr_21()
					; valptr = 2
					; sizeptr = 4
; 2118:     byte type, size1, size2
					; type = 6
					; size1 = 7
					; size2 = 8
; 2119:     word val1, val2
					; val1 = 9
					; val2 = 11
; 2120: 
; 2121:     type = parse_constval_21(@val1, @size1)
	JSR	INTERP
	DB	$58,$0D,$02		; ENTER	13,2
	DB	$28,$09			; LLA	9
	DB	$28,$07			; LLA	7
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2122:     if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0553,>C0553	; SKPFLS	C0553
; 2123:         return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2124:     fin
C0553:
C0554:
; 2125:     size2 = 0
	DB	$00			; ZERO
	DB	$74,$08			; SLB	8
; 2126:     when scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
; 2127:         is ADD_TKN
	DB	$2A,$AB			; CB	171
	DB	$3E,<C0556,>C0556	; SKPNE	C0556
; 2128:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2129:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0557,>C0557	; SKPFLS	C0557
; 2130:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2131:             fin
C0557:
C0558:
; 2132:             *valptr = val1 + val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$02			; ADD
	DB	$72			; SW
; 2133:         is SUB_TKN
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0556:
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0559,>C0559	; SKPNE	C0559
; 2134:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2135:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0560,>C0560	; SKPFLS	C0560
; 2136:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2137:             fin
C0560:
C0561:
; 2138:             *valptr = val1 - val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$04			; SUB
	DB	$72			; SW
; 2139:         is MUL_TKN
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0559:
	DB	$2A,$AA			; CB	170
	DB	$3E,<C0562,>C0562	; SKPNE	C0562
; 2140:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2141:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0563,>C0563	; SKPFLS	C0563
; 2142:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2143:             fin
C0563:
C0564:
; 2144:             *valptr = val1 * val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$06			; MUL
	DB	$72			; SW
; 2145:         is DIV_TKN
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0562:
	DB	$2A,$AF			; CB	175
	DB	$3E,<C0565,>C0565	; SKPNE	C0565
; 2146:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2147:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0566,>C0566	; SKPFLS	C0566
; 2148:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2149:             fin
C0566:
C0567:
; 2150:             *valptr = val1 + val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$02			; ADD
	DB	$72			; SW
; 2151:         is MOD_TKN
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0565:
	DB	$2A,$A5			; CB	165
	DB	$3E,<C0568,>C0568	; SKPNE	C0568
; 2152:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2153:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0569,>C0569	; SKPFLS	C0569
; 2154:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2155:             fin
C0569:
C0570:
; 2156:             *valptr = val1 % val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$0A			; DIV,MOD
	DB	$72			; SW
; 2157:             drop
	DB	$30			; DROP
; 2158:         is AND_TKN
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0568:
	DB	$2A,$A6			; CB	166
	DB	$3E,<C0571,>C0571	; SKPNE	C0571
; 2159:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2160:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0572,>C0572	; SKPFLS	C0572
; 2161:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2162:             fin
C0572:
C0573:
; 2163:             *valptr = val1 & val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$14			; BAND
	DB	$72			; SW
; 2164:         is OR_TKN
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0571:
	DB	$2A,$BF			; CB	191
	DB	$3E,<C0574,>C0574	; SKPNE	C0574
; 2165:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2166:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0575,>C0575	; SKPFLS	C0575
; 2167:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2168:             fin
C0575:
C0576:
; 2169:             *valptr = val1 ? val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$16			; IOR
	DB	$72			; SW
; 2170:         is EOR_TKN
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0574:
	DB	$2A,$DE			; CB	222
	DB	$3E,<C0577,>C0577	; SKPNE	C0577
; 2171:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$74,$06			; SLB	6
; 2172:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0578,>C0578	; SKPFLS	C0578
; 2173:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2174:             fin
C0578:
C0579:
; 2175:             *valptr = val1 ^ val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$18			; XOR
	DB	$72			; SW
; 2176:         otherwise
	DB	$50,<C0555,>C0555	; SKIP	C0555
C0577:
; 2177:             *valptr = val1
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$72			; SW
; 2178:     wend
C0555:
	DB	$30			; DROP
; 2179:     if size1 > size2
	DB	$64,$07			; LLB	7
	DB	$64,$08			; LLB	8
	DB	$44			; ISGT
	DB	$4C,<C0581,>C0581	; SKPFLS	C0581
; 2180:         ^sizeptr = size1
	DB	$66,$04			; LLW	4
	DB	$64,$07			; LLB	7
	DB	$70			; SB
; 2181:     else
	DB	$50,<C0582,>C0582	; SKIP	C0582
C0581:
; 2182:         ^sizeptr = size2
	DB	$66,$04			; LLW	4
	DB	$64,$08			; LLB	8
	DB	$70			; SB
; 2183:     fin
C0582:
; 2184:     return type
	DB	$64,$06			; LLB	6
	DB	$5A			; LEAVE
; 2185: end
; 2186: def parse_expr_01
C0000:					; parse_expr_01()
; 2187:     byte prevmatch, matchop, i
					; prevmatch = 2
					; matchop = 3
					; i = 4
; 2188:     word optos
					; optos = 5
; 2189: 
; 2190:     matchop = 0
	JSR	INTERP
	DB	$58,$07,$00		; ENTER	7,0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 2191:     optos   = opsp
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$76,$05			; SLW	5
; 2192:     repeat
C0585:
; 2193:         prevmatch = matchop
	DB	$64,$03			; LLB	3
	DB	$74,$02			; SLB	2
; 2194:         matchop     = 0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 2195:         if parse_value_11(1)
	DB	$2A,$01			; CB	1
	DB	$54,<C0424,>C0424	; CALL	C0424
	DB	$4C,<C0586,>C0586	; SKPFLS	C0586
; 2196:             matchop = 1
	DB	$2A,$01			; CB	1
	DB	$74,$03			; SLB	3
; 2197:             for i = 0 to bops_tblsz
	DB	$00			; ZERO
C0589:
	DB	$6C,$04			; DLB	4
	DB	$2A,$12			; CB	18
	DB	$3A,<C0588,>C0588	; SKPGT	C0588
	DB	$0C			; INCR
; 2198:                 if token == bops_tbl[i]
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$26,<D0341,>D0341	; LA	D0341
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$40			; ISEQ
	DB	$4C,<C0590,>C0590	; SKPFLS	C0590
; 2199:                     matchop = 2
	DB	$2A,$02			; CB	2
	DB	$74,$03			; SLB	3
; 2200:                     if bops_prec[i] >= tos_op_prec_11(optos)
	DB	$26,<D0360,>D0360	; LA	D0360
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$66,$05			; LLW	5
	DB	$54,<C0319,>C0319	; CALL	C0319
	DB	$48			; ISGE
	DB	$4C,<C0592,>C0592	; SKPFLS	C0592
; 2201:                         if !emit_binaryop_11(pop_op_01())
	DB	$54,<C0311,>C0311	; CALL	C0311
	DB	$54,<C0169,>C0169	; CALL	C0169
	DB	$20			; NOT
	DB	$4C,<C0594,>C0594	; SKPFLS	C0594
; 2202:                             return parse_err_11(@bad_op)
	DB	$30			; DROP
	DB	$26,<D0521,>D0521	; LA	D0521
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2203:                         fin
C0594:
C0595:
; 2204:                     fin
C0592:
C0593:
; 2205:                     drop push_op_21(token, bops_prec[i])
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$26,<D0360,>D0360	; LA	D0360
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0307,>C0307	; CALL	C0307
	DB	$30			; DROP
; 2206:                     break
	DB	$50,<C0588,>C0588	; SKIP	C0588
; 2207:                 fin
C0590:
C0591:
; 2208:             next
	DB	$50,<C0589,>C0589	; SKIP	C0589
C0588:
	DB	$30			; DROP
; 2209:         fin
C0586:
C0587:
; 2210:     until matchop <> 2
	DB	$64,$03			; LLB	3
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C0585,>C0585	; SKPFLS	C0585
C0584:
; 2211:     if matchop == 0 and prevmatch == 2
	DB	$64,$03			; LLB	3
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0596,>C0596	; SKPFLS	C0596
; 2212:         return parse_err_11(@missing_op)
	DB	$26,<D0759,>D0759	; LA	D0759
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2213:     fin
C0596:
C0597:
; 2214:     while optos < opsp
C0598:
	DB	$66,$05			; LLW	5
	DB	$6A,<D0411,>D0411	; LAW	D0411
	DB	$46			; ISLT
	DB	$4C,<C0599,>C0599	; SKPFLS	C0599
; 2215:         if !emit_binaryop_11(pop_op_01())
	DB	$54,<C0311,>C0311	; CALL	C0311
	DB	$54,<C0169,>C0169	; CALL	C0169
	DB	$20			; NOT
	DB	$4C,<C0600,>C0600	; SKPFLS	C0600
; 2216:             return parse_err_11(@bad_op)
	DB	$26,<D0521,>D0521	; LA	D0521
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2217:         fin
C0600:
C0601:
; 2218:     loop
	DB	$50,<C0598,>C0598	; SKIP	C0598
C0599:
; 2219:     return matchop or prevmatch
	DB	$64,$03			; LLB	3
	DB	$64,$02			; LLB	2
	DB	$22			; LOR
	DB	$5A			; LEAVE
; 2220: end
; 2221: def parse_setlist_21(addr, type)
C0602:					; parse_setlist_21()
					; addr = 2
					; type = 4
; 2222:     word nexttype, nextaddr, idptr, saveptr
					; nexttype = 6
					; nextaddr = 8
					; idptr = 10
					; saveptr = 12
; 2223: 
; 2224:     if !(type & VAR_TYPE)
	JSR	INTERP
	DB	$58,$0E,$02		; ENTER	14,2
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0604,>C0604	; SKPFLS	C0604
; 2225:         emit_push()
	DB	$54,<C0144,>C0144	; CALL	C0144
; 2226:     fin
C0604:
C0605:
; 2227:     nexttype = 0
	DB	$00			; ZERO
	DB	$76,$06			; SLW	6
; 2228:     nextaddr = 0
	DB	$00			; ZERO
	DB	$76,$08			; SLW	8
; 2229:     if scan_01() == ID_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C0606,>C0606	; SKPFLS	C0606
; 2230:         idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$54,<C0335,>C0335	; CALL	C0335
	DB	$76,$0A			; SLW	10
; 2231:         if !idptr
	DB	$66,$0A			; LLW	10
	DB	$20			; NOT
	DB	$4C,<C0608,>C0608	; SKPFLS	C0608
; 2232:             return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2233:         fin
C0608:
C0609:
; 2234:         nexttype = (idptr).idtype
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$76,$06			; SLW	6
; 2235:         if type & VAR_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0610,>C0610	; SKPFLS	C0610
; 2236:             nextaddr = (idptr):idval
	DB	$66,$0A			; LLW	10
	DB	$62			; LW
	DB	$76,$08			; SLW	8
; 2237:         fin
C0610:
C0611:
; 2238:     fin
C0606:
C0607:
; 2239:     saveptr = tknptr
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$76,$0C			; SLW	12
; 2240:     drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2241:     if nexttype & VAR_TYPE and token == SET_TKN
	DB	$66,$06			; LLW	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0612,>C0612	; SKPFLS	C0612
; 2242:         drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2243:         if type & LOCAL_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0614,>C0614	; SKPFLS	C0614
; 2244:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0616,>C0616	; SKPFLS	C0616
; 2245:                 emit_slb_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0124,>C0124	; CALL	C0124
; 2246:             else
	DB	$50,<C0617,>C0617	; SKIP	C0617
C0616:
; 2247:                 emit_slw_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0126,>C0126	; CALL	C0126
; 2248:             fin
C0617:
; 2249:         else
	DB	$50,<C0615,>C0615	; SKIP	C0615
C0614:
; 2250:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0618,>C0618	; SKPFLS	C0618
; 2251:                 emit_sab_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0132,>C0132	; CALL	C0132
; 2252:             else
	DB	$50,<C0619,>C0619	; SKIP	C0619
C0618:
; 2253:                 emit_saw_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0134,>C0134	; CALL	C0134
; 2254:             fin
C0619:
; 2255:         fin
C0615:
; 2256:     elsif nexttype & VAR_TYPE and token == SETLIST_TKN
	DB	$50,<C0613,>C0613	; SKIP	C0613
C0612:
	DB	$66,$06			; LLW	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0620,>C0620	; SKPFLS	C0620
; 2257:         if !parse_setlist_21(nextaddr, nexttype)
	DB	$66,$08			; LLW	8
	DB	$66,$06			; LLW	6
	DB	$54,<C0602,>C0602	; CALL	C0602
	DB	$20			; NOT
	DB	$4C,<C0621,>C0621	; SKPFLS	C0621
; 2258:             return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2259:         fin
C0621:
C0622:
; 2260:     else
	DB	$50,<C0613,>C0613	; SKIP	C0613
C0620:
; 2261:         tknptr = saveptr
	DB	$66,$0C			; LLW	12
	DB	$7A,<D0417,>D0417	; SAW	D0417
; 2262:         rewind_10(tknptr)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$54,<C0299,>C0299	; CALL	C0299
; 2263:         nexttype = parse_value_11(0)
	DB	$00			; ZERO
	DB	$54,<C0424,>C0424	; CALL	C0424
	DB	$76,$06			; SLW	6
; 2264:         if nexttype <> 0
	DB	$66,$06			; LLW	6
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0623,>C0623	; SKPFLS	C0623
; 2265:             if token == SET_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C0625,>C0625	; SKPFLS	C0625
; 2266:                 emit_push()
	DB	$54,<C0144,>C0144	; CALL	C0144
; 2267:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2268:                 emit_pull()
	DB	$54,<C0146,>C0146	; CALL	C0146
; 2269:                 emit_swap()
	DB	$54,<C0214,>C0214	; CALL	C0214
; 2270:                 if nexttype & (BYTE_TYPE ? BPTR_TYPE)
	DB	$66,$06			; LLW	6
	DB	$2A,$02			; CB	2
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$14			; BAND
	DB	$4C,<C0627,>C0627	; SKPFLS	C0627
; 2271:                     emit_sb()
	DB	$54,<C0120,>C0120	; CALL	C0120
; 2272:                 else
	DB	$50,<C0628,>C0628	; SKIP	C0628
C0627:
; 2273:                     emit_sw()
	DB	$54,<C0122,>C0122	; CALL	C0122
; 2274:                 fin
C0628:
; 2275:             fin
C0625:
C0626:
; 2276:         elsif token == SETLIST_TKN
	DB	$50,<C0624,>C0624	; SKIP	C0624
C0623:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$4C,<C0629,>C0629	; SKPFLS	C0629
; 2277:             if !parse_setlist_21(0, nexttype)
	DB	$00			; ZERO
	DB	$66,$06			; LLW	6
	DB	$54,<C0602,>C0602	; CALL	C0602
	DB	$20			; NOT
	DB	$4C,<C0630,>C0630	; SKPFLS	C0630
; 2278:                 return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2279:             fin
C0630:
C0631:
; 2280:         else
	DB	$50,<C0624,>C0624	; SKIP	C0624
C0629:
; 2281:             return parse_err_11(@bad_syntax)
	DB	$26,<D0563,>D0563	; LA	D0563
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2282:         fin
C0624:
; 2283:     fin
C0613:
; 2284:     if type & VAR_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0632,>C0632	; SKPFLS	C0632
; 2285:         if type & LOCAL_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0634,>C0634	; SKPFLS	C0634
; 2286:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0636,>C0636	; SKPFLS	C0636
; 2287:                 emit_slb_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0124,>C0124	; CALL	C0124
; 2288:             else
	DB	$50,<C0637,>C0637	; SKIP	C0637
C0636:
; 2289:                 emit_slw_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0126,>C0126	; CALL	C0126
; 2290:             fin
C0637:
; 2291:         else
	DB	$50,<C0635,>C0635	; SKIP	C0635
C0634:
; 2292:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0638,>C0638	; SKPFLS	C0638
; 2293:                 emit_sab_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0132,>C0132	; CALL	C0132
; 2294:             else
	DB	$50,<C0639,>C0639	; SKIP	C0639
C0638:
; 2295:                 emit_saw_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0134,>C0134	; CALL	C0134
; 2296:             fin
C0639:
; 2297:         fin
C0635:
; 2298:     else
	DB	$50,<C0633,>C0633	; SKIP	C0633
C0632:
; 2299:         emit_pull()
	DB	$54,<C0146,>C0146	; CALL	C0146
; 2300:         emit_swap()
	DB	$54,<C0214,>C0214	; CALL	C0214
; 2301:         if type & (BYTE_TYPE ? BPTR_TYPE)
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$14			; BAND
	DB	$4C,<C0640,>C0640	; SKPFLS	C0640
; 2302:             emit_sb()
	DB	$54,<C0120,>C0120	; CALL	C0120
; 2303:         else
	DB	$50,<C0641,>C0641	; SKIP	C0641
C0640:
; 2304:             emit_sw()
	DB	$54,<C0122,>C0122	; CALL	C0122
; 2305:         fin
C0641:
; 2306:     fin
C0633:
; 2307:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2308: end
; 2309: def parse_stmnt_01
C0642:					; parse_stmnt_01()
; 2310:     byte type, i
					; type = 2
					; i = 3
; 2311:     word tag_prevbrk, tag_else, tag_endif, tag_while, tag_wend
					; tag_prevbrk = 4
					; tag_else = 6
					; tag_endif = 8
					; tag_while = 10
					; tag_wend = 12
; 2312:     word tag_repeat, tag_for, tag_choice, idptr, saveptr, addr, stepdir
					; tag_repeat = 14
					; tag_for = 16
					; tag_choice = 18
					; idptr = 20
					; saveptr = 22
					; addr = 24
					; stepdir = 26
; 2313: 
; 2314:     if token <> END_TKN and token <> DONE_TKN
	JSR	INTERP
	DB	$58,$1C,$00		; ENTER	28,0
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$87			; CB	135
	DB	$42			; ISNE
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$98			; CB	152
	DB	$42			; ISNE
	DB	$24			; LAND
	DB	$4C,<C0644,>C0644	; SKPFLS	C0644
; 2315:         prevstmnt = token
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$78,<D0953,>D0953	; SAB	D0953
; 2316:     fin
C0644:
C0645:
; 2317:     when token
	DB	$68,<D0413,>D0413	; LAB	D0413
; 2318:         is IF_TKN
	DB	$2A,$83			; CB	131
	DB	$3E,<C0647,>C0647	; SKPNE	C0647
; 2319:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2320:             tag_else  = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$06			; SLW	6
; 2321:             tag_endif = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$08			; SLW	8
; 2322:             emit_brfls_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0202,>C0202	; CALL	C0202
; 2323:             drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2324:             repeat
C0649:
; 2325:                 while parse_stmnt_01()
C0650:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0651,>C0651	; SKPFLS	C0651
; 2326:                     drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2327:                 loop
	DB	$50,<C0650,>C0650	; SKIP	C0650
C0651:
; 2328:                 if token <> ELSEIF_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$84			; CB	132
	DB	$42			; ISNE
	DB	$4C,<C0652,>C0652	; SKPFLS	C0652
; 2329:                     break
	DB	$50,<C0648,>C0648	; SKIP	C0648
; 2330:                 fin
C0652:
C0653:
; 2331:                 emit_jump_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0210,>C0210	; CALL	C0210
; 2332:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2333:                 if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0654,>C0654	; SKPFLS	C0654
; 2334:                     return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2335:                 fin
C0654:
C0655:
; 2336:                 tag_else = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$06			; SLW	6
; 2337:                 emit_brfls_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0202,>C0202	; CALL	C0202
; 2338:             until FALSE
	DB	$00			; ZERO
	DB	$4C,<C0649,>C0649	; SKPFLS	C0649
C0648:
; 2339:             if token == ELSE_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$85			; CB	133
	DB	$40			; ISEQ
	DB	$4C,<C0656,>C0656	; SKPFLS	C0656
; 2340:                 emit_jump_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0210,>C0210	; CALL	C0210
; 2341:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2342:                 drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2343:                 while parse_stmnt_01()
C0658:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0659,>C0659	; SKPFLS	C0659
; 2344:                     drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2345:                 loop
	DB	$50,<C0658,>C0658	; SKIP	C0658
C0659:
; 2346:                 emit_codetag_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2347:             else
	DB	$50,<C0657,>C0657	; SKIP	C0657
C0656:
; 2348:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2349:                 emit_codetag_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2350:             fin
C0657:
; 2351:             if token <> FIN_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$86			; CB	134
	DB	$42			; ISNE
	DB	$4C,<C0660,>C0660	; SKPFLS	C0660
; 2352:                 return parse_err_11(@no_fin)
	DB	$30			; DROP
	DB	$26,<D0775,>D0775	; LA	D0775
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2353:             fin
C0660:
C0661:
; 2354:         is FOR_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0647:
	DB	$2A,$8E			; CB	142
	DB	$3E,<C0662,>C0662	; SKPNE	C0662
; 2355:             stack_loop  = stack_loop + 1
	DB	$68,<D0952,>D0952	; LAB	D0952
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0952,>D0952	; SAB	D0952
; 2356:             tag_for     = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$10			; SLW	16
; 2357:             tag_prevbrk = break_tag
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$76,$04			; SLW	4
; 2358:             break_tag   = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2359:             if scan_01() <> ID_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$D6			; CB	214
	DB	$42			; ISNE
	DB	$4C,<C0663,>C0663	; SKPFLS	C0663
; 2360:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2361:             fin
C0663:
C0664:
; 2362:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$54,<C0335,>C0335	; CALL	C0335
	DB	$76,$14			; SLW	20
; 2363:             if idptr
	DB	$66,$14			; LLW	20
	DB	$4C,<C0665,>C0665	; SKPFLS	C0665
; 2364:                 type  = (idptr).idtype
	DB	$66,$14			; LLW	20
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$02			; SLB	2
; 2365:                 addr  = (idptr):idval
	DB	$66,$14			; LLW	20
	DB	$62			; LW
	DB	$76,$18			; SLW	24
; 2366:             else
	DB	$50,<C0666,>C0666	; SKIP	C0666
C0665:
; 2367:                 return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2368:             fin
C0666:
; 2369:             if scan_01() <> SET_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$BD			; CB	189
	DB	$42			; ISNE
	DB	$4C,<C0667,>C0667	; SKPFLS	C0667
; 2370:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2371:             fin
C0667:
C0668:
; 2372:             if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0669,>C0669	; SKPFLS	C0669
; 2373:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2374:             fin
C0669:
C0670:
; 2375:             emit_codetag_10(tag_for)
	DB	$66,$10			; LLW	16
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2376:             if type & LOCAL_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0671,>C0671	; SKPFLS	C0671
; 2377:                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0673,>C0673	; SKPFLS	C0673
; 2378:                     emit_dlb_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0128,>C0128	; CALL	C0128
; 2379:                 else
	DB	$50,<C0674,>C0674	; SKIP	C0674
C0673:
; 2380:                     emit_dlw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0130,>C0130	; CALL	C0130
; 2381:                 fin
C0674:
; 2382:             else
	DB	$50,<C0672,>C0672	; SKIP	C0672
C0671:
; 2383:                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0675,>C0675	; SKPFLS	C0675
; 2384:                     emit_dab_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0136,>C0136	; CALL	C0136
; 2385:                 else
	DB	$50,<C0676,>C0676	; SKIP	C0676
C0675:
; 2386:                     emit_daw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0138,>C0138	; CALL	C0138
; 2387:                 fin
C0676:
; 2388:             fin
C0672:
; 2389:             stepdir = 1
	DB	$2A,$01			; CB	1
	DB	$76,$1A			; SLW	26
; 2390:             if token == TO_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$8F			; CB	143
	DB	$40			; ISEQ
	DB	$4C,<C0677,>C0677	; SKPFLS	C0677
; 2391:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2392:             elsif token == DOWNTO_TKN
	DB	$50,<C0678,>C0678	; SKIP	C0678
C0677:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$90			; CB	144
	DB	$40			; ISEQ
	DB	$4C,<C0679,>C0679	; SKPFLS	C0679
; 2393:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2394:                 stepdir = -1
	DB	$2C,$FF,$FF		; CW	-1
	DB	$76,$1A			; SLW	26
; 2395:             fin
C0679:
C0678:
; 2396:             if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0680,>C0680	; SKPFLS	C0680
; 2397:                 emit_brgt_10(break_tag)
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$54,<C0204,>C0204	; CALL	C0204
; 2398:             else
	DB	$50,<C0681,>C0681	; SKIP	C0681
C0680:
; 2399:                 emit_brlt_10(break_tag)
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$54,<C0206,>C0206	; CALL	C0206
; 2400:             fin
C0681:
; 2401:             if token == STEP_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$91			; CB	145
	DB	$40			; ISEQ
	DB	$4C,<C0682,>C0682	; SKPFLS	C0682
; 2402:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2403:                 if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0684,>C0684	; SKPFLS	C0684
; 2404:                     drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0169,>C0169	; CALL	C0169
	DB	$30			; DROP
; 2405:                 else
	DB	$50,<C0685,>C0685	; SKIP	C0685
C0684:
; 2406:                     drop emit_binaryop_11(SUB_TKN)
	DB	$2A,$AD			; CB	173
	DB	$54,<C0169,>C0169	; CALL	C0169
	DB	$30			; DROP
; 2407:                 fin
C0685:
; 2408:             else
	DB	$50,<C0683,>C0683	; SKIP	C0683
C0682:
; 2409:                 if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0686,>C0686	; SKPFLS	C0686
; 2410:                     drop emit_unaryop_11(INC_TKN)
	DB	$2A,$C1			; CB	193
	DB	$54,<C0158,>C0158	; CALL	C0158
	DB	$30			; DROP
; 2411:                 else
	DB	$50,<C0687,>C0687	; SKIP	C0687
C0686:
; 2412:                     drop emit_unaryop_11(DEC_TKN)
	DB	$2A,$C4			; CB	196
	DB	$54,<C0158,>C0158	; CALL	C0158
	DB	$30			; DROP
; 2413:                 fin
C0687:
; 2414:             fin
C0683:
; 2415:             while parse_stmnt_01()
C0688:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0689,>C0689	; SKPFLS	C0689
; 2416:                 drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2417:             loop
	DB	$50,<C0688,>C0688	; SKIP	C0688
C0689:
; 2418:             if token <> NEXT_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$92			; CB	146
	DB	$42			; ISNE
	DB	$4C,<C0690,>C0690	; SKPFLS	C0690
; 2419:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2420:             fin
C0690:
C0691:
; 2421:             emit_jump_10(tag_for)
	DB	$66,$10			; LLW	16
	DB	$54,<C0210,>C0210	; CALL	C0210
; 2422:             emit_codetag_10(break_tag)
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2423:             emit_drop()
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2424:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2425:             stack_loop = stack_loop - 1
	DB	$68,<D0952,>D0952	; LAB	D0952
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0952,>D0952	; SAB	D0952
; 2426:         is WHILE_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0662:
	DB	$2A,$88			; CB	136
	DB	$3E,<C0692,>C0692	; SKPNE	C0692
; 2427:             tag_while   = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$0A			; SLW	10
; 2428:             tag_wend    = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$0C			; SLW	12
; 2429:             tag_prevbrk = break_tag
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$76,$04			; SLW	4
; 2430:             break_tag   = tag_wend
	DB	$66,$0C			; LLW	12
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2431:             emit_codetag_10(tag_while)
	DB	$66,$0A			; LLW	10
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2432:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2433:             emit_brfls_10(tag_wend)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0202,>C0202	; CALL	C0202
; 2434:             while parse_stmnt_01()
C0693:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0694,>C0694	; SKPFLS	C0694
; 2435:                 drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2436:             loop
	DB	$50,<C0693,>C0693	; SKIP	C0693
C0694:
; 2437:             if token <> LOOP_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$89			; CB	137
	DB	$42			; ISNE
	DB	$4C,<C0695,>C0695	; SKPFLS	C0695
; 2438:                 return parse_err_11(@no_loop)
	DB	$30			; DROP
	DB	$26,<D0787,>D0787	; LA	D0787
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2439:             fin
C0695:
C0696:
; 2440:             emit_jump_10(tag_while)
	DB	$66,$0A			; LLW	10
	DB	$54,<C0210,>C0210	; CALL	C0210
; 2441:             emit_codetag_10(tag_wend)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2442:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2443:         is REPEAT_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0692:
	DB	$2A,$93			; CB	147
	DB	$3E,<C0697,>C0697	; SKPNE	C0697
; 2444:             tag_repeat  = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$0E			; SLW	14
; 2445:             tag_prevbrk = break_tag
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$76,$04			; SLW	4
; 2446:             break_tag   = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2447:             emit_codetag_10(tag_repeat)
	DB	$66,$0E			; LLW	14
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2448:             drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2449:             while parse_stmnt_01()
C0698:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0699,>C0699	; SKPFLS	C0699
; 2450:                 drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2451:             loop
	DB	$50,<C0698,>C0698	; SKIP	C0698
C0699:
; 2452:             if token <> UNTIL_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$94			; CB	148
	DB	$42			; ISNE
	DB	$4C,<C0700,>C0700	; SKPFLS	C0700
; 2453:                 return parse_err_11(@no_until)
	DB	$30			; DROP
	DB	$26,<D0800,>D0800	; LA	D0800
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2454:             fin
C0700:
C0701:
; 2455:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2456:             emit_brfls_10(tag_repeat)
	DB	$66,$0E			; LLW	14
	DB	$54,<C0202,>C0202	; CALL	C0202
; 2457:             emit_codetag_10(break_tag)
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2458:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2459:         is CASE_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0697:
	DB	$2A,$8A			; CB	138
	DB	$3E,<C0702,>C0702	; SKPNE	C0702
; 2460:             stack_loop  = stack_loop + 1
	DB	$68,<D0952,>D0952	; LAB	D0952
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0952,>D0952	; SAB	D0952
; 2461:             tag_choice  = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$12			; SLW	18
; 2462:             tag_prevbrk = break_tag
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$76,$04			; SLW	4
; 2463:             break_tag   = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2464:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2465:             drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2466:             while token <> ENDCASE_TKN
C0703:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$8D			; CB	141
	DB	$42			; ISNE
	DB	$4C,<C0704,>C0704	; SKPFLS	C0704
; 2467:                 when token
	DB	$68,<D0413,>D0413	; LAB	D0413
; 2468:                     is OF_TKN
	DB	$2A,$8B			; CB	139
	DB	$3E,<C0706,>C0706	; SKPNE	C0706
; 2469:                         if !parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$20			; NOT
	DB	$4C,<C0707,>C0707	; SKPFLS	C0707
; 2470:                             return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2471:                         fin
C0707:
C0708:
; 2472:                         emit_brne_10(tag_choice)
	DB	$66,$12			; LLW	18
	DB	$54,<C0208,>C0208	; CALL	C0208
; 2473:                         while parse_stmnt_01()
C0709:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0710,>C0710	; SKPFLS	C0710
; 2474:                             drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2475:                         loop
	DB	$50,<C0709,>C0709	; SKIP	C0709
C0710:
; 2476:                         emit_jump_10(break_tag)
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$54,<C0210,>C0210	; CALL	C0210
; 2477:                         emit_codetag_10(tag_choice)
	DB	$66,$12			; LLW	18
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2478:                         tag_choice = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$12			; SLW	18
; 2479:                     is DEFAULT_TKN
	DB	$50,<C0705,>C0705	; SKIP	C0705
C0706:
	DB	$2A,$8C			; CB	140
	DB	$3E,<C0711,>C0711	; SKPNE	C0711
; 2480:                         drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2481:                         while parse_stmnt_01()
C0712:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0713,>C0713	; SKPFLS	C0713
; 2482:                             drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2483:                         loop
	DB	$50,<C0712,>C0712	; SKIP	C0712
C0713:
; 2484:                         if token <> ENDCASE_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$8D			; CB	141
	DB	$42			; ISNE
	DB	$4C,<C0714,>C0714	; SKPFLS	C0714
; 2485:                             return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2486:                         fin
C0714:
C0715:
; 2487:                     otherwise
	DB	$50,<C0705,>C0705	; SKIP	C0705
C0711:
; 2488:                         return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2489:                 wend
C0705:
	DB	$30			; DROP
; 2490:             loop
	DB	$50,<C0703,>C0703	; SKIP	C0703
C0704:
; 2491:             emit_codetag_10(break_tag)
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2492:             emit_drop()
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2493:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D0956,>D0956	; SAW	D0956
; 2494:             stack_loop = stack_loop - 1
	DB	$68,<D0952,>D0952	; LAB	D0952
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0952,>D0952	; SAB	D0952
; 2495:         is BREAK_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0702:
	DB	$2A,$9A			; CB	154
	DB	$3E,<C0717,>C0717	; SKPNE	C0717
; 2496:             if break_tag
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$4C,<C0718,>C0718	; SKPFLS	C0718
; 2497:                 emit_jump_10(break_tag)
	DB	$6A,<D0956,>D0956	; LAW	D0956
	DB	$54,<C0210,>C0210	; CALL	C0210
; 2498:             else
	DB	$50,<C0719,>C0719	; SKIP	C0719
C0718:
; 2499:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2500:             fin
C0719:
; 2501:         is RETURN_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0717:
	DB	$2A,$99			; CB	153
	DB	$3E,<C0720,>C0720	; SKPNE	C0720
; 2502:             if infunc
	DB	$68,<D0951,>D0951	; LAB	D0951
	DB	$4C,<C0721,>C0721	; SKPFLS	C0721
; 2503:                 for i = 1 to stack_loop
	DB	$2A,$01			; CB	1
C0724:
	DB	$6C,$03			; DLB	3
	DB	$68,<D0952,>D0952	; LAB	D0952
	DB	$3A,<C0723,>C0723	; SKPGT	C0723
	DB	$0C			; INCR
; 2504:                     emit_drop()
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2505:                 next
	DB	$50,<C0724,>C0724	; SKIP	C0724
C0723:
	DB	$30			; DROP
; 2506:                 drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2507:                 emit_leave_10(framesize)
	DB	$6A,<D0008,>D0008	; LAW	D0008
	DB	$54,<C0216,>C0216	; CALL	C0216
; 2508:             else
	DB	$50,<C0722,>C0722	; SKIP	C0722
C0721:
; 2509:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0535,>D0535	; LA	D0535
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2510:             fin
C0722:
; 2511:         is EXIT_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0720:
	DB	$2A,$9C			; CB	156
	DB	$3E,<C0725,>C0725	; SKPNE	C0725
; 2512:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2513:             emit_exit()
	DB	$54,<C0226,>C0226	; CALL	C0226
; 2514:         is DROP_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0725:
	DB	$2A,$97			; CB	151
	DB	$3E,<C0726,>C0726	; SKPNE	C0726
; 2515:             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2516:             emit_drop()
	DB	$54,<C0212,>C0212	; CALL	C0212
; 2517:         is ELSE_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0726:
	DB	$2A,$85			; CB	133
	DB	$3E,<C0727,>C0727	; SKPNE	C0727
; 2518:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2519:         is ELSEIF_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0727:
	DB	$2A,$84			; CB	132
	DB	$3E,<C0728,>C0728	; SKPNE	C0728
; 2520:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2521:         is FIN_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0728:
	DB	$2A,$86			; CB	134
	DB	$3E,<C0729,>C0729	; SKPNE	C0729
; 2522:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2523:         is LOOP_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0729:
	DB	$2A,$89			; CB	137
	DB	$3E,<C0730,>C0730	; SKPNE	C0730
; 2524:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2525:         is UNTIL_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0730:
	DB	$2A,$94			; CB	148
	DB	$3E,<C0731,>C0731	; SKPNE	C0731
; 2526:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2527:         is NEXT_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0731:
	DB	$2A,$92			; CB	146
	DB	$3E,<C0732,>C0732	; SKPNE	C0732
; 2528:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2529:         is OF_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0732:
	DB	$2A,$8B			; CB	139
	DB	$3E,<C0733,>C0733	; SKPNE	C0733
; 2530:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2531:         is DEFAULT_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0733:
	DB	$2A,$8C			; CB	140
	DB	$3E,<C0734,>C0734	; SKPNE	C0734
; 2532:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2533:         is ENDCASE_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0734:
	DB	$2A,$8D			; CB	141
	DB	$3E,<C0735,>C0735	; SKPNE	C0735
; 2534:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2535:         is END_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0735:
	DB	$2A,$87			; CB	135
	DB	$3E,<C0736,>C0736	; SKPNE	C0736
; 2536:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2537:         is DONE_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0736:
	DB	$2A,$98			; CB	152
	DB	$3E,<C0737,>C0737	; SKPNE	C0737
; 2538:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2539:         is IFUNC_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0737:
	DB	$2A,$95			; CB	149
	DB	$3E,<C0738,>C0738	; SKPNE	C0738
; 2540:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2541:         is NFUNC_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0738:
	DB	$2A,$96			; CB	150
	DB	$3E,<C0739,>C0739	; SKPNE	C0739
; 2542:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2543:         is EOF_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0739:
	DB	$2A,$01			; CB	1
	DB	$3E,<C0740,>C0740	; SKPNE	C0740
; 2544:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2545:         is EOL_TKN
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0740:
	DB	$2A,$02			; CB	2
	DB	$3E,<C0741,>C0741	; SKPNE	C0741
; 2546:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2547:         otherwise
	DB	$50,<C0646,>C0646	; SKIP	C0646
C0741:
; 2548:             if token == ID_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C0743,>C0743	; SKPFLS	C0743
; 2549:                 saveptr = tknptr
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$76,$16			; SLW	22
; 2550:                 idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$54,<C0335,>C0335	; CALL	C0335
	DB	$76,$14			; SLW	20
; 2551:                 if !idptr
	DB	$66,$14			; LLW	20
	DB	$20			; NOT
	DB	$4C,<C0745,>C0745	; SKPFLS	C0745
; 2552:                     return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2553:                 fin
C0745:
C0746:
; 2554:                 type = (idptr).idtype
	DB	$66,$14			; LLW	20
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$02			; SLB	2
; 2555:                 if type & ADDR_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C0747,>C0747	; SKPFLS	C0747
; 2556:                     addr = (idptr):idval
	DB	$66,$14			; LLW	20
	DB	$62			; LW
	DB	$76,$18			; SLW	24
; 2557:                     if scan_01() == SET_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C0749,>C0749	; SKPFLS	C0749
; 2558:                         if type & VAR_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0751,>C0751	; SKPFLS	C0751
; 2559:                             drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2560:                             if type & LOCAL_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0753,>C0753	; SKPFLS	C0753
; 2561:                                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0755,>C0755	; SKPFLS	C0755
; 2562:                                     emit_slb_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0124,>C0124	; CALL	C0124
; 2563:                                 else
	DB	$50,<C0756,>C0756	; SKIP	C0756
C0755:
; 2564:                                     emit_slw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0126,>C0126	; CALL	C0126
; 2565:                                 fin
C0756:
; 2566:                             else
	DB	$50,<C0754,>C0754	; SKIP	C0754
C0753:
; 2567:                                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0757,>C0757	; SKPFLS	C0757
; 2568:                                     emit_sab_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0132,>C0132	; CALL	C0132
; 2569:                                 else
	DB	$50,<C0758,>C0758	; SKIP	C0758
C0757:
; 2570:                                     emit_saw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0134,>C0134	; CALL	C0134
; 2571:                                 fin
C0758:
; 2572:                             fin
C0754:
; 2573:                             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2574:                         fin
C0751:
C0752:
; 2575:                     elsif token == SETLIST_TKN and type & VAR_TYPE
	DB	$50,<C0750,>C0750	; SKIP	C0750
C0749:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$64,$02			; LLB	2
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0759,>C0759	; SKPFLS	C0759
; 2576:                             return parse_setlist_21(addr, type);
	DB	$30			; DROP
	DB	$66,$18			; LLW	24
	DB	$64,$02			; LLB	2
	DB	$54,<C0602,>C0602	; CALL	C0602
	DB	$5A			; LEAVE
; 2577:                     elsif token == EOL_TKN and type & FUNC_TYPE
	DB	$50,<C0750,>C0750	; SKIP	C0750
C0759:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$02			; CB	2
	DB	$40			; ISEQ
	DB	$64,$02			; LLB	2
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0760,>C0760	; SKPFLS	C0760
; 2578:                         emit_call_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0140,>C0140	; CALL	C0140
; 2579:                         return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2580:                     fin
C0760:
C0750:
; 2581:                 fin
C0747:
C0748:
; 2582:                 tknptr = saveptr
	DB	$66,$16			; LLW	22
	DB	$7A,<D0417,>D0417	; SAW	D0417
; 2583:             fin
C0743:
C0744:
; 2584:             rewind_10(tknptr)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$54,<C0299,>C0299	; CALL	C0299
; 2585:             type = parse_value_11(0)
	DB	$00			; ZERO
	DB	$54,<C0424,>C0424	; CALL	C0424
	DB	$74,$02			; SLB	2
; 2586:             if type
	DB	$64,$02			; LLB	2
	DB	$4C,<C0761,>C0761	; SKPFLS	C0761
; 2587:                 if token == SET_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C0763,>C0763	; SKPFLS	C0763
; 2588:                     drop parse_expr_01()
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
; 2589:                     if type & XBYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$22			; CB	34
	DB	$14			; BAND
	DB	$4C,<C0765,>C0765	; SKPFLS	C0765
; 2590:                         emit_sb()
	DB	$54,<C0120,>C0120	; CALL	C0120
; 2591:                     else
	DB	$50,<C0766,>C0766	; SKIP	C0766
C0765:
; 2592:                         emit_sw()
	DB	$54,<C0122,>C0122	; CALL	C0122
; 2593:                     fin
C0766:
; 2594:                 elsif token == SETLIST_TKN
	DB	$50,<C0764,>C0764	; SKIP	C0764
C0763:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$4C,<C0767,>C0767	; SKPFLS	C0767
; 2595:                         return parse_setlist_21(0, type);
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$64,$02			; LLB	2
	DB	$54,<C0602,>C0602	; CALL	C0602
	DB	$5A			; LEAVE
; 2596:                 else
	DB	$50,<C0764,>C0764	; SKIP	C0764
C0767:
; 2597:                     if type & BPTR_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C0768,>C0768	; SKPFLS	C0768
; 2598:                         emit_lb()
	DB	$54,<C0108,>C0108	; CALL	C0108
; 2599:                     elsif type & WPTR_TYPE
	DB	$50,<C0769,>C0769	; SKIP	C0769
C0768:
	DB	$64,$02			; LLB	2
	DB	$2A,$40			; CB	64
	DB	$14			; BAND
	DB	$4C,<C0770,>C0770	; SKPFLS	C0770
; 2600:                         emit_lw()
	DB	$54,<C0110,>C0110	; CALL	C0110
; 2601:                     fin
C0770:
C0769:
; 2602:                 fin
C0764:
; 2603:             else
	DB	$50,<C0762,>C0762	; SKIP	C0762
C0761:
; 2604:                 return parse_err_11(@bad_syntax)
	DB	$30			; DROP
	DB	$26,<D0563,>D0563	; LA	D0563
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2605:             fin
C0762:
; 2606:     wend
C0646:
	DB	$30			; DROP
; 2607:     if scan_01() <> EOL_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C0771,>C0771	; SKPFLS	C0771
; 2608:         return parse_err_11(@bad_syntax)
	DB	$26,<D0563,>D0563	; LA	D0563
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2609:     fin
C0771:
C0772:
; 2610:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2611: end
; 2612: def parse_var_11(type)
C0773:					; parse_var_11()
					; type = 2
; 2613:     byte consttype, constsize, idlen
					; consttype = 4
					; constsize = 5
					; idlen = 6
; 2614:     word idptr, constval, arraysize, size
					; idptr = 7
					; constval = 9
					; arraysize = 11
					; size = 13
; 2615: 
; 2616:     idlen = 0
	JSR	INTERP
	DB	$58,$0F,$01		; ENTER	15,1
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 2617:     size  = 1
	DB	$2A,$01			; CB	1
	DB	$76,$0D			; SLW	13
; 2618:     if scan_01() == ID_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C0775,>C0775	; SKPFLS	C0775
; 2619:         idptr = tknptr
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$76,$07			; SLW	7
; 2620:         idlen = tknlen
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$74,$06			; SLB	6
; 2621:         if scan_01() == OPEN_BRACKET_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C0777,>C0777	; SKPFLS	C0777
; 2622:             size = 0
	DB	$00			; ZERO
	DB	$76,$0D			; SLW	13
; 2623:             drop parse_constexpr_21(@size, @constsize)
	DB	$28,$0D			; LLA	13
	DB	$28,$05			; LLA	5
	DB	$54,<C0551,>C0551	; CALL	C0551
	DB	$30			; DROP
; 2624:             if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C0779,>C0779	; SKPFLS	C0779
; 2625:                 return parse_err_11(@no_close_bracket)
	DB	$26,<D0735,>D0735	; LA	D0735
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2626:             fin
C0779:
C0780:
; 2627:             drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2628:         fin
C0777:
C0778:
; 2629:     fin
C0775:
C0776:
; 2630:     if type == WORD_TYPE
	DB	$66,$02			; LLW	2
	DB	$2A,$04			; CB	4
	DB	$40			; ISEQ
	DB	$4C,<C0781,>C0781	; SKPFLS	C0781
; 2631:         size = size * 2
	DB	$66,$0D			; LLW	13
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$76,$0D			; SLW	13
; 2632:     fin
C0781:
C0782:
; 2633:     if token == SET_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C0783,>C0783	; SKPFLS	C0783
; 2634:         if infunc
	DB	$68,<D0951,>D0951	; LAB	D0951
	DB	$4C,<C0785,>C0785	; SKPFLS	C0785
; 2635:             return parse_err_11(@no_local_init)
	DB	$26,<D0827,>D0827	; LA	D0827
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2636:         fin
C0785:
C0786:
; 2637:         if idlen
	DB	$64,$06			; LLB	6
	DB	$4C,<C0787,>C0787	; SKPFLS	C0787
; 2638:             drop iddata_add_41(idptr, idlen, type, 0)
	DB	$66,$07			; LLW	7
	DB	$64,$06			; LLB	6
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$54,<C0351,>C0351	; CALL	C0351
	DB	$30			; DROP
; 2639:         fin
C0787:
C0788:
; 2640:         consttype = parse_constexpr_21(@constval, @constsize)
	DB	$28,$09			; LLA	9
	DB	$28,$05			; LLA	5
	DB	$54,<C0551,>C0551	; CALL	C0551
	DB	$74,$04			; SLB	4
; 2641:         if consttype
	DB	$64,$04			; LLB	4
	DB	$4C,<C0789,>C0789	; SKPFLS	C0789
; 2642:             arraysize = emit_data_41(type, consttype, constval, constsize)
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$66,$09			; LLW	9
	DB	$64,$05			; LLB	5
	DB	$54,<C0094,>C0094	; CALL	C0094
	DB	$76,$0B			; SLW	11
; 2643:             while token == COMMA_TKN
C0791:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$AC			; CB	172
	DB	$40			; ISEQ
	DB	$4C,<C0792,>C0792	; SKPFLS	C0792
; 2644:                 consttype = parse_constexpr_21(@constval, @constsize)
	DB	$28,$09			; LLA	9
	DB	$28,$05			; LLA	5
	DB	$54,<C0551,>C0551	; CALL	C0551
	DB	$74,$04			; SLB	4
; 2645:                 if consttype
	DB	$64,$04			; LLB	4
	DB	$4C,<C0793,>C0793	; SKPFLS	C0793
; 2646:                     arraysize = arraysize + emit_data_41(type, consttype, constval, constsize)
	DB	$66,$0B			; LLW	11
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$66,$09			; LLW	9
	DB	$64,$05			; LLB	5
	DB	$54,<C0094,>C0094	; CALL	C0094
	DB	$02			; ADD
	DB	$76,$0B			; SLW	11
; 2647:                 else
	DB	$50,<C0794,>C0794	; SKIP	C0794
C0793:
; 2648:                     return parse_err_11(@bad_decl)
	DB	$26,<D0505,>D0505	; LA	D0505
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2649:                 fin
C0794:
; 2650:             loop
	DB	$50,<C0791,>C0791	; SKIP	C0791
C0792:
; 2651:             if token <> EOL_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C0795,>C0795	; SKPFLS	C0795
; 2652:                 return parse_err_11(@no_close_bracket)
	DB	$26,<D0735,>D0735	; LA	D0735
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2653:             fin
C0795:
C0796:
; 2654:             iddata_size_30(PTR_TYPE, size, arraysize);
	DB	$2A,$60			; CB	96
	DB	$66,$0D			; LLW	13
	DB	$66,$0B			; LLW	11
	DB	$54,<C0357,>C0357	; CALL	C0357
; 2655:         else
	DB	$50,<C0790,>C0790	; SKIP	C0790
C0789:
; 2656:             return parse_err_11(@bad_decl)
	DB	$26,<D0505,>D0505	; LA	D0505
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2657:         fin
C0790:
; 2658:     elsif idlen
	DB	$50,<C0784,>C0784	; SKIP	C0784
C0783:
	DB	$64,$06			; LLB	6
	DB	$4C,<C0797,>C0797	; SKPFLS	C0797
; 2659:         if infunc
	DB	$68,<D0951,>D0951	; LAB	D0951
	DB	$4C,<C0798,>C0798	; SKPFLS	C0798
; 2660:             drop idlocal_add_41(idptr, idlen, type, size)
	DB	$66,$07			; LLW	7
	DB	$64,$06			; LLB	6
	DB	$66,$02			; LLW	2
	DB	$66,$0D			; LLW	13
	DB	$54,<C0343,>C0343	; CALL	C0343
	DB	$30			; DROP
; 2661:         else
	DB	$50,<C0799,>C0799	; SKIP	C0799
C0798:
; 2662:             drop iddata_add_41(idptr, idlen, type, size)
	DB	$66,$07			; LLW	7
	DB	$64,$06			; LLB	6
	DB	$66,$02			; LLW	2
	DB	$66,$0D			; LLW	13
	DB	$54,<C0351,>C0351	; CALL	C0351
	DB	$30			; DROP
; 2663:         fin
C0799:
; 2664:     fin
C0797:
C0784:
; 2665:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2666: end
; 2667: def parse_vars_01
C0800:					; parse_vars_01()
; 2668:     byte idlen, type, size
					; idlen = 2
					; type = 3
					; size = 4
; 2669:     word value, idptr
					; value = 5
					; idptr = 7
; 2670: 
; 2671:     when token
	JSR	INTERP
	DB	$58,$09,$00		; ENTER	9,0
	DB	$68,<D0413,>D0413	; LAB	D0413
; 2672:         is CONST_TKN
	DB	$2A,$80			; CB	128
	DB	$3E,<C0803,>C0803	; SKPNE	C0803
; 2673:             if scan_01() <> ID_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$D6			; CB	214
	DB	$42			; ISNE
	DB	$4C,<C0804,>C0804	; SKPFLS	C0804
; 2674:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0474,>D0474	; LA	D0474
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2675:             fin
C0804:
C0805:
; 2676:             idptr = tknptr;
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$76,$07			; SLW	7
; 2677:             idlen = tknlen
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$74,$02			; SLB	2
; 2678:             if scan_01() <> SET_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$BD			; CB	189
	DB	$42			; ISNE
	DB	$4C,<C0806,>C0806	; SKPFLS	C0806
; 2679:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0474,>D0474	; LA	D0474
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2680:             fin
C0806:
C0807:
; 2681:             if !parse_constexpr_21(@value, @size)
	DB	$28,$05			; LLA	5
	DB	$28,$04			; LLA	4
	DB	$54,<C0551,>C0551	; CALL	C0551
	DB	$20			; NOT
	DB	$4C,<C0808,>C0808	; SKPFLS	C0808
; 2682:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0474,>D0474	; LA	D0474
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2683:             fin
C0808:
C0809:
; 2684:             drop idconst_add_31(idptr, idlen, value)
	DB	$66,$07			; LLW	7
	DB	$64,$02			; LLB	2
	DB	$66,$05			; LLW	5
	DB	$54,<C0369,>C0369	; CALL	C0369
	DB	$30			; DROP
; 2685:         is BYTE_TKN
	DB	$50,<C0802,>C0802	; SKIP	C0802
C0803:
	DB	$2A,$81			; CB	129
	DB	$3E,<C0810,>C0810	; SKPNE	C0810
; 2686:             type = BYTE_TYPE
	DB	$2A,$02			; CB	2
	DB	$74,$03			; SLB	3
; 2687:             repeat
C0812:
; 2688:                 if !parse_var_11(type)
	DB	$64,$03			; LLB	3
	DB	$54,<C0773,>C0773	; CALL	C0773
	DB	$20			; NOT
	DB	$4C,<C0813,>C0813	; SKPFLS	C0813
; 2689:                     return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2690:                 fin
C0813:
C0814:
; 2691:             until token <> COMMA_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C0812,>C0812	; SKPFLS	C0812
C0811:
; 2692:         is WORD_TKN
	DB	$50,<C0802,>C0802	; SKIP	C0802
C0810:
	DB	$2A,$82			; CB	130
	DB	$3E,<C0815,>C0815	; SKPNE	C0815
; 2693:             type = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$03			; SLB	3
; 2694:             repeat
C0817:
; 2695:                 if !parse_var_11(type)
	DB	$64,$03			; LLB	3
	DB	$54,<C0773,>C0773	; CALL	C0773
	DB	$20			; NOT
	DB	$4C,<C0818,>C0818	; SKPFLS	C0818
; 2696:                     return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2697:                 fin
C0818:
C0819:
; 2698:             until token <> COMMA_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C0817,>C0817	; SKPFLS	C0817
C0816:
; 2699:         is FUNC_TKN
	DB	$50,<C0802,>C0802	; SKIP	C0802
C0815:
	DB	$2A,$9E			; CB	158
	DB	$3E,<C0820,>C0820	; SKPNE	C0820
; 2700:             repeat
C0822:
; 2701:                 if scan_01() == ID_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C0823,>C0823	; SKPFLS	C0823
; 2702:                     drop idfunc_add_31(tknptr, tknlen, ctag_new_01())
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 2703:                 else
	DB	$50,<C0824,>C0824	; SKIP	C0824
C0823:
; 2704:                     return parse_err_11(@bad_decl)
	DB	$30			; DROP
	DB	$26,<D0505,>D0505	; LA	D0505
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2705:                 fin
C0824:
; 2706:             until scan_01() <> COMMA_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C0822,>C0822	; SKPFLS	C0822
C0821:
; 2707:         is EOL_TKN
	DB	$50,<C0802,>C0802	; SKIP	C0802
C0820:
	DB	$2A,$02			; CB	2
	DB	$3E,<C0825,>C0825	; SKPNE	C0825
; 2708:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2709:         otherwise
	DB	$50,<C0802,>C0802	; SKIP	C0802
C0825:
; 2710:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2711:     wend
C0802:
	DB	$30			; DROP
; 2712:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2713: end
; 2714: def parse_func_01
C0827:					; parse_func_01()
; 2715:     byte opt, cfnparms
					; opt = 2
					; cfnparms = 3
; 2716:     word func_tag, idptr
					; func_tag = 4
					; idptr = 6
; 2717: 
; 2718:     if token == IFUNC_TKN or token == NFUNC_TKN
	JSR	INTERP
	DB	$58,$08,$00		; ENTER	8,0
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$95			; CB	149
	DB	$40			; ISEQ
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$96			; CB	150
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0829,>C0829	; SKPFLS	C0829
; 2719:         opt = token - IFUNC_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$95			; CB	149
	DB	$04			; SUB
	DB	$74,$02			; SLB	2
; 2720:         if scan_01() <> ID_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$D6			; CB	214
	DB	$42			; ISNE
	DB	$4C,<C0831,>C0831	; SKPFLS	C0831
; 2721:             return parse_err_11(@bad_decl)
	DB	$26,<D0505,>D0505	; LA	D0505
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2722:         fin
C0831:
C0832:
; 2723:         cfnparms = 0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 2724:         infunc   = TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$78,<D0951,>D0951	; SAB	D0951
; 2725:         idptr = idglobal_lookup_21(tknptr, tknlen)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$54,<C0341,>C0341	; CALL	C0341
	DB	$76,$06			; SLW	6
; 2726:         if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C0833,>C0833	; SKPFLS	C0833
; 2727:             func_tag = (idptr):idval
	DB	$66,$06			; LLW	6
	DB	$62			; LW
	DB	$76,$04			; SLW	4
; 2728:         else
	DB	$50,<C0834,>C0834	; SKIP	C0834
C0833:
; 2729:             func_tag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$76,$04			; SLW	4
; 2730:             drop idfunc_add_31(tknptr, tknlen, func_tag)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$66,$04			; LLW	4
	DB	$54,<C0367,>C0367	; CALL	C0367
	DB	$30			; DROP
; 2731:         fin
C0834:
; 2732:         emit_codetag_10(func_tag)
	DB	$66,$04			; LLW	4
	DB	$54,<C0082,>C0082	; CALL	C0082
; 2733:         retfunc_tag = ctag_new_01()
	DB	$54,<C0066,>C0066	; CALL	C0066
	DB	$7A,<D0954,>D0954	; SAW	D0954
; 2734:         idlocal_init()
	DB	$54,<C0373,>C0373	; CALL	C0373
; 2735:         if scan_01() == OPEN_PAREN_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$A8			; CB	168
	DB	$40			; ISEQ
	DB	$4C,<C0835,>C0835	; SKPFLS	C0835
; 2736:           repeat
C0838:
; 2737:         if scan_01() == ID_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C0839,>C0839	; SKPFLS	C0839
; 2738:           cfnparms = cfnparms + 1
	DB	$64,$03			; LLB	3
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$03			; SLB	3
; 2739:           drop idlocal_add_41(tknptr, tknlen, WORD_TYPE, 2)
	DB	$6A,<D0417,>D0417	; LAW	D0417
	DB	$68,<D0414,>D0414	; LAB	D0414
	DB	$2A,$04			; CB	4
	DB	$2A,$02			; CB	2
	DB	$54,<C0343,>C0343	; CALL	C0343
	DB	$30			; DROP
; 2740:           drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2741:         fin
C0839:
C0840:
; 2742:             until token <> COMMA_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C0838,>C0838	; SKPFLS	C0838
C0837:
; 2743:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C0841,>C0841	; SKPFLS	C0841
; 2744:                 return parse_err_11(@bad_decl)
	DB	$26,<D0505,>D0505	; LA	D0505
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2745:             fin
C0841:
C0842:
; 2746:             drop scan_01()
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$30			; DROP
; 2747:         fin
C0835:
C0836:
; 2748:         while parse_vars_01()
C0843:
	DB	$54,<C0800,>C0800	; CALL	C0800
	DB	$4C,<C0844,>C0844	; SKPFLS	C0844
; 2749:             drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2750:         loop
	DB	$50,<C0843,>C0843	; SKIP	C0843
C0844:
; 2751:         emit_enter_20(framesize, cfnparms)
	DB	$6A,<D0008,>D0008	; LAW	D0008
	DB	$64,$03			; LLB	3
	DB	$54,<C0220,>C0220	; CALL	C0220
; 2752:         prevstmnt = 0
	DB	$00			; ZERO
	DB	$78,<D0953,>D0953	; SAB	D0953
; 2753:         while parse_stmnt_01()
C0845:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0846,>C0846	; SKPFLS	C0846
; 2754:             drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2755:         loop
	DB	$50,<C0845,>C0845	; SKIP	C0845
C0846:
; 2756:         infunc = FALSE
	DB	$00			; ZERO
	DB	$78,<D0951,>D0951	; SAB	D0951
; 2757:         if token <> END_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$87			; CB	135
	DB	$42			; ISNE
	DB	$4C,<C0847,>C0847	; SKPFLS	C0847
; 2758:             return parse_err_11(@bad_syntax)
	DB	$26,<D0563,>D0563	; LA	D0563
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2759:         fin
C0847:
C0848:
; 2760:         if scan_01() <> EOL_TKN
	DB	$54,<C0246,>C0246	; CALL	C0246
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C0849,>C0849	; SKPFLS	C0849
; 2761:             return parse_err_11(@bad_syntax)
	DB	$26,<D0563,>D0563	; LA	D0563
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$5A			; LEAVE
; 2762:         fin
C0849:
C0850:
; 2763:         if prevstmnt <> RETURN_TKN
	DB	$68,<D0953,>D0953	; LAB	D0953
	DB	$2A,$99			; CB	153
	DB	$42			; ISNE
	DB	$4C,<C0851,>C0851	; SKPFLS	C0851
; 2764:             emit_leave_10(framesize)
	DB	$6A,<D0008,>D0008	; LAW	D0008
	DB	$54,<C0216,>C0216	; CALL	C0216
; 2765:         fin
C0851:
C0852:
; 2766:         return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2767:     elsif token == EOL_TKN
	DB	$50,<C0830,>C0830	; SKIP	C0830
C0829:
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$02			; CB	2
	DB	$40			; ISEQ
	DB	$4C,<C0853,>C0853	; SKPFLS	C0853
; 2768:         return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2769:     fin
C0853:
C0830:
; 2770:     return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2771: end
; 2772: def parse_module_01
C0001:					; parse_module_01()
; 2773:     entrypoint = 0
	JSR	INTERP
	DB	$00			; ZERO
	DB	$7A,<D0016,>D0016	; SAW	D0016
; 2774:     idglobal_init()
	DB	$54,<C0371,>C0371	; CALL	C0371
; 2775:     idlocal_init()
	DB	$54,<C0373,>C0373	; CALL	C0373
; 2776:     if nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$4C,<C0855,>C0855	; SKPFLS	C0855
; 2777:         while parse_vars_01()
C0857:
	DB	$54,<C0800,>C0800	; CALL	C0800
	DB	$4C,<C0858,>C0858	; SKPFLS	C0858
; 2778:             drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2779:         loop
	DB	$50,<C0857,>C0857	; SKIP	C0857
C0858:
; 2780:         while parse_func_01()
C0859:
	DB	$54,<C0827,>C0827	; CALL	C0827
	DB	$4C,<C0860,>C0860	; SKPFLS	C0860
; 2781:             drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2782:         loop
	DB	$50,<C0859,>C0859	; SKIP	C0859
C0860:
; 2783:         if token <> DONE_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$98			; CB	152
	DB	$42			; ISNE
	DB	$4C,<C0861,>C0861	; SKPFLS	C0861
; 2784:             emit_start()
	DB	$54,<C0224,>C0224	; CALL	C0224
; 2785:             prevstmnt = 0
	DB	$00			; ZERO
	DB	$78,<D0953,>D0953	; SAB	D0953
; 2786:             while parse_stmnt_01()
C0863:
	DB	$54,<C0642,>C0642	; CALL	C0642
	DB	$4C,<C0864,>C0864	; SKPFLS	C0864
; 2787:                 drop nextln_01()
	DB	$54,<C0301,>C0301	; CALL	C0301
	DB	$30			; DROP
; 2788:             loop
	DB	$50,<C0863,>C0863	; SKIP	C0863
C0864:
; 2789:             if token <> DONE_TKN
	DB	$68,<D0413,>D0413	; LAB	D0413
	DB	$2A,$98			; CB	152
	DB	$42			; ISNE
	DB	$4C,<C0865,>C0865	; SKPFLS	C0865
; 2790:                 drop parse_err_11(@no_done)
	DB	$26,<D0814,>D0814	; LA	D0814
	DB	$54,<C0062,>C0062	; CALL	C0062
	DB	$30			; DROP
; 2791:             fin
C0865:
C0866:
; 2792:             if prevstmnt <> EXIT_TKN
	DB	$68,<D0953,>D0953	; LAB	D0953
	DB	$2A,$9C			; CB	156
	DB	$42			; ISNE
	DB	$4C,<C0867,>C0867	; SKPFLS	C0867
; 2793:                 emit_const_10(0)
	DB	$00			; ZERO
	DB	$54,<C0103,>C0103	; CALL	C0103
; 2794:                 emit_exit()
	DB	$54,<C0226,>C0226	; CALL	C0226
; 2795:             fin
C0867:
C0868:
; 2796:         fin
C0861:
C0862:
; 2797:         ; dumpsym(idglobal_tbl, globals)
; 2798:         ; prstr(@entrypt_str)
; 2799:         ; prword(entrypoint)
; 2800:         ; crout()
; 2801:         ; keyin_01()
; 2802:         return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2803:     fin
C0855:
C0856:
; 2804:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 2805: end
; 2806: def exec
C0869:					; exec()
; 2807: 	when execentry()
	JSR	INTERP
	DB	$54,<C0004,>C0004	; CALL	C0004
; 2808: 		is 0
	DB	$00			; ZERO
	DB	$3E,<C0872,>C0872	; SKPNE	C0872
; 2809: 	        crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
; 2810:     	    prstr(@donemsg)
	DB	$26,<D0053,>D0053	; LA	D0053
	DB	$54,<C0020,>C0020	; CALL	C0020
; 2811: 		is 1
	DB	$50,<C0871,>C0871	; SKIP	C0871
C0872:
	DB	$2A,$01			; CB	1
	DB	$3E,<C0873,>C0873	; SKPNE	C0873
; 2812: 			crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
; 2813: 			prstr(@brkmsg)
	DB	$26,<D0104,>D0104	; LA	D0104
	DB	$54,<C0020,>C0020	; CALL	C0020
; 2814: 		is 2
	DB	$50,<C0871,>C0871	; SKIP	C0871
C0873:
	DB	$2A,$02			; CB	2
	DB	$3E,<C0874,>C0874	; SKPNE	C0874
; 2815: 			crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
; 2816: 			prstr(@stkovflwmsg)
	DB	$26,<D0117,>D0117	; LA	D0117
	DB	$54,<C0020,>C0020	; CALL	C0020
; 2817: 	wend
	DB	$50,<C0871,>C0871	; SKIP	C0871
C0874:
C0871:
	DB	$30			; DROP
; 2818: end
	DB	$5C			; RET
; 2819: ;
; 2820: ; Compile PLASMA file and execute it
; 2821: ;
; 2822: ;emptystk = estk()
; 2823: prstr(@version)
START:	; JSR	INTERP
	DB	$26,<D0020,>D0020	; LA	D0020
	DB	$54,<C0020,>C0020	; CALL	C0020
; 2824: crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
; 2825: if ^argbuff
	DB	$2C,$06,$20		; CW	8198
	DB	$60			; LB
	DB	$4C,<C0876,>C0876	; SKPFLS	C0876
; 2826:     inref = open_21(argbuff, iobuffer)
	DB	$2C,$06,$20		; CW	8198
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0032,>C0032	; CALL	C0032
	DB	$78,<D0000,>D0000	; SAB	D0000
; 2827: else
	DB	$50,<C0877,>C0877	; SKIP	C0877
C0876:
; 2828:     inref = open_21(rdstr($BA), iobuffer)
	DB	$2A,$BA			; CB	186
	DB	$54,<C0022,>C0022	; CALL	C0022
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0032,>C0032	; CALL	C0032
	DB	$78,<D0000,>D0000	; SAB	D0000
; 2829: fin
C0877:
; 2830: if inref
	DB	$68,<D0000,>D0000	; LAB	D0000
	DB	$4C,<C0878,>C0878	; SKPFLS	C0878
; 2831:     drop newline_31(inref, $7F, $0D)
	DB	$68,<D0000,>D0000	; LAB	D0000
	DB	$2A,$7F			; CB	127
	DB	$2A,$0D			; CB	13
	DB	$54,<C0044,>C0044	; CALL	C0044
	DB	$30			; DROP
; 2832:     if parse_module_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$4C,<C0880,>C0880	; SKPFLS	C0880
; 2833:         drop close_11(inref)
	DB	$68,<D0000,>D0000	; LAB	D0000
	DB	$54,<C0034,>C0034	; CALL	C0034
	DB	$30			; DROP
; 2834:         exec(entrypoint)
	DB	$6A,<D0016,>D0016	; LAW	D0016
	DB	$54,<C0869,>C0869	; CALL	C0869
; 2835: 		;
; 2836: 		; Close all files
; 2837: 		;
; 2838: 		^$BFD8 = 0
	DB	$2C,$D8,$BF		; CW	49112
	DB	$00			; ZERO
	DB	$70			; SB
; 2839: 		drop close_11(0)
	DB	$00			; ZERO
	DB	$54,<C0034,>C0034	; CALL	C0034
	DB	$30			; DROP
; 2840:     else
	DB	$50,<C0881,>C0881	; SKIP	C0881
C0880:
; 2841:         drop close_11(inref)
	DB	$68,<D0000,>D0000	; LAB	D0000
	DB	$54,<C0034,>C0034	; CALL	C0034
	DB	$30			; DROP
; 2842:         crout()
	DB	$54,<C0046,>C0046	; CALL	C0046
; 2843:         prstr(@badfile)
	DB	$26,<D0089,>D0089	; LA	D0089
	DB	$54,<C0020,>C0020	; CALL	C0020
; 2844:     fin
C0881:
; 2845: 	cin()
	DB	$54,<C0018,>C0018	; CALL	C0018
; 2846: ;   crout
; 2847: ;   dumpsym(@idglobal_tbl, globals)
; 2848: ;   crout
; 2849: fin
C0878:
C0879:
; 2850: done
	DB	$5C			; RET
