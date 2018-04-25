	.INCLUDE	"plstub.s"
;    1: ;
;    2: ; Global constants
;    3: ;
;    4: const FALSE         = 0
					; FALSE = 0
;    5: const TRUE          = !FALSE
					; TRUE = -1
;    6: ;
;    7: ; Hardware constants
;    8: ;
;    9: const csw           = $0036
					; csw = 54
;   10: const speaker       = $C030
					; speaker = 49200
;   11: const showgraphics  = $C050
					; showgraphics = 49232
;   12: const showtext      = $C051
					; showtext = 49233
;   13: const showfull      = $C052
					; showfull = 49234
;   14: const showmix       = $C053
					; showmix = 49235
;   15: const showpage1     = $C054
					; showpage1 = 49236
;   16: const showpage2     = $C055
					; showpage2 = 49237
;   17: const showlores     = $C056
					; showlores = 49238
;   18: const showhires     = $C057
					; showhires = 49239
;   19: const pushbttn1     = $C061
					; pushbttn1 = 49249
;   20: const pushbttn2     = $C062
					; pushbttn2 = 49250
;   21: const pushbttn3     = $C063
					; pushbttn3 = 49251
;   22: const keyboard      = $C000
					; keyboard = 49152
;   23: const keystrobe     = $C010
					; keystrobe = 49168
;   24: const keyenter      = $8D
					; keyenter = 141
;   25: const keyspace      = $A0
					; keyspace = 160
;   26: const keyarrowup    = $8B
					; keyarrowup = 139
;   27: const keyarrowdown  = $8A
					; keyarrowdown = 138
;   28: const keyarrowleft  = $88
					; keyarrowleft = 136
;   29: const keyarrowright = $95
					; keyarrowright = 149
;   30: const keyescape     = $9B
					; keyescape = 155
;   31: const keyctrla      = $81
					; keyctrla = 129
;   32: const keyctrlb      = $82
					; keyctrlb = 130
;   33: const keyctrlc      = $83
					; keyctrlc = 131
;   34: const keyctrld      = $84
					; keyctrld = 132
;   35: const keyctrle      = $85
					; keyctrle = 133
;   36: const keyctrli      = $89
					; keyctrli = 137
;   37: const keyctrlk      = $8B
					; keyctrlk = 139
;   38: const keyctrll      = $8C
					; keyctrll = 140
;   39: const keyctrln      = $8E
					; keyctrln = 142
;   40: const keyctrlo      = $8F
					; keyctrlo = 143
;   41: const keyctrlp      = $90
					; keyctrlp = 144
;   42: const keyctrlq      = $91
					; keyctrlq = 145
;   43: const keyctrlr      = $92
					; keyctrlr = 146
;   44: const keyctrls      = $93
					; keyctrls = 147
;   45: const keyctrlt      = $94
					; keyctrlt = 148
;   46: const keyctrlu      = $95
					; keyctrlu = 149
;   47: const keyctrlv      = $96
					; keyctrlv = 150
;   48: const keyctrlw      = $97
					; keyctrlw = 151
;   49: const keyctrlx      = $98
					; keyctrlx = 152
;   50: const keyctrlz      = $9A
					; keyctrlz = 154
;   51: const keydelete     = $FF
					; keydelete = 255
;   52: const getbuff       = $01FF
					; getbuff = 511
;   53: const argbuff       = $2006
					; argbuff = 8198
;   54: word txtscrn[]      = $0400,$0480,$0500,$0580,$0600,$0680,$0700,$0780
D0000:					; txtscrn
	DW	$0400
	DW	$0480
	DW	$0500
	DW	$0580
	DW	$0600
	DW	$0680
	DW	$0700
	DW	$0780
;   55: word                = $0428,$04A8,$0528,$05A8,$0628,$06A8,$0728,$07A8
	DW	$0428
	DW	$04A8
	DW	$0528
	DW	$05A8
	DW	$0628
	DW	$06A8
	DW	$0728
	DW	$07A8
;   56: word                = $0450,$04D0,$0550,$05D0,$0650,$06D0,$0750,$07D0
	DW	$0450
	DW	$04D0
	DW	$0550
	DW	$05D0
	DW	$0650
	DW	$06D0
	DW	$0750
	DW	$07D0
;   57: ;
;   58: ; Data and text buffer constants
;   59: ;
;   60: const machid        = $BF98
					; machid = 49048
;   61: const maxlines      = 626
					; maxlines = 626
;   62: const maxfill       = 640
					; maxfill = 640
;   63: const iobuffer      = $0800
					; iobuffer = 2048
;   64: const databuff      = $0C00
					; databuff = 3072
;   65: const strlinbuf     = $1000
					; strlinbuf = 4096
;   66: const strheapmap    = $1500
					; strheapmap = 5376
;   67: const strheapmsz    = $70 ; = memory@16 bytes per bit map, 128 bytes per 8 bit map, 1K bytes per 8 byte map
					; strheapmsz = 112
;   68: const maxlnlen      = 79
					; maxlnlen = 79
;   69: const strheap       = $7000
					; strheap = 28672
;   70: const strheasz      = $3800
					; strheasz = 14336
;   71: const codebuff      = $A800
					; codebuff = 43008
;   72: const codebuffsz    = $1000
					; codebuffsz = 4096
;   73: const pgjmp         = 16
					; pgjmp = 16
;   74: const changed       = 1
					; changed = 1
;   75: const insmode       = 2
					; insmode = 2
;   76: const showcurs      = 4
					; showcurs = 4
;   77: const uppercase     = 8
					; uppercase = 8
;   78: const shiftlock     = 128
					; shiftlock = 128
;   79: ;
;   80: ; Editor variables
;   81: ;
;   82: byte nullstr[]      = ""
D0048:					; nullstr
	DB	$00
;   83: byte version[]      = "PLASMA ][ IDE VERSION 0.8 "
D0049:					; version
	DB	$1A
	DB	$50,$4C,$41,$53,$4D,$41,$20,$5D
	DB	$5B,$20,$49,$44,$45,$20,$56,$45
	DB	$52,$53,$49,$4F,$4E,$20,$30,$2E
	DB	$38,$20
;   84: byte errorstr[]     = "ERROR: $"
D0076:					; errorstr
	DB	$08
	DB	$45,$52,$52,$4F,$52,$3A,$20,$24
;   85: byte okstr[]        = "OK"
D0085:					; okstr
	DB	$02
	DB	$4F,$4B
;   86: byte perr
D0088:	DS	1			; perr
;   87: byte outofmem[]     = "OUT OF MEMORY!"
D0089:					; outofmem
	DB	$0E
	DB	$4F,$55,$54,$20,$4F,$46,$20,$4D
	DB	$45,$4D,$4F,$52,$59,$21
;   88: byte losechng[]     = "LOSE CHANGES TO FILE (Y/N)?"
D0104:					; losechng
	DB	$1B
	DB	$4C,$4F,$53,$45,$20,$43,$48,$41
	DB	$4E,$47,$45,$53,$20,$54,$4F,$20
	DB	$46,$49,$4C,$45,$20,$28,$59,$2F
	DB	$4E,$29,$3F
;   89: ;byte emiterr[]     = "EMIT CODE/DATA MISMATCH"
;   90: byte untitled[]     = "UNTITLED"
D0132:					; untitled
	DB	$08
	DB	$55,$4E,$54,$49,$54,$4C,$45,$44
;   91: byte txtfile[64]    = "UNTITLED.PLA"
D0141:					; txtfile
	DB	$0C
	DB	$55,$4E,$54,$49,$54,$4C,$45,$44
	DB	$2E,$50,$4C,$41
	DS	$33
;   92: byte flags          = 0
D0192:					; flags
	DB	$00
;   93: byte flash          = 0
D0193:					; flash
	DB	$00
;   94: byte cursx, cursy, scrnleft, curscol, underchr, curschr
D0194:	DS	1			; cursx
D0195:	DS	1			; cursy
D0196:	DS	1			; scrnleft
D0197:	DS	1			; curscol
D0198:	DS	1			; underchr
D0199:	DS	1			; curschr
;   95: word cursrow, scrntop, cursptr
D0200:	DS	2			; cursrow
D0202:	DS	2			; scrntop
D0204:	DS	2			; cursptr
;   96: word numlines       = 0
D0206:					; numlines
	DW	$0000
;   97: word cutbuf         = 0
D0208:					; cutbuf
	DW	$0000
;   98: word keyin_01
D0210:	DS	2			; keyin_01
;   99: ;
;  100: ; Predeclared functions
;  101: ;
;  102: func cmdmode
;  103: ;
;  104: ; Compiler variables
;  105: ;
;  106: ;
;  107: ; Tokens
;  108: ;
;  109: const ID_TKN            = $D6 ; V
					; ID_TKN = 214
;  110: const CHR_TKN           = $C3 ; C
					; CHR_TKN = 195
;  111: const INT_TKN           = $C9 ; I
					; INT_TKN = 201
;  112: const STR_TKN           = $D3 ; S
					; STR_TKN = 211
;  113: const EOL_TKN           = $02
					; EOL_TKN = 2
;  114: const EOF_TKN           = $01
					; EOF_TKN = 1
;  115: const ERR_TKN           = $00
					; ERR_TKN = 0
;  116: ;
;  117: ; Binary operand operators
;  118: ;
;  119: const SET_TKN           = $BD ; =
					; SET_TKN = 189
;  120: const SETLIST_TKN       = $B9 ; =,
					; SETLIST_TKN = 185
;  121: const ADD_TKN           = $AB ; +
					; ADD_TKN = 171
;  122: const SUB_TKN           = $AD ; -
					; SUB_TKN = 173
;  123: const MUL_TKN           = $AA ; *
					; MUL_TKN = 170
;  124: const DIV_TKN           = $AF ; /
					; DIV_TKN = 175
;  125: const MOD_TKN           = $A5 ; %
					; MOD_TKN = 165
;  126: const OR_TKN            = $BF ; ?
					; OR_TKN = 191
;  127: const EOR_TKN           = $DE ; ^
					; EOR_TKN = 222
;  128: const AND_TKN           = $A6 ; &
					; AND_TKN = 166
;  129: const SHR_TKN           = $D2 ; R
					; SHR_TKN = 210
;  130: const SHL_TKN           = $CC ; L
					; SHL_TKN = 204
;  131: const GT_TKN            = $BE ; >
					; GT_TKN = 190
;  132: const GE_TKN            = $C8 ; H
					; GE_TKN = 200
;  133: const LT_TKN            = $BC ; <
					; LT_TKN = 188
;  134: const LE_TKN            = $C2 ; B
					; LE_TKN = 194
;  135: const NE_TKN            = $D5 ; U
					; NE_TKN = 213
;  136: const EQ_TKN            = $C5 ; E
					; EQ_TKN = 197
;  137: const LOGIC_AND_TKN     = $CE ; N
					; LOGIC_AND_TKN = 206
;  138: const LOGIC_OR_TKN      = $CF ; O
					; LOGIC_OR_TKN = 207
;  139: ;
;  140: ; Unary operand operators
;  141: ;
;  142: const AT_TKN            = $C0 ; @
					; AT_TKN = 192
;  143: const DOT_TKN           = $AE ; .
					; DOT_TKN = 174
;  144: const COLON_TKN         = $BA ; :
					; COLON_TKN = 186
;  145: const NEG_TKN           = $AD ; -
					; NEG_TKN = 173
;  146: const COMP_TKN          = $A3 ; #
					; COMP_TKN = 163
;  147: const LOGIC_NOT_TKN     = $A1 ; !
					; LOGIC_NOT_TKN = 161
;  148: const BPTR_TKN          = $DE ; ^
					; BPTR_TKN = 222
;  149: const WPTR_TKN          = $AA ; *
					; WPTR_TKN = 170
;  150: const INC_TKN           = $C1 ; A
					; INC_TKN = 193
;  151: const DEC_TKN           = $C4 ; D
					; DEC_TKN = 196
;  152: ;
;  153: ; Enclosure tokens
;  154: ;
;  155: const OPEN_PAREN_TKN    = $A8 ; (
					; OPEN_PAREN_TKN = 168
;  156: const CLOSE_PAREN_TKN   = $A9 ; )
					; CLOSE_PAREN_TKN = 169
;  157: const OPEN_BRACKET_TKN  = $DB ; [
					; OPEN_BRACKET_TKN = 219
;  158: const CLOSE_BRACKET_TKN = $DD ; ]
					; CLOSE_BRACKET_TKN = 221
;  159: ;
;  160: ; Misc. tokens
;  161: ;
;  162: const COMMA_TKN         = $AC ; ,
					; COMMA_TKN = 172
;  163: const COMMENT_TKN       = $BB ; ;
					; COMMENT_TKN = 187
;  164: ;
;  165: ; Keyword tokens
;  166: ;
;  167: const CONST_TKN         = $80
					; CONST_TKN = 128
;  168: const BYTE_TKN          = $81
					; BYTE_TKN = 129
;  169: const WORD_TKN          = $82
					; WORD_TKN = 130
;  170: const IF_TKN            = $83
					; IF_TKN = 131
;  171: const ELSEIF_TKN        = $84
					; ELSEIF_TKN = 132
;  172: const ELSE_TKN          = $85
					; ELSE_TKN = 133
;  173: const FIN_TKN           = $86
					; FIN_TKN = 134
;  174: const END_TKN           = $87
					; END_TKN = 135
;  175: const WHILE_TKN         = $88
					; WHILE_TKN = 136
;  176: const LOOP_TKN          = $89
					; LOOP_TKN = 137
;  177: const CASE_TKN          = $8A
					; CASE_TKN = 138
;  178: const OF_TKN            = $8B
					; OF_TKN = 139
;  179: const DEFAULT_TKN       = $8C
					; DEFAULT_TKN = 140
;  180: const ENDCASE_TKN       = $8D
					; ENDCASE_TKN = 141
;  181: const FOR_TKN           = $8E
					; FOR_TKN = 142
;  182: const TO_TKN            = $8F
					; TO_TKN = 143
;  183: const DOWNTO_TKN        = $90
					; DOWNTO_TKN = 144
;  184: const STEP_TKN          = $91
					; STEP_TKN = 145
;  185: const NEXT_TKN          = $92
					; NEXT_TKN = 146
;  186: const REPEAT_TKN        = $93
					; REPEAT_TKN = 147
;  187: const UNTIL_TKN         = $94
					; UNTIL_TKN = 148
;  188: const IFUNC_TKN         = $95
					; IFUNC_TKN = 149
;  189: const NFUNC_TKN         = $96
					; NFUNC_TKN = 150
;  190: const DROP_TKN          = $97
					; DROP_TKN = 151
;  191: const DONE_TKN          = $98
					; DONE_TKN = 152
;  192: const RETURN_TKN        = $99
					; RETURN_TKN = 153
;  193: const BREAK_TKN         = $9A
					; BREAK_TKN = 154
;  194: const START_TKN         = $9B
					; START_TKN = 155
;  195: const EXIT_TKN          = $9C
					; EXIT_TKN = 156
;  196: const EVAL_TKN          = $9D
					; EVAL_TKN = 157
;  197: const FUNC_TKN          = $9E
					; FUNC_TKN = 158
;  198: ;
;  199: ; Types
;  200: ;
;  201: const CONST_TYPE        = $01
					; CONST_TYPE = 1
;  202: const BYTE_TYPE         = $02
					; BYTE_TYPE = 2
;  203: const WORD_TYPE         = $04
					; WORD_TYPE = 4
;  204: const VAR_TYPE          = $06 ; (WORD_TYPE | BYTE_TYPE)
					; VAR_TYPE = 6
;  205: const FUNC_TYPE         = $08
					; FUNC_TYPE = 8
;  206: const FUNC_CONST_TYPE   = $09
					; FUNC_CONST_TYPE = 9
;  207: const ADDR_TYPE         = $0E ; (VAR_TYPE | FUNC_TYPE)
					; ADDR_TYPE = 14
;  208: const LOCAL_TYPE        = $10
					; LOCAL_TYPE = 16
;  209: const BPTR_TYPE         = $20
					; BPTR_TYPE = 32
;  210: const WPTR_TYPE         = $40
					; WPTR_TYPE = 64
;  211: const PTR_TYPE          = $60 ; (BPTR_TYPE | WPTR_TYPE)
					; PTR_TYPE = 96
;  212: const XBYTE_TYPE        = $22 ; (BPTR_TYPE | BYTE_TYPE)
					; XBYTE_TYPE = 34
;  213: const XWORD_TYPE        = $44 ; (WPTR_TYPE | WORD_TYPE)
					; XWORD_TYPE = 68
;  214: const STR_TYPE          = $80
					; STR_TYPE = 128
;  215: ;
;  216: ; Keywords
;  217: ;
;  218: byte keywrds[]
D0212:					; keywrds
;  219: byte                = "IF",     IF_TKN
	DB	$02
	DB	$49,$46
	DB	$83
;  220: byte                = "TO",     TO_TKN
	DB	$02
	DB	$54,$4F
	DB	$8F
;  221: byte                = "IS",     OF_TKN
	DB	$02
	DB	$49,$53
	DB	$8B
;  222: byte                = "OR",     LOGIC_OR_TKN
	DB	$02
	DB	$4F,$52
	DB	$CF
;  223: byte                = "FOR",    FOR_TKN
	DB	$03
	DB	$46,$4F,$52
	DB	$8E
;  224: byte                = "FIN",    FIN_TKN
	DB	$03
	DB	$46,$49,$4E
	DB	$86
;  225: byte                = "DEF",    IFUNC_TKN
	DB	$03
	DB	$44,$45,$46
	DB	$95
;  226: byte                = "END",    END_TKN
	DB	$03
	DB	$45,$4E,$44
	DB	$87
;  227: byte                = "AND",    LOGIC_AND_TKN
	DB	$03
	DB	$41,$4E,$44
	DB	$CE
;  228: byte                = "NOT",    LOGIC_NOT_TKN
	DB	$03
	DB	$4E,$4F,$54
	DB	$A1
;  229: byte                = "BYTE",   BYTE_TKN
	DB	$04
	DB	$42,$59,$54,$45
	DB	$81
;  230: byte                = "WORD",   WORD_TKN
	DB	$04
	DB	$57,$4F,$52,$44
	DB	$82
;  231: byte                = "DROP",   DROP_TKN
	DB	$04
	DB	$44,$52,$4F,$50
	DB	$97
;  232: byte                = "ELSE",   ELSE_TKN
	DB	$04
	DB	$45,$4C,$53,$45
	DB	$85
;  233: byte                = "NEXT",   NEXT_TKN
	DB	$04
	DB	$4E,$45,$58,$54
	DB	$92
;  234: byte                = "WHEN",   CASE_TKN
	DB	$04
	DB	$57,$48,$45,$4E
	DB	$8A
;  235: byte                = "LOOP",   LOOP_TKN
	DB	$04
	DB	$4C,$4F,$4F,$50
	DB	$89
;  236: byte                = "FUNC",   FUNC_TKN
	DB	$04
	DB	$46,$55,$4E,$43
	DB	$9E
;  237: byte                = "STEP",   STEP_TKN
	DB	$04
	DB	$53,$54,$45,$50
	DB	$91
;  238: byte                = "EXIT",   EXIT_TKN
	DB	$04
	DB	$45,$58,$49,$54
	DB	$9C
;  239: byte                = "DONE",   DONE_TKN
	DB	$04
	DB	$44,$4F,$4E,$45
	DB	$98
;  240: byte                = "WEND",   ENDCASE_TKN
	DB	$04
	DB	$57,$45,$4E,$44
	DB	$8D
;  241: byte                = "CONST",  CONST_TKN
	DB	$05
	DB	$43,$4F,$4E,$53,$54
	DB	$80
;  242: byte                = "ELSIF",  ELSEIF_TKN
	DB	$05
	DB	$45,$4C,$53,$49,$46
	DB	$84
;  243: byte                = "WHILE",  WHILE_TKN
	DB	$05
	DB	$57,$48,$49,$4C,$45
	DB	$88
;  244: byte                = "UNTIL",  UNTIL_TKN
	DB	$05
	DB	$55,$4E,$54,$49,$4C
	DB	$94
;  245: byte                = "BREAK",  BREAK_TKN
	DB	$05
	DB	$42,$52,$45,$41,$4B
	DB	$9A
;  246: byte                = "OTHER",  DEFAULT_TKN
	DB	$05
	DB	$4F,$54,$48,$45,$52
	DB	$8C
;  247: byte                = "DOWNTO", DOWNTO_TKN
	DB	$06
	DB	$44,$4F,$57,$4E,$54,$4F
	DB	$90
;  248: byte                = "REPEAT", REPEAT_TKN
	DB	$06
	DB	$52,$45,$50,$45,$41,$54
	DB	$93
;  249: byte                = "DEFOPT", NFUNC_TKN
	DB	$06
	DB	$44,$45,$46,$4F,$50,$54
	DB	$96
;  250: byte                = "RETURN", RETURN_TKN
	DB	$06
	DB	$52,$45,$54,$55,$52,$4E
	DB	$99
;  251: byte                = $FF
	DB	$FF
;  252: ;
;  253: ; Mathematical ops
;  254: ;
;  255: const bops_tblsz = 18 ; minus 1
					; bops_tblsz = 18
;  256: byte bops_tbl[]     ; Highest precedence
D0405:					; bops_tbl
;  257: byte                = MUL_TKN, DIV_TKN, MOD_TKN
	DB	$AA
	DB	$AF
	DB	$A5
;  258: byte                = ADD_TKN, SUB_TKN
	DB	$AB
	DB	$AD
;  259: byte                = SHR_TKN, SHL_TKN
	DB	$D2
	DB	$CC
;  260: byte                = AND_TKN
	DB	$A6
;  261: byte                = EOR_TKN
	DB	$DE
;  262: byte                = OR_TKN
	DB	$BF
;  263: byte                = GT_TKN, GE_TKN, LT_TKN, LE_TKN
	DB	$BE
	DB	$C8
	DB	$BC
	DB	$C2
;  264: byte                = EQ_TKN, NE_TKN
	DB	$C5
	DB	$D5
;  265: byte                = LOGIC_AND_TKN
	DB	$CE
;  266: byte                = LOGIC_OR_TKN
	DB	$CF
;  267: byte                = COMMA_TKN
	DB	$AC
;  268:                     ; Lowest precedence
;  269: byte bops_prec[]    ; Highest precedence
D0424:					; bops_prec
;  270: byte                = 1, 1, 1
	DB	$01
	DB	$01
	DB	$01
;  271: byte                = 2, 2
	DB	$02
	DB	$02
;  272: byte                = 3, 3
	DB	$03
	DB	$03
;  273: byte                = 4
	DB	$04
;  274: byte                = 5
	DB	$05
;  275: byte                = 6
	DB	$06
;  276: byte                = 7, 7, 7, 7
	DB	$07
	DB	$07
	DB	$07
	DB	$07
;  277: byte                = 8, 8
	DB	$08
	DB	$08
;  278: byte                = 9
	DB	$09
;  279: byte                = 10
	DB	$0A
;  280: byte                = 11
	DB	$0B
;  281:                     ; Lowest precedence
;  282: byte opstack[16]
D0443:	DS	16			; opstack
;  283: byte precstack[16]
D0459:	DS	16			; precstack
;  284: word opsp = -1
D0475:					; opsp
	DW	$FFFF
;  285: ;
;  286: ; Symbol table variables
;  287: ;
;  288: const idglobal_tblsz        = 2048
					; idglobal_tblsz = 2048
;  289: const idlocal_tblsz         = 512
					; idlocal_tblsz = 512
;  290: const idglobal_tbl          = $1600
					; idglobal_tbl = 5632
;  291: const idlocal_tbl           = $1E00
					; idlocal_tbl = 7680
;  292: const ctag_max              = 640
					; ctag_max = 640
;  293: const ctag_value            = $800
					; ctag_value = 2048
;  294: const ctag_flags            = $D80
					; ctag_flags = 3456
;  295: const idval                 = 0
					; idval = 0
;  296: const idtype                = 2
					; idtype = 2
;  297: const idname                = 3
					; idname = 3
;  298: const idrecsz               = 4
					; idrecsz = 4
;  299: word globals                = 0
D0477:					; globals
	DW	$0000
;  300: word datasize               = 0
D0479:					; datasize
	DW	$0000
;  301: word lastglobal
D0481:	DS	2			; lastglobal
;  302: byte locals                 = 0
D0483:					; locals
	DB	$00
;  303: word framesize              = 0
D0484:					; framesize
	DW	$0000
;  304: word lastlocal
D0486:	DS	2			; lastlocal
;  305: const resolved              = 1
					; resolved = 1
;  306: const is_ctag               = $8000
					; is_ctag = 32768
;  307: const mask_ctag             = $7FFF
					; mask_ctag = 32767
;  308: word codetag                = -1
D0488:					; codetag
	DW	$FFFF
;  309: word codeptr, entrypoint    = 0
D0490:	DS	2			; codeptr
D0492:					; entrypoint
	DW	$0000
;  310: byte lastop                 = $FF
D0494:					; lastop
	DB	$FF
;  311: ;
;  312: ; Scanner variables
;  313: ;
;  314: const inbuff = $0200
					; inbuff = 512
;  315: const instr  = $01FF
					; instr = 511
;  316: byte  token, tknlen
D0495:	DS	1			; token
D0496:	DS	1			; tknlen
;  317: byte  parserrpos, parserr = 0
D0497:	DS	1			; parserrpos
D0498:					; parserr
	DB	$00
;  318: word  scanptr, tknptr, parserrln
D0499:	DS	2			; scanptr
D0501:	DS	2			; tknptr
D0503:	DS	2			; parserrln
;  319: word  constval
D0505:	DS	2			; constval
;  320: word  lineno = 0
D0507:					; lineno
	DW	$0000
;  321: ;
;  322: ; Compiler output messages
;  323: ;
;  324: byte entrypt_str[]          = "START: "
D0509:					; entrypt_str
	DB	$07
	DB	$53,$54,$41,$52,$54,$3A,$20
;  325: byte comp_ok_msg[]          = "COMPILATION COMPLETE"
D0517:					; comp_ok_msg
	DB	$14
	DB	$43,$4F,$4D,$50,$49,$4C,$41,$54
	DB	$49,$4F,$4E,$20,$43,$4F,$4D,$50
	DB	$4C,$45,$54,$45
;  326: byte dup_id[]               = "DUPLICATE IDENTIFIER"
D0538:					; dup_id
	DB	$14
	DB	$44,$55,$50,$4C,$49,$43,$41,$54
	DB	$45,$20,$49,$44,$45,$4E,$54,$49
	DB	$46,$49,$45,$52
;  327: byte undecl_id[]            = "UNDECLARED IDENTIFIER"
D0559:					; undecl_id
	DB	$15
	DB	$55,$4E,$44,$45,$43,$4C,$41,$52
	DB	$45,$44,$20,$49,$44,$45,$4E,$54
	DB	$49,$46,$49,$45,$52
;  328: byte bad_cnst[]             = "BAD CONSTANT"
D0581:					; bad_cnst
	DB	$0C
	DB	$42,$41,$44,$20,$43,$4F,$4E,$53
	DB	$54,$41,$4E,$54
;  329: byte bad_offset[]           = "BAD STRUCT OFFSET"
D0594:					; bad_offset
	DB	$11
	DB	$42,$41,$44,$20,$53,$54,$52,$55
	DB	$43,$54,$20,$4F,$46,$46,$53,$45
	DB	$54
;  330: byte bad_decl[]             = "BAD DECLARATION"
D0612:					; bad_decl
	DB	$0F
	DB	$42,$41,$44,$20,$44,$45,$43,$4C
	DB	$41,$52,$41,$54,$49,$4F,$4E
;  331: byte bad_op[]               = "BAD OPERATION"
D0628:					; bad_op
	DB	$0D
	DB	$42,$41,$44,$20,$4F,$50,$45,$52
	DB	$41,$54,$49,$4F,$4E
;  332: byte bad_stmnt[]            = "BAD STATMENT"
D0642:					; bad_stmnt
	DB	$0C
	DB	$42,$41,$44,$20,$53,$54,$41,$54
	DB	$4D,$45,$4E,$54
;  333: byte bad_expr[]             = "BAD EXPRESSION"
D0655:					; bad_expr
	DB	$0E
	DB	$42,$41,$44,$20,$45,$58,$50,$52
	DB	$45,$53,$53,$49,$4F,$4E
;  334: byte bad_syntax[]           = "BAD SYNTAX"
D0670:					; bad_syntax
	DB	$0A
	DB	$42,$41,$44,$20,$53,$59,$4E,$54
	DB	$41,$58
;  335: byte estk_overflw[]         = "EVAL STACK OVERFLOW"
D0681:					; estk_overflw
	DB	$13
	DB	$45,$56,$41,$4C,$20,$53,$54,$41
	DB	$43,$4B,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  336: byte estk_underflw[]        = "EVAL STACK UNDERFLOW"
D0701:					; estk_underflw
	DB	$14
	DB	$45,$56,$41,$4C,$20,$53,$54,$41
	DB	$43,$4B,$20,$55,$4E,$44,$45,$52
	DB	$46,$4C,$4F,$57
;  337: byte local_overflw[]        = "LOCAL FRAME OVERFLOW"
D0722:					; local_overflw
	DB	$14
	DB	$4C,$4F,$43,$41,$4C,$20,$46,$52
	DB	$41,$4D,$45,$20,$4F,$56,$45,$52
	DB	$46,$4C,$4F,$57
;  338: byte global_sym_overflw[]   = "GLOBAL SYMBOL TABLE OVERFLOW"
D0743:					; global_sym_overflw
	DB	$1C
	DB	$47,$4C,$4F,$42,$41,$4C,$20,$53
	DB	$59,$4D,$42,$4F,$4C,$20,$54,$41
	DB	$42,$4C,$45,$20,$4F,$56,$45,$52
	DB	$46,$4C,$4F,$57
;  339: byte local_sym_overflw[]    = "LOCAL SYMBOL TABLE OVERFLOW"
D0772:					; local_sym_overflw
	DB	$1B
	DB	$4C,$4F,$43,$41,$4C,$20,$53,$59
	DB	$4D,$42,$4F,$4C,$20,$54,$41,$42
	DB	$4C,$45,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  340: byte ctag_full[]            = "CODE LABEL OVERFLOW"
D0800:					; ctag_full
	DB	$13
	DB	$43,$4F,$44,$45,$20,$4C,$41,$42
	DB	$45,$4C,$20,$4F,$56,$45,$52,$46
	DB	$4C,$4F,$57
;  341: byte no_close_paren[]       = "MISSING CLOSING PAREN"
D0820:					; no_close_paren
	DB	$15
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$43,$4C,$4F,$53,$49,$4E,$47,$20
	DB	$50,$41,$52,$45,$4E
;  342: byte no_close_bracket[]     = "MISSING CLOSING BRACKET"
D0842:					; no_close_bracket
	DB	$17
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$43,$4C,$4F,$53,$49,$4E,$47,$20
	DB	$42,$52,$41,$43,$4B,$45,$54
;  343: byte missing_op[]           = "MISSING OPERAND"
D0866:					; missing_op
	DB	$0F
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$4F,$50,$45,$52,$41,$4E,$44
;  344: byte no_fin[]               = "MISSING FIN"
D0882:					; no_fin
	DB	$0B
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$46,$49,$4E
;  345: byte no_loop[]              = "MISSING LOOP"
D0894:					; no_loop
	DB	$0C
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$4C,$4F,$4F,$50
;  346: byte no_until[]             = "MISSING UNTIL"
D0907:					; no_until
	DB	$0D
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$55,$4E,$54,$49,$4C
;  347: byte no_done[]              = "MISSING DONE"
D0921:					; no_done
	DB	$0C
	DB	$4D,$49,$53,$53,$49,$4E,$47,$20
	DB	$44,$4F,$4E,$45
;  348: byte no_local_init[]        = "NO INITIALIZED LOCALS"
D0934:					; no_local_init
	DB	$15
	DB	$4E,$4F,$20,$49,$4E,$49,$54,$49
	DB	$41,$4C,$49,$5A,$45,$44,$20,$4C
	DB	$4F,$43,$41,$4C,$53
;  349: ;
;  350: ; Runtime messages
;  351: ;
;  352: byte brkmsg[] 		= "CTRL-C BREAK"
D0956:					; brkmsg
	DB	$0C
	DB	$43,$54,$52,$4C,$2D,$43,$20,$42
	DB	$52,$45,$41,$4B
;  353: byte stkovflwmsg[] 	= "STACK OVERFLOW/UNDERFLOW ERROR"
D0969:					; stkovflwmsg
	DB	$1E
	DB	$53,$54,$41,$43,$4B,$20,$4F,$56
	DB	$45,$52,$46,$4C,$4F,$57,$2F,$55
	DB	$4E,$44,$45,$52,$46,$4C,$4F,$57
	DB	$20,$45,$52,$52,$4F,$52
;  354: ;
;  355: ; Runtime functions
;  356: ;
;  357: byte runtime0[]         = "romcall"
D1000:					; runtime0
	DB	$07
	DB	$72,$6F,$6D,$63,$61,$6C,$6C
;  358: byte RUNTIME0[]         = "ROMCALL"
D1008:					; RUNTIME0
	DB	$07
	DB	$52,$4F,$4D,$43,$41,$4C,$4C
;  359: byte runtime1[]         = "syscall"
D1016:					; runtime1
	DB	$07
	DB	$73,$79,$73,$63,$61,$6C,$6C
;  360: byte RUNTIME1[]         = "SYSCALL"
D1024:					; RUNTIME1
	DB	$07
	DB	$53,$59,$53,$43,$41,$4C,$4C
;  361: byte runtime2[]         = "memset"
D1032:					; runtime2
	DB	$06
	DB	$6D,$65,$6D,$73,$65,$74
;  362: byte RUNTIME2[]         = "MEMSET"
D1039:					; RUNTIME2
	DB	$06
	DB	$4D,$45,$4D,$53,$45,$54
;  363: byte runtime3[]         = "memcpy"
D1046:					; runtime3
	DB	$06
	DB	$6D,$65,$6D,$63,$70,$79
;  364: byte RUNTIME3[]         = "MEMCPY"
D1053:					; RUNTIME3
	DB	$06
	DB	$4D,$45,$4D,$43,$50,$59
;  365: byte runtime4[]         = "cout"
D1060:					; runtime4
	DB	$04
	DB	$63,$6F,$75,$74
;  366: byte RUNTIME4[]         = "COUT"
D1065:					; RUNTIME4
	DB	$04
	DB	$43,$4F,$55,$54
;  367: byte runtime5[]         = "cin"
D1070:					; runtime5
	DB	$03
	DB	$63,$69,$6E
;  368: byte RUNTIME5[]         = "CIN"
D1074:					; RUNTIME5
	DB	$03
	DB	$43,$49,$4E
;  369: byte runtime6[]         = "prstr"
D1078:					; runtime6
	DB	$05
	DB	$70,$72,$73,$74,$72
;  370: byte RUNTIME6[]         = "PRSTR"
D1084:					; RUNTIME6
	DB	$05
	DB	$50,$52,$53,$54,$52
;  371: byte runtime7[]         = "rdstr"
D1090:					; runtime7
	DB	$05
	DB	$72,$64,$73,$74,$72
;  372: byte RUNTIME7[]         = "RDSTR"
D1096:					; RUNTIME7
	DB	$05
	DB	$52,$44,$53,$54,$52
;  373: ;
;  374: ; Parser variables
;  375: ;
;  376: byte infunc             = 0
D1102:					; infunc
	DB	$00
;  377: byte stack_loop         = 0
D1103:					; stack_loop
	DB	$00
;  378: byte prevstmnt          = 0
D1104:					; prevstmnt
	DB	$00
;  379: word retfunc_tag        = 0
D1105:					; retfunc_tag
	DW	$0000
;  380: word break_tag          = 0
D1107:					; break_tag
	DW	$0000
;  381: func parse_expr_01, parse_module_01
;  382: ;
;  383: ; Utility functions
;  384: ;
;  385: ; Defines for ASM routines
;  386: ;
;  387: asm equates
C0003:					; equates()
;  388:         TMP     EQU     $F0
        TMP     EQU     $F0
;  389:         TMPL    EQU     TMP
        TMPL    EQU     TMP
;  390:         TMPH    EQU     TMP+1
        TMPH    EQU     TMP+1
;  391:         SRC     EQU     TMP
        SRC     EQU     TMP
;  392:         SRCL    EQU     SRC
        SRCL    EQU     SRC
;  393:         SRCH    EQU     SRC+1
        SRCH    EQU     SRC+1
;  394:         DST     EQU     SRC+2
        DST     EQU     SRC+2
;  395:         DSTL    EQU     DST
        DSTL    EQU     DST
;  396:         DSTH    EQU     DST+1
        DSTH    EQU     DST+1
;  397:         ESP     EQU     DST+2
        ESP     EQU     DST+2
;  398: 		SAVEESP	EQU		ESP+1
		SAVEESP	EQU		ESP+1
;  399: 		SAVESP	EQU		SAVEESP+1
		SAVESP	EQU		SAVEESP+1
;  400: 		SAVEFP	EQU		SAVESP+1
		SAVEFP	EQU		SAVESP+1
;  401: 		SAVETMR	EQU		SAVEFP+2
		SAVETMR	EQU		SAVEFP+2
;  402: 		SAVEINT	EQU		SAVETMR+2
		SAVEINT	EQU		SAVETMR+2
;  403: 		TMRVEC	EQU		$03E8
		TMRVEC	EQU		$03E8
;  404: 		INTVEC	EQU		$03EA
		INTVEC	EQU		$03EA
;  405: JMPTMP:	JMP		(TMP)
JMPTMP:	JMP		(TMP)
;  406: STKOVFLW:
STKOVFLW:
;  407: 		LDY		#$02
		LDY		#$02
;  408: 		JMP		EXECRET
		JMP		EXECRET
;  409: BRKCHK:
BRKCHK:
;  410: 		LDA		$C000
		LDA		$C000
;  411: 		CMP		#$83		; CTRL-C
		CMP		#$83		; CTRL-C
;  412: 		BNE		:+
		BNE		:+
;  413: 		BIT		$C010
		BIT		$C010
;  414: 		LDY		#$01
		LDY		#$01
;  415: 		JMP		EXECRET
		JMP		EXECRET
;  416: :
:
;  417: end
	RTS
;  418: ;
;  419: ; ENTER MODULE UNDER TEST
;  420: ;
;  421: asm execentry
C0005:					; execentry()
;  422:         LDA     ESTKL,X
        LDA     ESTKL,X
;  423:         STA     TMPL
        STA     TMPL
;  424:         LDA     ESTKH,X
        LDA     ESTKH,X
;  425:         STA     TMPH
        STA     TMPH
;  426: 		STX		SAVEESP
		STX		SAVEESP
;  427: 		TSX
		TSX
;  428: 		STX		SAVESP
		STX		SAVESP
;  429: 		LDA		FRMPL
		LDA		FRMPL
;  430: 		STA		SAVEFP
		STA		SAVEFP
;  431: 		LDA		FRMPH
		LDA		FRMPH
;  432: 		STA		SAVEFP+1
		STA		SAVEFP+1
;  433: 		LDA		TMRVEC
		LDA		TMRVEC
;  434: 		STA		SAVETMR
		STA		SAVETMR
;  435: 		LDA		TMRVEC+1
		LDA		TMRVEC+1
;  436: 		STA		SAVETMR+1
		STA		SAVETMR+1
;  437: 		LDA		INTVEC
		LDA		INTVEC
;  438: 		STA		SAVEINT
		STA		SAVEINT
;  439: 		LDA		INTVEC+1
		LDA		INTVEC+1
;  440: 		STA		SAVEINT+1
		STA		SAVEINT+1
;  441: 		LDA		#<BRKCHK
		LDA		#<BRKCHK
;  442: 		STA		TMRVEC
		STA		TMRVEC
;  443: 		LDA		#>BRKCHK
		LDA		#>BRKCHK
;  444: 		STA		TMRVEC+1
		STA		TMRVEC+1
;  445: 		LDA		#<STKOVFLW
		LDA		#<STKOVFLW
;  446: 		STA		INTVEC
		STA		INTVEC
;  447: 		LDA		#>STKOVFLW
		LDA		#>STKOVFLW
;  448: 		STA		INTVEC+1
		STA		INTVEC+1
;  449: 		LDX		#ESTKSZ/2
		LDX		#ESTKSZ/2
;  450: 		JSR		JMPTMP
		JSR		JMPTMP
;  451: 		LDY		#$00
		LDY		#$00
;  452: EXECRET:
EXECRET:
;  453: 		STY		TMP
		STY		TMP
;  454: 		BIT		ROMIN
		BIT		ROMIN
;  455: 		BIT		$C054
		BIT		$C054
;  456: 		BIT		$C051
		BIT		$C051
;  457: 		BIT		$C058
		BIT		$C058
;  458: 		JSR		$FB39		; SET TEXT MODE
		JSR		$FB39		; SET TEXT MODE
;  459: 		BIT		LCBNK2
		BIT		LCBNK2
;  460: 		LDA		SAVEFP
		LDA		SAVEFP
;  461: 		STA		FRMPL
		STA		FRMPL
;  462: 		LDA		SAVEFP+1
		LDA		SAVEFP+1
;  463: 		STA		FRMPH
		STA		FRMPH
;  464: 		LDA		SAVETMR
		LDA		SAVETMR
;  465: 		STA		TMRVEC
		STA		TMRVEC
;  466: 		LDA		SAVETMR+1
		LDA		SAVETMR+1
;  467: 		STA		TMRVEC+1
		STA		TMRVEC+1
;  468: 		LDA		SAVEINT
		LDA		SAVEINT
;  469: 		STA		INTVEC
		STA		INTVEC
;  470: 		LDA		SAVEINT+1
		LDA		SAVEINT+1
;  471: 		STA		INTVEC+1
		STA		INTVEC+1
;  472: 		LDX		SAVESP
		LDX		SAVESP
;  473: 		TXS
		TXS
;  474: 		LDX		SAVEESP
		LDX		SAVEESP
;  475: 		LDY		TMP
		LDY		TMP
;  476: 		STY		ESTKL,X
		STY		ESTKL,X
;  477: 		LDY		#$00
		LDY		#$00
;  478: 		STY		ESTKH,X
		STY		ESTKH,X
;  479: end
	RTS
;  480: ;
;  481: ; CALL 6502 ROUTINE
;  482: ; ROMCALL(AREG, XREG, YREG, STATUS, ADDR)
;  483: ;
;  484: asm romcall
C0007:					; romcall()
;  485:         PHP
        PHP
;  486:         LDA     ESTKL,X
        LDA     ESTKL,X
;  487:         STA     TMPL
        STA     TMPL
;  488:         LDA     ESTKH,X
        LDA     ESTKH,X
;  489:         STA     TMPH
        STA     TMPH
;  490:         INX
        INX
;  491:         LDA     ESTKL,X
        LDA     ESTKL,X
;  492:         PHA
        PHA
;  493:         INX
        INX
;  494:         LDA     ESTKL,X
        LDA     ESTKL,X
;  495:         TAY
        TAY
;  496:         INX
        INX
;  497:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  498:         PHA
        PHA
;  499:         LDA     ESTKL,X
        LDA     ESTKL,X
;  500:         INX
        INX
;  501:         STX     ESP
        STX     ESP
;  502:         TAX
        TAX
;  503:         PLA
        PLA
;  504:         BIT     ROMIN
        BIT     ROMIN
;  505:         PLP
        PLP
;  506:         JSR     JMPTMP
        JSR     JMPTMP
;  507:         PHP
        PHP
;  508:         BIT     LCBNK2
        BIT     LCBNK2
;  509:         STA     REGVALS+0
        STA     REGVALS+0
;  510:         STX     REGVALS+1
        STX     REGVALS+1
;  511:         STY     REGVALS+2
        STY     REGVALS+2
;  512:         PLA
        PLA
;  513:         STA     REGVALS+3
        STA     REGVALS+3
;  514:         LDX     ESP
        LDX     ESP
;  515:         LDA     #<REGVALS
        LDA     #<REGVALS
;  516:         LDY     #>REGVALS
        LDY     #>REGVALS
;  517:         STA     ESTKL,X
        STA     ESTKL,X
;  518:         STY     ESTKH,X
        STY     ESTKH,X
;  519:         PLP
        PLP
;  520:         RTS
        RTS
;  521: REGVALS: DS 4
REGVALS: DS 4
;  522: end
	RTS
;  523: ;
;  524: ; CALL PRODOS
;  525: ; SYSCALL(CMD, PARAMS)
;  526: ;
;  527: asm syscall
C0009:					; syscall()
;  528:         LDA     ESTKL,X
        LDA     ESTKL,X
;  529:         LDY     ESTKH,X
        LDY     ESTKH,X
;  530:         STA     PARAMS
        STA     PARAMS
;  531:         STY     PARAMS+1
        STY     PARAMS+1
;  532:         INX
        INX
;  533:         LDA     ESTKL,X
        LDA     ESTKL,X
;  534:         STA     CMD
        STA     CMD
;  535:         STX     ESP
        STX     ESP
;  536:         JSR     $BF00
        JSR     $BF00
;  537: CMD:    DB      00
CMD:    DB      00
;  538: PARAMS: DW      0000
PARAMS: DW      0000
;  539:         BIT     LCBNK2
        BIT     LCBNK2
;  540:         LDX     ESP
        LDX     ESP
;  541:         STA     ESTKL,X
        STA     ESTKL,X
;  542:         LDY     #$00
        LDY     #$00
;  543:         STY     ESTKH,X
        STY     ESTKH,X
;  544: end
	RTS
;  545: ;
;  546: ; SET MEMORY TO VALUE
;  547: ; MEMSET(VALUE, ADDR, SIZE)
;  548: ;
;  549: asm memset
C0011:					; memset()
;  550:         LDY     #$00
        LDY     #$00
;  551:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  552:         STA     DSTL
        STA     DSTL
;  553:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  554:         STA     DSTH
        STA     DSTH
;  555:         INC     ESTKL,X
        INC     ESTKL,X
;  556:         INC     ESTKH,X
        INC     ESTKH,X
;  557: SETMEM: DEC     ESTKL,X
SETMEM: DEC     ESTKL,X
;  558:         BNE     :+
        BNE     :+
;  559:         DEC     ESTKH,X
        DEC     ESTKH,X
;  560:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  561: :       LDA     ESTKL+2,X
:       LDA     ESTKL+2,X
;  562:         STA     (DST),Y
        STA     (DST),Y
;  563:         INY
        INY
;  564:         BNE     :+
        BNE     :+
;  565:         INC     DSTH
        INC     DSTH
;  566: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  567:         BNE     :+
        BNE     :+
;  568:         DEC     ESTKH,X
        DEC     ESTKH,X
;  569:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  570: :       LDA     ESTKH+2,X
:       LDA     ESTKH+2,X
;  571:         STA     (DST),Y
        STA     (DST),Y
;  572:         INY
        INY
;  573:         BNE     SETMEM
        BNE     SETMEM
;  574:         INC     DSTH
        INC     DSTH
;  575:         BNE     SETMEM
        BNE     SETMEM
;  576: MEMEXIT: INX
MEMEXIT: INX
;  577:         INX
        INX
;  578:         INX
        INX
;  579: end
	RTS
;  580: ;
;  581: ; COPY MEMORY
;  582: ; MEMCPY(SRCADDR, DSTADDR, SIZE)
;  583: ;
;  584: asm memcpy
C0013:					; memcpy()
;  585:         LDY     #$00
        LDY     #$00
;  586:         LDA     ESTKL,X
        LDA     ESTKL,X
;  587:         BNE     :+
        BNE     :+
;  588:         LDA     ESTKH,X
        LDA     ESTKH,X
;  589:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  590: :       LDA     ESTKL+1,X
:       LDA     ESTKL+1,X
;  591:         STA     DSTL
        STA     DSTL
;  592:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  593:         STA     DSTH
        STA     DSTH
;  594:         LDA     ESTKL+2,X
        LDA     ESTKL+2,X
;  595:         STA     SRCL
        STA     SRCL
;  596:         LDA     ESTKH+2,X
        LDA     ESTKH+2,X
;  597:         STA     SRCH
        STA     SRCH
;  598:         CMP     DSTH
        CMP     DSTH
;  599:         BCC     REVCPY
        BCC     REVCPY
;  600:         BNE     FORCPY
        BNE     FORCPY
;  601:         LDA     SRCL
        LDA     SRCL
;  602:         CMP     DSTL
        CMP     DSTL
;  603:         BCS     FORCPY
        BCS     FORCPY
;  604: REVCPY:             ; REVERSE DIRECTION COPY
REVCPY:             ; REVERSE DIRECTION COPY
;  605: ;       CLC
;  606:         LDA     ESTKL,X
        LDA     ESTKL,X
;  607:         ADC     DSTL
        ADC     DSTL
;  608:         STA     DSTL
        STA     DSTL
;  609:         LDA     ESTKH,X
        LDA     ESTKH,X
;  610:         ADC     DSTH
        ADC     DSTH
;  611:         STA     DSTH
        STA     DSTH
;  612:         CLC
        CLC
;  613:         LDA     ESTKL,X
        LDA     ESTKL,X
;  614:         ADC     SRCL
        ADC     SRCL
;  615:         STA     SRCL
        STA     SRCL
;  616:         LDA     ESTKH,X
        LDA     ESTKH,X
;  617:         ADC     SRCH
        ADC     SRCH
;  618:         STA     SRCH
        STA     SRCH
;  619:         INC     ESTKH,X
        INC     ESTKH,X
;  620: REVCPYLP:
REVCPYLP:
;  621:         LDA     DSTL
        LDA     DSTL
;  622:         BNE     :+
        BNE     :+
;  623:         DEC     DSTH
        DEC     DSTH
;  624: :       DEC     DSTL
:       DEC     DSTL
;  625:         LDA     SRCL
        LDA     SRCL
;  626:         BNE     :+
        BNE     :+
;  627:         DEC     SRCH
        DEC     SRCH
;  628: :       DEC     SRCL
:       DEC     SRCL
;  629:         LDA     (SRC),Y
        LDA     (SRC),Y
;  630:         STA     (DST),Y
        STA     (DST),Y
;  631:         DEC     ESTKL,X
        DEC     ESTKL,X
;  632:         BNE     REVCPYLP
        BNE     REVCPYLP
;  633:         DEC     ESTKH,X
        DEC     ESTKH,X
;  634:         BNE     REVCPYLP
        BNE     REVCPYLP
;  635:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  636: FORCPY: INC     ESTKH,X
FORCPY: INC     ESTKH,X
;  637: FORCPYLP:
FORCPYLP:
;  638:         LDA     (SRC),Y
        LDA     (SRC),Y
;  639:         STA     (DST),Y
        STA     (DST),Y
;  640:         INC     DSTL
        INC     DSTL
;  641:         BNE     :+
        BNE     :+
;  642:         INC     DSTH
        INC     DSTH
;  643: :       INC     SRCL
:       INC     SRCL
;  644:         BNE     :+
        BNE     :+
;  645:         INC     SRCH
        INC     SRCH
;  646: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  647:         BNE     FORCPYLP
        BNE     FORCPYLP
;  648:         DEC     ESTKH,X
        DEC     ESTKH,X
;  649:         BNE     FORCPYLP
        BNE     FORCPYLP
;  650:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  651: end
	RTS
;  652: ;
;  653: ; CHAR OUT
;  654: ; COUT(CHAR)
;  655: ;
;  656: asm cout
C0015:					; cout()
;  657:         LDA     ESTKL,X
        LDA     ESTKL,X
;  658:         INX
        INX
;  659:         ORA     #$80
        ORA     #$80
;  660:         BIT     ROMIN
        BIT     ROMIN
;  661:         JSR     $FDED
        JSR     $FDED
;  662:         BIT     LCBNK2
        BIT     LCBNK2
;  663: end
	RTS
;  664: ;
;  665: ; CHAR IN
;  666: ; RDKEY()
;  667: ;
;  668: asm cin
C0017:					; cin()
;  669:         BIT     ROMIN
        BIT     ROMIN
;  670:         STX     ESP
        STX     ESP
;  671:         JSR     $FD0C
        JSR     $FD0C
;  672:         LDX     ESP
        LDX     ESP
;  673:         BIT     LCBNK2
        BIT     LCBNK2
;  674:         DEX
        DEX
;  675:         AND     #$7F
        AND     #$7F
;  676:         STA     ESTKL,X
        STA     ESTKL,X
;  677:         LDY     #$00
        LDY     #$00
;  678:         STY     ESTKH,X
        STY     ESTKH,X
;  679: end
	RTS
;  680: ;
;  681: ; PRINT STRING
;  682: ; PRSTR(STR)
;  683: ;
;  684: asm prstr
C0019:					; prstr()
;  685:         LDY     #$00
        LDY     #$00
;  686:         LDA     ESTKL,X
        LDA     ESTKL,X
;  687:         STA     SRCL
        STA     SRCL
;  688:         LDA     ESTKH,X
        LDA     ESTKH,X
;  689:         STA     SRCH
        STA     SRCH
;  690:         BIT     ROMIN
        BIT     ROMIN
;  691:         LDA     (SRC),Y
        LDA     (SRC),Y
;  692:         STA     ESTKL,X
        STA     ESTKL,X
;  693:         BEQ     :+
        BEQ     :+
;  694: _PRS1:  INY
_PRS1:  INY
;  695:         LDA     (SRC),Y
        LDA     (SRC),Y
;  696:         ORA     #$80
        ORA     #$80
;  697:         JSR     $FDED
        JSR     $FDED
;  698:         TYA
        TYA
;  699:         CMP     ESTKL,X
        CMP     ESTKL,X
;  700:         BNE     _PRS1
        BNE     _PRS1
;  701: :       INX
:       INX
;  702:         BIT     LCBNK2
        BIT     LCBNK2
;  703: end
	RTS
;  704: ;
;  705: ; READ STRING
;  706: ; STR = RDSTR(PROMPTCHAR)
;  707: ;
;  708: asm rdstr
C0021:					; rdstr()
;  709:         LDA     ESTKL,X
        LDA     ESTKL,X
;  710:         STA     $33
        STA     $33
;  711:         STX     ESP
        STX     ESP
;  712:         BIT     ROMIN
        BIT     ROMIN
;  713:         JSR     $FD6A
        JSR     $FD6A
;  714:         BIT     LCBNK2
        BIT     LCBNK2
;  715:         STX     $01FF
        STX     $01FF
;  716: :       LDA     $01FF,X
:       LDA     $01FF,X
;  717:         AND     #$7F
        AND     #$7F
;  718:         STA     $01FF,X
        STA     $01FF,X
;  719:         DEX
        DEX
;  720:         BPL     :-
        BPL     :-
;  721:         LDX     ESP
        LDX     ESP
;  722:         LDA     #$FF
        LDA     #$FF
;  723:         STA     ESTKL,X
        STA     ESTKL,X
;  724:         LDA     #$01
        LDA     #$01
;  725:         STA     ESTKH,X
        STA     ESTKH,X
;  726: end
	RTS
;  727: ;
;  728: ; EXIT
;  729: ;
;  730: asm exit
C0023:					; exit()
;  731:         JSR $BF00
        JSR $BF00
;  732:         DB  $65
        DB  $65
;  733:         DW  EXITTBL
        DW  EXITTBL
;  734: EXITTBL:
EXITTBL:
;  735:         DB  4
        DB  4
;  736:         DB  0
        DB  0
;  737: end
	RTS
;  738: ;
;  739: ; ProDOS routines
;  740: ;
;  741: def getpfx_11(path)
C0025:					; getpfx_11()
					; path = 2
;  742:     byte params[3]
					; params = 4
;  743: 
;  744:     ^path    = 0
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$70			; SB
;  745:     params.0 = 1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  746:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  747:     perr     = syscall($C7, @params)
	DB	$2A,$C7			; CB	199
	DB	$28,$04			; LLA	4
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  748:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  749: end
;  750: def setpfx_11(path)
C0027:					; setpfx_11()
					; path = 2
;  751:     byte params[3]
					; params = 4
;  752: 
;  753:     params.0 = 1
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  754:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  755:     perr     = syscall($C6, @params)
	DB	$2A,$C6			; CB	198
	DB	$28,$04			; LLA	4
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  756:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  757: end
;  758: def open_21(path, buff)
C0029:					; open_21()
					; path = 2
					; buff = 4
;  759:     byte params[6]
					; params = 6
;  760: 
;  761:     params.0 = 3
	JSR	INTERP
	DB	$58,$0C,$02		; ENTER	12,2
	DB	$28,$06			; LLA	6
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  762:     params:1 = path
	DB	$28,$07			; LLA	7
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  763:     params:3 = buff
	DB	$28,$09			; LLA	9
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  764:     params.5 = 0
	DB	$28,$0B			; LLA	11
	DB	$00			; ZERO
	DB	$70			; SB
;  765:     perr     = syscall($C8, @params)
	DB	$2A,$C8			; CB	200
	DB	$28,$06			; LLA	6
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  766:     return params.5
	DB	$28,$0B			; LLA	11
	DB	$60			; LB
	DB	$5A			; LEAVE
;  767: end
;  768: def close_11(refnum)
C0031:					; close_11()
					; refnum = 2
;  769:     byte params[2]
					; params = 4
;  770: 
;  771:     params.0 = 1
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  772:     params.1 = refnum
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  773:     perr     = syscall($CC, @params)
	DB	$2A,$CC			; CB	204
	DB	$28,$04			; LLA	4
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  774:     return perr
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$5A			; LEAVE
;  775: end
;  776: def read_31(refnum, buff, len)
C0033:					; read_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  777:     byte params[8]
					; params = 8
;  778: 
;  779:     params.0 = 4
	JSR	INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  780:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  781:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  782:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  783:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  784:     perr     = syscall($CA, @params)
	DB	$2A,$CA			; CB	202
	DB	$28,$08			; LLA	8
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  785:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  786: end
;  787: def write_31(refnum, buff, len)
C0035:					; write_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  788:     byte params[8]
					; params = 8
;  789: 
;  790:     params.0 = 4
	JSR	INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  791:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  792:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  793:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  794:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  795:     perr     = syscall($CB, @params)
	DB	$2A,$CB			; CB	203
	DB	$28,$08			; LLA	8
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  796:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  797: end
;  798: def create_41(path, access, type, aux)
C0037:					; create_41()
					; path = 2
					; access = 4
					; type = 6
					; aux = 8
;  799:     byte params[12]
					; params = 10
;  800: 
;  801:     params.0  = 7
	JSR	INTERP
	DB	$58,$16,$04		; ENTER	22,4
	DB	$28,$0A			; LLA	10
	DB	$2A,$07			; CB	7
	DB	$70			; SB
;  802:     params:1  = path
	DB	$28,$0B			; LLA	11
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  803:     params.3  = access
	DB	$28,$0D			; LLA	13
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  804:     params.4  = type
	DB	$28,$0E			; LLA	14
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  805:     params:5  = aux
	DB	$28,$0F			; LLA	15
	DB	$66,$08			; LLW	8
	DB	$72			; SW
;  806:     params.7  = $1
	DB	$28,$11			; LLA	17
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  807:     params:8  = 0
	DB	$28,$12			; LLA	18
	DB	$00			; ZERO
	DB	$72			; SW
;  808:     params:10 = 0
	DB	$28,$14			; LLA	20
	DB	$00			; ZERO
	DB	$72			; SW
;  809:     perr      = syscall($C0, @params)
	DB	$2A,$C0			; CB	192
	DB	$28,$0A			; LLA	10
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  810:     return perr
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$5A			; LEAVE
;  811: end
;  812: def destroy_11(path)
C0039:					; destroy_11()
					; path = 2
;  813:     byte params[12]
					; params = 4
;  814: 
;  815:     params.0 = 1
	JSR	INTERP
	DB	$58,$10,$01		; ENTER	16,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  816:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  817:     perr     = syscall($C1, @params)
	DB	$2A,$C1			; CB	193
	DB	$28,$04			; LLA	4
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  818:     return perr
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$5A			; LEAVE
;  819: end
;  820: def newline_31(refnum, emask, nlchar)
C0041:					; newline_31()
					; refnum = 2
					; emask = 4
					; nlchar = 6
;  821:     byte params[4]
					; params = 8
;  822: 
;  823:     params.0 = 3
	JSR	INTERP
	DB	$58,$0C,$03		; ENTER	12,3
	DB	$28,$08			; LLA	8
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  824:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  825:     params.2 = emask
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  826:     params.3 = nlchar
	DB	$28,$0B			; LLA	11
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  827:     perr     = syscall($C9, @params)
	DB	$2A,$C9			; CB	201
	DB	$28,$08			; LLA	8
	DB	$54,<C0009,>C0009	; CALL	C0009
	DB	$78,<D0088,>D0088	; SAB	D0088
;  828:     return perr
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$5A			; LEAVE
;  829: end
;  830: 
;  831: ;=====================================
;  832: ;
;  833: ;            Editor
;  834: ;
;  835: ;=====================================
;  836: 
;  837: def crout
C0043:					; crout()
;  838:     cout($0D)
	JSR	INTERP
	DB	$2A,$0D			; CB	13
	DB	$54,<C0015,>C0015	; CALL	C0015
;  839: end
	DB	$5C			; RET
;  840: def bell
C0045:					; bell()
;  841:     drop romcall(0, 0, 0, 0, $FBDD)
	JSR	INTERP
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DD,$FB		; CW	64477
	DB	$54,<C0007,>C0007	; CALL	C0007
	DB	$30			; DROP
;  842: end
	DB	$5C			; RET
;  843: ;
;  844: ; Memory management routines
;  845: ;
;  846: defopt strcpy_20(srcstr, dststr)
C0047:					; strcpy_20()
					; srcstr = 2
					; dststr = 4
;  847:     byte strlen
					; strlen = 6
;  848: 
;  849:     strlen = ^srcstr
	LDY	#7
	LDA	#2
	JSR	ENTER
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
;  850:     while (srcstr).[strlen] == $8D or (srcstr).[strlen] == $A0
	INX
C0049:
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
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$8D
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
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
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$A0
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	JSR	LOR
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0050
:
;  851:         strlen = strlen - 1
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
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
;  852:     loop
	INX
	JMP	C0049
C0050:
;  853:     ^dststr = strlen
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
	LDY	#$00
	STY	ESTKH,X
	JSR	SB
;  854:     memcpy(srcstr + 1, dststr + 1, strlen)
	DEX
	LDY	#$02
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
	JSR	ADD
	DEX
	LDY	#$04
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
	JSR	ADD
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0013
;  855: end
	JMP	LEAVE
;  856: defopt heapaddr_21(ofst, mask)
C0051:					; heapaddr_21()
					; ofst = 2
					; mask = 4
;  857:     word addr
					; addr = 6
;  858: 
;  859:     addr = (ofst << 7) + strheap
	LDY	#8
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
	LDA	#$07
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SHL
	DEX
	STY	ESTKL,X
	LDA	#$70
	STA	ESTKH,X
	JSR	ADD
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  860:     while !(mask & 1)
	INX
C0053:
	DEX
	LDY	#$04
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
	JSR	BAND
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0054
:
;  861:         addr = addr + 16
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$10
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  862:         mask = mask >> 1
	LDY	#$04
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
	JSR	SHR
	LDY	#$04
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  863:     loop
	INX
	JMP	C0053
C0054:
;  864:     return addr
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JMP	LEAVE
;  865: end
;  866: defopt sizemask_11(size)
C0055:					; sizemask_11()
					; size = 2
;  867:     if size <= 16
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
	DEX
	LDA	#$10
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ISLE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0057
:
;  868:         return $01
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  869:     elsif size <= 32
	JMP	C0058
C0057:
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$20
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ISLE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0059
:
;  870:         return $03
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  871:     elsif size <= 48
	JMP	C0058
C0059:
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$30
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ISLE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0060
:
;  872:         return $07
	DEX
	LDA	#$07
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  873:     elsif size <= 64
	JMP	C0058
C0060:
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$40
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ISLE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0061
:
;  874:         return $0F
	DEX
	LDA	#$0F
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  875:     elsif size <= 80
	JMP	C0058
C0061:
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDA	#$50
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ISLE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0062
:
;  876:         return $1F
	DEX
	LDA	#$1F
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  877:     fin
C0062:
C0058:
;  878:     return 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
;  879: end
;  880: defopt heapalloc_11(size)
C0063:					; heapalloc_11()
					; size = 2
;  881:     byte szmask, i
					; szmask = 4
					; i = 5
;  882:     word mapmask
					; mapmask = 6
;  883: 
;  884:     szmask = sizemask_11(size)
	LDY	#8
	LDA	#1
	JSR	ENTER
	DEX
	LDY	#$02
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	C0055
	LDY	#$04
	LDA	ESTKL,X
	STA	(FRMP),Y
;  885:     for i = strheapmsz - 1 downto 0
	LDA	#$70
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
C0066:
	LDY	#$05
	LDA	ESTKL,X
	STA	(FRMP),Y
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	INX
	LDA	ESTKL,X
	CMP	ESTKL-1,X
	LDA	ESTKH,X
	SBC	ESTKH-1,X
	BPL	:+
	JMP	C0065
:
	LDA	ESTKL,X
	BNE	:+
	DEC	ESTKH,X
:	DEC	ESTKL,X
;  886:         if strheapmap.[i] <> $FF
	DEX
	STY	ESTKL,X
	LDA	#$15
	STA	ESTKH,X
	DEX
	LDY	#$05
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDA	#$FF
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISNE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0067
:
;  887:             mapmask = szmask
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  888:             repeat
	INX
C0070:
;  889:                 if strheapmap.[i] & mapmask
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$15
	STA	ESTKH,X
	DEX
	LDY	#$05
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	BAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0071
:
;  890:                     mapmask = mapmask << 1
	DEX
	LDY	#$06
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
	JSR	SHL
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  891:                 else
	INX
	JMP	C0072
C0071:
;  892:                     strheapmap.[i] = strheapmap.[i] ? mapmask
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$15
	STA	ESTKH,X
	DEX
	LDY	#$05
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	DEX
	STY	ESTKL,X
	LDA	#$15
	STA	ESTKH,X
	DEX
	LDY	#$05
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	JSR	LB
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	IOR
	LDY	#$00
	JSR	SB
;  893:                     return heapaddr_21(i, mapmask)
	LDY	#$05
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	C0051
	JMP	LEAVE
;  894:                 fin
C0072:
;  895:             until mapmask & $100
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$01
	STA	ESTKH,X
	JSR	BAND
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0070
:
C0069:
;  896:         fin
C0067:
C0068:
;  897:     next
	JMP	C0066
C0065:
;  898:     bell()
	INX
	JSR	C0045
;  899:     prstr(@outofmem)
	DEX
	LDA	#<D0089
	STA	ESTKL,X
	LDA	#>D0089
	STA	ESTKH,X
	JSR	C0019
;  900:     return 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
;  901: end
;  902: def freestr_10(strptr)
C0073:					; freestr_10()
					; strptr = 2
;  903:     byte mask, ofst
					; mask = 4
					; ofst = 5
;  904: 
;  905:     if strptr and strptr <> @nullstr
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$42			; ISNE
	DB	$24			; LAND
	DB	$4C,<C0075,>C0075	; SKPFLS	C0075
;  906:         mask = sizemask_11(^strptr + 1)
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0055,>C0055	; CALL	C0055
	DB	$74,$04			; SLB	4
;  907:         ofst = (strptr - strheap) >> 4
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$70		; CW	28672
	DB	$04			; SUB
	DB	$2A,$04			; CB	4
	DB	$1C			; SHR
	DB	$74,$05			; SLB	5
;  908:         mask = mask << (ofst & $07)
	DB	$64,$04			; LLB	4
	DB	$64,$05			; LLB	5
	DB	$2A,$07			; CB	7
	DB	$14			; BAND
	DB	$1A			; SHL
	DB	$74,$04			; SLB	4
;  909:         ofst = ofst >> 3
	DB	$64,$05			; LLB	5
	DB	$2A,$03			; CB	3
	DB	$1C			; SHR
	DB	$74,$05			; SLB	5
;  910:         strheapmap.[ofst] = strheapmap.[ofst] & #mask
	DB	$2C,$00,$15		; CW	5376
	DB	$64,$05			; LLB	5
	DB	$02			; IDXB
	DB	$2C,$00,$15		; CW	5376
	DB	$64,$05			; LLB	5
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$64,$04			; LLB	4
	DB	$12			; COMP
	DB	$14			; BAND
	DB	$70			; SB
;  911:     fin
C0075:
C0076:
;  912: end
	DB	$5A			; LEAVE
;  913: def newstr_11(strptr)
C0077:					; newstr_11()
					; strptr = 2
;  914:     byte strlen
					; strlen = 4
;  915:     word newptr
					; newptr = 5
;  916: 
;  917:     strlen = ^strptr
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$74,$04			; SLB	4
;  918:     while (strptr).[strlen] == $8D or (strptr).[strlen] == $A0
C0079:
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$2A,$8D			; CB	141
	DB	$40			; ISEQ
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$2A,$A0			; CB	160
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0080,>C0080	; SKPFLS	C0080
;  919:         strlen = strlen - 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$04			; SLB	4
;  920:     loop
	DB	$50,<C0079,>C0079	; SKIP	C0079
C0080:
;  921:     if strlen == 0
	DB	$64,$04			; LLB	4
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0081,>C0081	; SKPFLS	C0081
;  922:         return @nullstr
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$5A			; LEAVE
;  923:     fin
C0081:
C0082:
;  924:     newptr = heapalloc_11(strlen + 1)
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0063,>C0063	; CALL	C0063
	DB	$76,$05			; SLW	5
;  925:     if newptr
	DB	$66,$05			; LLW	5
	DB	$4C,<C0083,>C0083	; SKPFLS	C0083
;  926:         memcpy(strptr, newptr, strlen + 1)
	DB	$66,$02			; LLW	2
	DB	$66,$05			; LLW	5
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0013,>C0013	; CALL	C0013
;  927:         ^newptr = strlen
	DB	$66,$05			; LLW	5
	DB	$64,$04			; LLB	4
	DB	$70			; SB
;  928:         return newptr
	DB	$66,$05			; LLW	5
	DB	$5A			; LEAVE
;  929:     fin
C0083:
C0084:
;  930:     return @nullstr
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$5A			; LEAVE
;  931: end
;  932: def inittxtbuf
C0085:					; inittxtbuf()
;  933:     word i
					; i = 2
;  934: 
;  935:     memset(0, strheapmap, strheapmsz)
	JSR	INTERP
	DB	$58,$04,$00		; ENTER	4,0
	DB	$00			; ZERO
	DB	$2C,$00,$15		; CW	5376
	DB	$2A,$70			; CB	112
	DB	$54,<C0011,>C0011	; CALL	C0011
;  936:     memset(@nullstr, strlinbuf, maxfill * 2)
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$2C,$00,$10		; CW	4096
	DB	$2C,$80,$02		; CW	640
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0011,>C0011	; CALL	C0011
;  937:     entrypoint = 0
	DB	$00			; ZERO
	DB	$7A,<D0492,>D0492	; SAW	D0492
;  938:     numlines   = 0
	DB	$00			; ZERO
	DB	$7A,<D0206,>D0206	; SAW	D0206
;  939:     cursrow    = 0
	DB	$00			; ZERO
	DB	$7A,<D0200,>D0200	; SAW	D0200
;  940:     curscol    = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
;  941:     cursx      = 0
	DB	$00			; ZERO
	DB	$78,<D0194,>D0194	; SAB	D0194
;  942:     cursy      = 0
	DB	$00			; ZERO
	DB	$78,<D0195,>D0195	; SAB	D0195
;  943:     scrnleft   = 0
	DB	$00			; ZERO
	DB	$78,<D0196,>D0196	; SAB	D0196
;  944:     scrntop    = 0
	DB	$00			; ZERO
	DB	$7A,<D0202,>D0202	; SAW	D0202
;  945:     cutbuf     = 0
	DB	$00			; ZERO
	DB	$7A,<D0208,>D0208	; SAW	D0208
;  946: end
	DB	$5A			; LEAVE
;  947: ;
;  948: ; Case conversion/printing routines
;  949: ;
;  950: def caseconv_11(chr)
C0087:					; caseconv_11()
					; chr = 2
;  951:     if flags & uppercase
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0089,>C0089	; SKPFLS	C0089
;  952:         if chr & $E0 == $E0
	DB	$66,$02			; LLW	2
	DB	$2A,$E0			; CB	224
	DB	$14			; BAND
	DB	$2A,$E0			; CB	224
	DB	$40			; ISEQ
	DB	$4C,<C0091,>C0091	; SKPFLS	C0091
;  953:             chr = chr - $E0
	DB	$66,$02			; LLW	2
	DB	$2A,$E0			; CB	224
	DB	$04			; SUB
	DB	$76,$02			; SLW	2
;  954:         fin
C0091:
C0092:
;  955:     fin
C0089:
C0090:
;  956:     return chr
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  957: end
;  958: def strupper_10(strptr)
C0093:					; strupper_10()
					; strptr = 2
;  959:     byte i, chr
					; i = 4
					; chr = 5
;  960: 
;  961:     for i = ^strptr downto 1
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
C0096:
	DB	$6C,$04			; DLB	4
	DB	$2A,$01			; CB	1
	DB	$38,<C0095,>C0095	; SKPLT	C0095
	DB	$0E			; DECR
;  962:         chr = (strptr).[i]
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$74,$05			; SLB	5
;  963:         if chr & $E0 == $E0
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$14			; BAND
	DB	$2A,$E0			; CB	224
	DB	$40			; ISEQ
	DB	$4C,<C0097,>C0097	; SKPFLS	C0097
;  964:             (strptr).[i] = chr - $E0
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$04			; SUB
	DB	$70			; SB
;  965:         fin
C0097:
C0098:
;  966:     next
	DB	$50,<C0096,>C0096	; SKIP	C0096
C0095:
	DB	$30			; DROP
;  967: end
	DB	$5A			; LEAVE
;  968: def strlower_10(strptr)
C0099:					; strlower_10()
					; strptr = 2
;  969:     byte i, chr
					; i = 4
					; chr = 5
;  970: 
;  971:     for i = ^strptr downto 1
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
C0102:
	DB	$6C,$04			; DLB	4
	DB	$2A,$01			; CB	1
	DB	$38,<C0101,>C0101	; SKPLT	C0101
	DB	$0E			; DECR
;  972:         chr = (strptr).[i]
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$74,$05			; SLB	5
;  973:         if chr & $E0 == $00
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$14			; BAND
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0103,>C0103	; SKPFLS	C0103
;  974:             (strptr).[i] = chr + $E0
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$02			; ADD
	DB	$70			; SB
;  975:         fin
C0103:
C0104:
;  976:     next
	DB	$50,<C0102,>C0102	; SKIP	C0102
C0101:
	DB	$30			; DROP
;  977: end
	DB	$5A			; LEAVE
;  978: def txtupper
C0105:					; txtupper()
;  979:     word i, strptr
					; i = 2
					; strptr = 4
;  980: 
;  981:     flags = flags ? uppercase
	JSR	INTERP
	DB	$58,$06,$00		; ENTER	6,0
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$08			; CB	8
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
;  982:     for i = numlines - 1 downto 0
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
C0108:
	DB	$6E,$02			; DLW	2
	DB	$00			; ZERO
	DB	$38,<C0107,>C0107	; SKPLT	C0107
	DB	$0E			; DECR
;  983:         strupper_10(strlinbuf:[i])
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0093,>C0093	; CALL	C0093
;  984:     next
	DB	$50,<C0108,>C0108	; SKIP	C0108
C0107:
	DB	$30			; DROP
;  985: end
	DB	$5A			; LEAVE
;  986: def txtlower
C0109:					; txtlower()
;  987:     word i, strptr
					; i = 2
					; strptr = 4
;  988: 
;  989:     flags = flags & #uppercase
	JSR	INTERP
	DB	$58,$06,$00		; ENTER	6,0
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2C,$F7,$FF		; CW	-9
	DB	$14			; BAND
	DB	$78,<D0192,>D0192	; SAB	D0192
;  990:     for i = numlines - 1 downto 0
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
C0112:
	DB	$6E,$02			; DLW	2
	DB	$00			; ZERO
	DB	$38,<C0111,>C0111	; SKPLT	C0111
	DB	$0E			; DECR
;  991:         strlower_10(strlinbuf:[i])
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0099,>C0099	; CALL	C0099
;  992:     next
	DB	$50,<C0112,>C0112	; SKIP	C0112
C0111:
	DB	$30			; DROP
;  993: end
	DB	$5A			; LEAVE
;  994: def prbyte_10(h)
C0113:					; prbyte_10()
					; h = 2
;  995:     cout('$')
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0015,>C0015	; CALL	C0015
;  996:     drop romcall(h, 0, 0, 0, $FDDA)
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DA,$FD		; CW	64986
	DB	$54,<C0007,>C0007	; CALL	C0007
	DB	$30			; DROP
;  997: end
	DB	$5A			; LEAVE
;  998: def prword_10(h)
C0115:					; prword_10()
					; h = 2
;  999:     cout('$')
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1000:     drop romcall(h >> 8, h, 0, 0, $F941)
	DB	$66,$02			; LLW	2
	DB	$2A,$08			; CB	8
	DB	$1C			; SHR
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$41,$F9		; CW	63809
	DB	$54,<C0007,>C0007	; CALL	C0007
	DB	$30			; DROP
; 1001: end
	DB	$5A			; LEAVE
; 1002: def print_10(i)
C0117:					; print_10()
					; i = 2
; 1003:     byte numstr[7]
					; numstr = 4
; 1004:     byte place, sign
					; place = 11
					; sign = 12
; 1005: 
; 1006:     place = 6
	JSR	INTERP
	DB	$58,$0D,$01		; ENTER	13,1
	DB	$2A,$06			; CB	6
	DB	$74,$0B			; SLB	11
; 1007:     if i < 0
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0119,>C0119	; SKPFLS	C0119
; 1008:         sign = 1
	DB	$2A,$01			; CB	1
	DB	$74,$0C			; SLB	12
; 1009:         i    = -i
	DB	$66,$02			; LLW	2
	DB	$10			; NEG
	DB	$76,$02			; SLW	2
; 1010:     else
	DB	$50,<C0120,>C0120	; SKIP	C0120
C0119:
; 1011:         sign = 0
	DB	$00			; ZERO
	DB	$74,$0C			; SLB	12
; 1012:     fin
C0120:
; 1013:     while i >= 10
C0121:
	DB	$66,$02			; LLW	2
	DB	$2A,$0A			; CB	10
	DB	$48			; ISGE
	DB	$4C,<C0122,>C0122	; SKPFLS	C0122
; 1014:         i =, numstr[place] = i % 10 + '0'
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
; 1015:         place              = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
; 1016:     loop
	DB	$50,<C0121,>C0121	; SKIP	C0121
C0122:
; 1017:     numstr[place] = i + '0'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$2A,$30			; CB	48
	DB	$02			; ADD
	DB	$70			; SB
; 1018:     place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
; 1019:     if sign
	DB	$64,$0C			; LLB	12
	DB	$4C,<C0123,>C0123	; SKPFLS	C0123
; 1020:         numstr[place] = '-'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$2D			; CB	45
	DB	$70			; SB
; 1021:         place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
; 1022:     fin
C0123:
C0124:
; 1023:     numstr[place] = 6 - place
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$06			; CB	6
	DB	$64,$0B			; LLB	11
	DB	$04			; SUB
	DB	$70			; SB
; 1024:     prstr(@numstr[place])
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1025: end
	DB	$5A			; LEAVE
; 1026: def nametostr_30(namestr, len, strptr)
C0125:					; nametostr_30()
					; namestr = 2
					; len = 4
					; strptr = 6
; 1027:     ^strptr = len
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$06			; LLW	6
	DB	$66,$04			; LLW	4
	DB	$70			; SB
; 1028:     memcpy(namestr, strptr + 1, len)
	DB	$66,$02			; LLW	2
	DB	$66,$06			; LLW	6
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1029: end
	DB	$5A			; LEAVE
; 1030: ;def toupper_11(c)
; 1031: ;   if c >= 'a'
; 1032: ;       if c <= 'z'
; 1033: ;           return c - $20
; 1034: ;       fin
; 1035: ;   fin
; 1036: ;   return c
; 1037: ;end
; 1038: asm toupper_11
C0127:					; toupper_11()
; 1039:         LDA     ESTKL,X
        LDA     ESTKL,X
; 1040:         AND     #$7F
        AND     #$7F
; 1041:         CMP     #'a'
        CMP     #'a'
; 1042:         BCC     :+
        BCC     :+
; 1043:         CMP     #'z'+1
        CMP     #'z'+1
; 1044:         BCS     :+
        BCS     :+
; 1045:         SEC
        SEC
; 1046:         SBC     #$20
        SBC     #$20
; 1047: :       STA     ESTKL,X
:       STA     ESTKL,X
; 1048: end
	RTS
; 1049: asm clrhibit_10(strptr)
C0129:					; clrhibit_10()
					; strptr = 2
; 1050:         LDY     #$02        ; strptr
	LDY	#4
	LDA	#1
	JSR	ENTER
        LDY     #$02        ; strptr
; 1051:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1052:         STA     SRCL
        STA     SRCL
; 1053:         INY
        INY
; 1054:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1055:         STA     SRCH
        STA     SRCH
; 1056:         LDY     #$00
        LDY     #$00
; 1057:         LDA     (SRC),Y
        LDA     (SRC),Y
; 1058:         BEQ     :+
        BEQ     :+
; 1059:         TAY
        TAY
; 1060: CLHILP: LDA     (SRC),Y
CLHILP: LDA     (SRC),Y
; 1061:         AND     #$7F
        AND     #$7F
; 1062:         STA     (SRC),Y
        STA     (SRC),Y
; 1063:         DEY
        DEY
; 1064:         BNE     CLHILP
        BNE     CLHILP
; 1065: :
:
; 1066: end
	JMP	LEAVE
; 1067: asm sethibit_10(strptr)
C0131:					; sethibit_10()
					; strptr = 2
; 1068:         LDY     #$02        ; strptr
	LDY	#4
	LDA	#1
	JSR	ENTER
        LDY     #$02        ; strptr
; 1069:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1070:         STA     SRCL
        STA     SRCL
; 1071:         INY
        INY
; 1072:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1073:         STA     SRCH
        STA     SRCH
; 1074:         LDY     #$00
        LDY     #$00
; 1075:         LDA     (SRC),Y
        LDA     (SRC),Y
; 1076:         BEQ     :+
        BEQ     :+
; 1077:         TAY
        TAY
; 1078: STHILP: LDA     (SRC),Y
STHILP: LDA     (SRC),Y
; 1079:         ORA     #$80
        ORA     #$80
; 1080:         STA     (SRC),Y
        STA     (SRC),Y
; 1081:         DEY
        DEY
; 1082:         BNE     STHILP
        BNE     STHILP
; 1083: :
:
; 1084: end
	JMP	LEAVE
; 1085: asm cpyln_20(srcstr, dststr)
C0133:					; cpyln_20()
					; srcstr = 2
					; dststr = 4
; 1086:         LDY     #$02        ; srcstr
	LDY	#6
	LDA	#2
	JSR	ENTER
        LDY     #$02        ; srcstr
; 1087:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1088:         STA     SRCL
        STA     SRCL
; 1089:         INY
        INY
; 1090:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1091:         STA     SRCH
        STA     SRCH
; 1092:         INY                 ; dststr
        INY                 ; dststr
; 1093:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1094:         STA     DSTL
        STA     DSTL
; 1095:         INY
        INY
; 1096:         LDA     (FRMP),Y
        LDA     (FRMP),Y
; 1097:         STA     DSTH
        STA     DSTH
; 1098:         LDY     #$00
        LDY     #$00
; 1099:         LDA     (SRC),Y
        LDA     (SRC),Y
; 1100:         TAY
        TAY
; 1101:         LDA     #$00
        LDA     #$00
; 1102:         INY
        INY
; 1103:         STA     (DST),Y
        STA     (DST),Y
; 1104:         DEY
        DEY
; 1105:         BEQ     :++
        BEQ     :++
; 1106: CPLNLP: LDA     (SRC),Y
CPLNLP: LDA     (SRC),Y
; 1107:         CMP     #$20
        CMP     #$20
; 1108:         BCS     :+
        BCS     :+
; 1109:         ADC     #$60
        ADC     #$60
; 1110: :       AND     #$7F
:       AND     #$7F
; 1111:         STA     (DST),Y
        STA     (DST),Y
; 1112:         DEY
        DEY
; 1113:         BNE     CPLNLP
        BNE     CPLNLP
; 1114:         LDA     (SRC),Y
        LDA     (SRC),Y
; 1115: :       STA     (DST),Y
:       STA     (DST),Y
; 1116: end
	JMP	LEAVE
; 1117: ;
; 1118: ; File routines
; 1119: ;
; 1120: def readtxt_10(filename)
C0135:					; readtxt_10()
					; filename = 2
; 1121:     byte txtbuf[81], refnum, i, j
					; txtbuf = 4
					; refnum = 85
					; i = 86
					; j = 87
; 1122: 
; 1123:     refnum = open_21(filename, iobuffer)
	JSR	INTERP
	DB	$58,$58,$01		; ENTER	88,1
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0029,>C0029	; CALL	C0029
	DB	$74,$55			; SLB	85
; 1124:     if refnum
	DB	$64,$55			; LLB	85
	DB	$4C,<C0137,>C0137	; SKPFLS	C0137
; 1125:         drop newline_31(refnum, $7F, $0D)
	DB	$64,$55			; LLB	85
	DB	$2A,$7F			; CB	127
	DB	$2A,$0D			; CB	13
	DB	$54,<C0041,>C0041	; CALL	C0041
	DB	$30			; DROP
; 1126:         repeat
C0140:
; 1127:             txtbuf = read_31(refnum, @txtbuf + 1, maxlnlen)
	DB	$64,$55			; LLB	85
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$2A,$4F			; CB	79
	DB	$54,<C0033,>C0033	; CALL	C0033
	DB	$74,$04			; SLB	4
; 1128:             if txtbuf
	DB	$64,$04			; LLB	4
	DB	$4C,<C0141,>C0141	; SKPFLS	C0141
; 1129:                 sethibit_10(@txtbuf)
	DB	$28,$04			; LLA	4
	DB	$54,<C0131,>C0131	; CALL	C0131
; 1130:                 if flags & uppercase
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0143,>C0143	; SKPFLS	C0143
; 1131:                     strupper_10(@txtbuf)
	DB	$28,$04			; LLA	4
	DB	$54,<C0093,>C0093	; CALL	C0093
; 1132:                 fin
C0143:
C0144:
; 1133:                 strlinbuf:[numlines] = newstr_11(@txtbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$1E			; IDXW
	DB	$28,$04			; LLA	4
	DB	$54,<C0077,>C0077	; CALL	C0077
	DB	$72			; SW
; 1134:                 numlines = numlines + 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 1135:             fin
C0141:
C0142:
; 1136:             if !(numlines & $0F)
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0145,>C0145	; SKPFLS	C0145
; 1137:                 cout('.')
	DB	$2A,$2E			; CB	46
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1138:             fin
C0145:
C0146:
; 1139:         until txtbuf == 0 or numlines == maxlines
	DB	$64,$04			; LLB	4
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2C,$72,$02		; CW	626
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0140,>C0140	; SKPFLS	C0140
C0139:
; 1140:         drop close_11(refnum)
	DB	$64,$55			; LLB	85
	DB	$54,<C0031,>C0031	; CALL	C0031
	DB	$30			; DROP
; 1141:     fin
C0137:
C0138:
; 1142:     if numlines == 0
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0147,>C0147	; SKPFLS	C0147
; 1143:         numlines = 1
	DB	$2A,$01			; CB	1
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 1144:     fin
C0147:
C0148:
; 1145: end
	DB	$5A			; LEAVE
; 1146: def writetxt_10(filename)
C0149:					; writetxt_10()
					; filename = 2
; 1147:     byte txtbuf[81], refnum
					; txtbuf = 4
					; refnum = 85
; 1148:     byte j, chr
					; j = 86
					; chr = 87
; 1149:     word i, strptr
					; i = 88
					; strptr = 90
; 1150: 
; 1151:     drop destroy_11(filename)
	JSR	INTERP
	DB	$58,$5C,$01		; ENTER	92,1
	DB	$66,$02			; LLW	2
	DB	$54,<C0039,>C0039	; CALL	C0039
	DB	$30			; DROP
; 1152:     drop create_41(filename, $C3, $04, $00) ; full access, TXT file
	DB	$66,$02			; LLW	2
	DB	$2A,$C3			; CB	195
	DB	$2A,$04			; CB	4
	DB	$00			; ZERO
	DB	$54,<C0037,>C0037	; CALL	C0037
	DB	$30			; DROP
; 1153:     refnum = open_21(filename, iobuffer)
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0029,>C0029	; CALL	C0029
	DB	$74,$55			; SLB	85
; 1154:     if refnum == 0
	DB	$64,$55			; LLB	85
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0151,>C0151	; SKPFLS	C0151
; 1155:         return
	DB	$5A			; LEAVE
; 1156:     fin
C0151:
C0152:
; 1157:     for i = 0 to numlines - 1
	DB	$00			; ZERO
C0154:
	DB	$6E,$58			; DLW	88
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$3A,<C0153,>C0153	; SKPGT	C0153
	DB	$0C			; INCR
; 1158:         cpyln_20(strlinbuf:[i], @txtbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$58			; LLW	88
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$04			; LLA	4
	DB	$54,<C0133,>C0133	; CALL	C0133
; 1159:         txtbuf = txtbuf + 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$04			; SLB	4
; 1160:         txtbuf[txtbuf] = $0D
	DB	$28,$04			; LLA	4
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$2A,$0D			; CB	13
	DB	$70			; SB
; 1161:         drop write_31(refnum, @txtbuf + 1, txtbuf)
	DB	$64,$55			; LLB	85
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$64,$04			; LLB	4
	DB	$54,<C0035,>C0035	; CALL	C0035
	DB	$30			; DROP
; 1162:         if !(i & $0F)
	DB	$66,$58			; LLW	88
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0155,>C0155	; SKPFLS	C0155
; 1163:             cout('.')
	DB	$2A,$2E			; CB	46
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1164:         fin
C0155:
C0156:
; 1165:     next
	DB	$50,<C0154,>C0154	; SKIP	C0154
C0153:
	DB	$30			; DROP
; 1166:     drop close_11(refnum)
	DB	$64,$55			; LLB	85
	DB	$54,<C0031,>C0031	; CALL	C0031
	DB	$30			; DROP
; 1167: end
	DB	$5A			; LEAVE
; 1168: ;
; 1169: ; Screen routines
; 1170: ;
; 1171: def clrscrn
C0157:					; clrscrn()
; 1172:     drop romcall(0, 0, 0, 0, $FC58)
	JSR	INTERP
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$58,$FC		; CW	64600
	DB	$54,<C0007,>C0007	; CALL	C0007
	DB	$30			; DROP
; 1173: end
	DB	$5C			; RET
; 1174: def drawrow_30(row, ofst, strptr)
C0159:					; drawrow_30()
					; row = 2
					; ofst = 4
					; strptr = 6
; 1175:     byte numchars
					; numchars = 8
; 1176:     word scrnptr
					; scrnptr = 9
; 1177: 
; 1178:     scrnptr = txtscrn[row]
	JSR	INTERP
	DB	$58,$0B,$03		; ENTER	11,3
	DB	$26,<D0000,>D0000	; LA	D0000
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$76,$09			; SLW	9
; 1179:     if ^strptr <= ofst
	DB	$66,$06			; LLW	6
	DB	$60			; LB
	DB	$66,$04			; LLW	4
	DB	$4A			; ISLE
	DB	$4C,<C0161,>C0161	; SKPFLS	C0161
; 1180:         numchars = 0
	DB	$00			; ZERO
	DB	$74,$08			; SLB	8
; 1181:     else
	DB	$50,<C0162,>C0162	; SKIP	C0162
C0161:
; 1182:         numchars = ^strptr - ofst
	DB	$66,$06			; LLW	6
	DB	$60			; LB
	DB	$66,$04			; LLW	4
	DB	$04			; SUB
	DB	$74,$08			; SLB	8
; 1183:     fin
C0162:
; 1184:     if numchars >= 40
	DB	$64,$08			; LLB	8
	DB	$2A,$28			; CB	40
	DB	$48			; ISGE
	DB	$4C,<C0163,>C0163	; SKPFLS	C0163
; 1185:         numchars = 40
	DB	$2A,$28			; CB	40
	DB	$74,$08			; SLB	8
; 1186:     else
	DB	$50,<C0164,>C0164	; SKIP	C0164
C0163:
; 1187:         memset($A0A0, scrnptr + numchars, 40 - numchars)
	DB	$2C,$A0,$A0		; CW	41120
	DB	$66,$09			; LLW	9
	DB	$64,$08			; LLB	8
	DB	$02			; ADD
	DB	$2A,$28			; CB	40
	DB	$64,$08			; LLB	8
	DB	$04			; SUB
	DB	$54,<C0011,>C0011	; CALL	C0011
; 1188:     fin
C0164:
; 1189:     memcpy(strptr + ofst + 1, scrnptr, numchars)
	DB	$66,$06			; LLW	6
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$09			; LLW	9
	DB	$64,$08			; LLB	8
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1190: end
	DB	$5A			; LEAVE
; 1191: defopt drawscrn_20(toprow, ofst)
C0165:					; drawscrn_20()
					; toprow = 2
					; ofst = 4
; 1192:     byte row, numchars
					; row = 6
					; numchars = 7
; 1193:     word strptr, scrnptr
					; strptr = 8
					; scrnptr = 10
; 1194: 
; 1195:     for row = 0 to 23
	LDY	#12
	LDA	#2
	JSR	ENTER
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
C0168:
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	DEX
	LDA	#$17
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	LDA	ESTKH-1,X
	SBC	ESTKH,X
	BPL	:+
	JMP	C0167
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
; 1196:         strptr  = strlinbuf:[toprow + row]
	DEX
	STY	ESTKL,X
	LDA	#$10
	STA	ESTKH,X
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
	JSR	ADD
	JSR	IDXW
	JSR	LW
	LDY	#$08
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
; 1197:         scrnptr = txtscrn[row]
	LDA	#<D0000
	STA	ESTKL,X
	LDA	#>D0000
	STA	ESTKH,X
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	IDXW
	JSR	LW
	LDY	#$0A
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
; 1198:         if ^strptr <= ofst
	LDY	#$08
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	ISLE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0169
:
; 1199:             numchars = 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 1200:         else
	INX
	JMP	C0170
C0169:
; 1201:             numchars = ^strptr - ofst
	DEX
	LDY	#$08
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDY	#$04
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JSR	SUB
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 1202:         fin
	INX
C0170:
; 1203:         if numchars >= 40
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#$28
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0171
:
; 1204:             numchars = 40
	DEX
	LDA	#$28
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 1205:         else
	INX
	JMP	C0172
C0171:
; 1206:             memset($A0A0, scrnptr + numchars, 40 - numchars)
	DEX
	LDA	#$A0
	STA	ESTKL,X
	STA	ESTKH,X
	DEX
	LDY	#$0A
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	DEX
	LDA	#$28
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	JSR	C0011
; 1207:         fin
C0172:
; 1208:         memcpy(strptr + ofst + 1, scrnptr, numchars)
	DEX
	LDY	#$08
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
	JSR	ADD
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	DEX
	LDY	#$0A
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0013
; 1209:     next
	JMP	C0168
C0167:
; 1210: end
	INX
	JMP	LEAVE
; 1211: def cursoff
C0173:					; cursoff()
; 1212:     if flags & showcurs
	JSR	INTERP
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0175,>C0175	; SKPFLS	C0175
; 1213:         ^cursptr = underchr
	DB	$6A,<D0204,>D0204	; LAW	D0204
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$70			; SB
; 1214:         flags = flags & #showcurs
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2C,$FB,$FF		; CW	-5
	DB	$14			; BAND
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1215:     fin
C0175:
C0176:
; 1216: end
	DB	$5C			; RET
; 1217: def curson
C0177:					; curson()
; 1218:     if !(flags & showcurs)
	JSR	INTERP
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0179,>C0179	; SKPFLS	C0179
; 1219:         cursptr  = txtscrn[cursy] + cursx
	DB	$26,<D0000,>D0000	; LA	D0000
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$02			; ADD
	DB	$7A,<D0204,>D0204	; SAW	D0204
; 1220:         underchr = ^cursptr
	DB	$6A,<D0204,>D0204	; LAW	D0204
	DB	$60			; LB
	DB	$78,<D0198,>D0198	; SAB	D0198
; 1221:         ^cursptr = curschr
	DB	$6A,<D0204,>D0204	; LAW	D0204
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$70			; SB
; 1222:         flags = flags ? showcurs
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1223:     fin
C0179:
C0180:
; 1224: end
	DB	$5C			; RET
; 1225: def cursflash()
C0181:					; cursflash()
; 1226:     if flags & showcurs
	JSR	INTERP
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0183,>C0183	; SKPFLS	C0183
; 1227:         if flash == 0
	DB	$68,<D0193,>D0193	; LAB	D0193
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0185,>C0185	; SKPFLS	C0185
; 1228:             ^cursptr = curschr
	DB	$6A,<D0204,>D0204	; LAW	D0204
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$70			; SB
; 1229:         elsif flash == 128
	DB	$50,<C0186,>C0186	; SKIP	C0186
C0185:
	DB	$68,<D0193,>D0193	; LAB	D0193
	DB	$2A,$80			; CB	128
	DB	$40			; ISEQ
	DB	$4C,<C0187,>C0187	; SKPFLS	C0187
; 1230:             ^cursptr = underchr
	DB	$6A,<D0204,>D0204	; LAW	D0204
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$70			; SB
; 1231:         fin
C0187:
C0186:
; 1232:         flash = flash + 1
	DB	$68,<D0193,>D0193	; LAB	D0193
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0193,>D0193	; SAB	D0193
; 1233:     fin
C0183:
C0184:
; 1234: end
	DB	$5C			; RET
; 1235: def redraw
C0188:					; redraw()
; 1236:     cursoff()
	JSR	INTERP
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1237:     drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1238:     curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1239: end
	DB	$5C			; RET
; 1240: def curshome
C0190:					; curshome()
; 1241:     cursoff()
	JSR	INTERP
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1242:     cursrow  = 0
	DB	$00			; ZERO
	DB	$7A,<D0200,>D0200	; SAW	D0200
; 1243:     curscol  = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1244:     cursx    = 0
	DB	$00			; ZERO
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1245:     cursy    = 0
	DB	$00			; ZERO
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1246:     scrnleft = 0
	DB	$00			; ZERO
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1247:     scrntop  = 0
	DB	$00			; ZERO
	DB	$7A,<D0202,>D0202	; SAW	D0202
; 1248:     drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1249:     curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1250: end
	DB	$5C			; RET
; 1251: def cursend
C0192:					; cursend()
; 1252:     cursoff()
	JSR	INTERP
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1253:     if numlines > 23
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$17			; CB	23
	DB	$44			; ISGT
	DB	$4C,<C0194,>C0194	; SKPFLS	C0194
; 1254:         cursrow  = numlines - 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0200,>D0200	; SAW	D0200
; 1255:         cursy    = 23
	DB	$2A,$17			; CB	23
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1256:         scrntop  = cursrow - 23
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$17			; CB	23
	DB	$04			; SUB
	DB	$7A,<D0202,>D0202	; SAW	D0202
; 1257:     else
	DB	$50,<C0195,>C0195	; SKIP	C0195
C0194:
; 1258:         cursrow  = numlines - 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0200,>D0200	; SAW	D0200
; 1259:         cursy    = numlines - 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1260:         scrntop  = 0
	DB	$00			; ZERO
	DB	$7A,<D0202,>D0202	; SAW	D0202
; 1261:     fin
C0195:
; 1262:     curscol  = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1263:     cursx    = 0
	DB	$00			; ZERO
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1264:     scrnleft = 0
	DB	$00			; ZERO
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1265:     drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1266:     curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1267: end
	DB	$5C			; RET
; 1268: def cursup
C0196:					; cursup()
; 1269:     if cursrow > 0
	JSR	INTERP
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0198,>C0198	; SKPFLS	C0198
; 1270:         cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1271:         cursrow = cursrow - 1
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0200,>D0200	; SAW	D0200
; 1272:         if cursy > 0
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0200,>C0200	; SKPFLS	C0200
; 1273:             cursy = cursy - 1
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1274:         else
	DB	$50,<C0201,>C0201	; SKIP	C0201
C0200:
; 1275:             scrntop = cursrow
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$7A,<D0202,>D0202	; SAW	D0202
; 1276:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1277:         fin
C0201:
; 1278:         curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1279:     fin
C0198:
C0199:
; 1280: end
	DB	$5C			; RET
; 1281: def pgup
C0202:					; pgup()
; 1282:     byte i
					; i = 2
; 1283: 
; 1284:     for i = pgjmp downto 0
	JSR	INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$10			; CB	16
C0205:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0204,>C0204	; SKPLT	C0204
	DB	$0E			; DECR
; 1285:         cursup()
	DB	$54,<C0196,>C0196	; CALL	C0196
; 1286:     next
	DB	$50,<C0205,>C0205	; SKIP	C0205
C0204:
	DB	$30			; DROP
; 1287: end
	DB	$5A			; LEAVE
; 1288: def cursdown
C0206:					; cursdown()
; 1289:     if cursrow < numlines - 1
	JSR	INTERP
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$46			; ISLT
	DB	$4C,<C0208,>C0208	; SKPFLS	C0208
; 1290:         cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1291:         cursrow = cursrow + 1
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0200,>D0200	; SAW	D0200
; 1292:         if cursy < 23
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$17			; CB	23
	DB	$46			; ISLT
	DB	$4C,<C0210,>C0210	; SKPFLS	C0210
; 1293:             cursy = cursy + 1
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1294:         else
	DB	$50,<C0211,>C0211	; SKIP	C0211
C0210:
; 1295:             scrntop = cursrow - 23
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$17			; CB	23
	DB	$04			; SUB
	DB	$7A,<D0202,>D0202	; SAW	D0202
; 1296:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1297:         fin
C0211:
; 1298:         curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1299:     fin
C0208:
C0209:
; 1300: end
	DB	$5C			; RET
; 1301: def pgdown
C0212:					; pgdown()
; 1302:     byte i
					; i = 2
; 1303: 
; 1304:     for i = pgjmp downto 0
	JSR	INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$10			; CB	16
C0215:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0214,>C0214	; SKPLT	C0214
	DB	$0E			; DECR
; 1305:         cursdown()
	DB	$54,<C0206,>C0206	; CALL	C0206
; 1306:     next
	DB	$50,<C0215,>C0215	; SKIP	C0215
C0214:
	DB	$30			; DROP
; 1307: end
	DB	$5A			; LEAVE
; 1308: def cursleft
C0216:					; cursleft()
; 1309:     if curscol > 0
	JSR	INTERP
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0218,>C0218	; SKPFLS	C0218
; 1310:         cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1311:         curscol = curscol - 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1312:         if cursx > 0
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0220,>C0220	; SKPFLS	C0220
; 1313:             cursx = cursx - 1
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1314:         else
	DB	$50,<C0221,>C0221	; SKIP	C0221
C0220:
; 1315:             scrnleft = curscol
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1316:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1317:         fin
C0221:
; 1318:         curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1319:     fin
C0218:
C0219:
; 1320: end
	DB	$5C			; RET
; 1321: def pgleft
C0222:					; pgleft()
; 1322:     byte i
					; i = 2
; 1323: 
; 1324:     for i = 7 downto 0
	JSR	INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$07			; CB	7
C0225:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0224,>C0224	; SKPLT	C0224
	DB	$0E			; DECR
; 1325:         cursleft()
	DB	$54,<C0216,>C0216	; CALL	C0216
; 1326:     next
	DB	$50,<C0225,>C0225	; SKIP	C0225
C0224:
	DB	$30			; DROP
; 1327: end
	DB	$5A			; LEAVE
; 1328: def cursright
C0226:					; cursright()
; 1329:     if curscol < 80
	JSR	INTERP
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$50			; CB	80
	DB	$46			; ISLT
	DB	$4C,<C0228,>C0228	; SKPFLS	C0228
; 1330:         cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1331:         curscol = curscol + 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1332:         if cursx < 39
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$2A,$27			; CB	39
	DB	$46			; ISLT
	DB	$4C,<C0230,>C0230	; SKPFLS	C0230
; 1333:             cursx = cursx + 1
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1334:         else
	DB	$50,<C0231,>C0231	; SKIP	C0231
C0230:
; 1335:             scrnleft = curscol - 39
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$27			; CB	39
	DB	$04			; SUB
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1336:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1337:         fin
C0231:
; 1338:         curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1339:     fin
C0228:
C0229:
; 1340: end
	DB	$5C			; RET
; 1341: def pgright
C0232:					; pgright()
; 1342:     byte i
					; i = 2
; 1343: 
; 1344:     for i = 7 downto 0
	JSR	INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$07			; CB	7
C0235:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0234,>C0234	; SKPLT	C0234
	DB	$0E			; DECR
; 1345:         cursright()
	DB	$54,<C0226,>C0226	; CALL	C0226
; 1346:     next
	DB	$50,<C0235,>C0235	; SKIP	C0235
C0234:
	DB	$30			; DROP
; 1347: end
	DB	$5A			; LEAVE
; 1348: ;
; 1349: ; Keyboard routines
; 1350: ;
; 1351: def keyin2e_01
C0236:					; keyin2e_01()
; 1352:     repeat
	JSR	INTERP
C0239:
; 1353:         cursflash()
	DB	$54,<C0181,>C0181	; CALL	C0181
; 1354:     until ^keyboard >= 128
	DB	$2C,$00,$C0		; CW	49152
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$48			; ISGE
	DB	$4C,<C0239,>C0239	; SKPFLS	C0239
C0238:
; 1355:     return ^keystrobe
	DB	$2C,$10,$C0		; CW	49168
	DB	$60			; LB
	DB	$5C			; RET
; 1356: end
; 1357: def keyin2_01
C0240:					; keyin2_01()
; 1358:     byte key
					; key = 2
; 1359: 
; 1360:     repeat
	JSR	INTERP
	DB	$58,$03,$00		; ENTER	3,0
C0243:
; 1361:         cursflash()
	DB	$54,<C0181,>C0181	; CALL	C0181
; 1362:         key = ^keyboard
	DB	$2C,$00,$C0		; CW	49152
	DB	$60			; LB
	DB	$74,$02			; SLB	2
; 1363:         if key == keyctrll
	DB	$64,$02			; LLB	2
	DB	$2A,$8C			; CB	140
	DB	$40			; ISEQ
	DB	$4C,<C0244,>C0244	; SKPFLS	C0244
; 1364:             drop ^keystrobe
	DB	$2C,$10,$C0		; CW	49168
	DB	$60			; LB
	DB	$30			; DROP
; 1365:             flags = flags ^ shiftlock
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$80			; CB	128
	DB	$18			; XOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1366:             key   = 0
	DB	$00			; ZERO
	DB	$74,$02			; SLB	2
; 1367:         fin
C0244:
C0245:
; 1368:     until key >= 128
	DB	$64,$02			; LLB	2
	DB	$2A,$80			; CB	128
	DB	$48			; ISGE
	DB	$4C,<C0243,>C0243	; SKPFLS	C0243
C0242:
; 1369:     drop ^keystrobe
	DB	$2C,$10,$C0		; CW	49168
	DB	$60			; LB
	DB	$30			; DROP
; 1370:     if key == keyctrln
	DB	$64,$02			; LLB	2
	DB	$2A,$8E			; CB	142
	DB	$40			; ISEQ
	DB	$4C,<C0246,>C0246	; SKPFLS	C0246
; 1371:         key = $DB ; [
	DB	$2A,$DB			; CB	219
	DB	$74,$02			; SLB	2
; 1372:     elsif key == keyctrlp
	DB	$50,<C0247,>C0247	; SKIP	C0247
C0246:
	DB	$64,$02			; LLB	2
	DB	$2A,$90			; CB	144
	DB	$40			; ISEQ
	DB	$4C,<C0248,>C0248	; SKPFLS	C0248
; 1373:         key = $DF ; _
	DB	$2A,$DF			; CB	223
	DB	$74,$02			; SLB	2
; 1374:     elsif key == keyctrlb
	DB	$50,<C0247,>C0247	; SKIP	C0247
C0248:
	DB	$64,$02			; LLB	2
	DB	$2A,$82			; CB	130
	DB	$40			; ISEQ
	DB	$4C,<C0249,>C0249	; SKPFLS	C0249
; 1375:         key = $DC ; \
	DB	$2A,$DC			; CB	220
	DB	$74,$02			; SLB	2
; 1376:     elsif key == keyarrowleft
	DB	$50,<C0247,>C0247	; SKIP	C0247
C0249:
	DB	$64,$02			; LLB	2
	DB	$2A,$88			; CB	136
	DB	$40			; ISEQ
	DB	$4C,<C0250,>C0250	; SKPFLS	C0250
; 1377:         if ^pushbttn3 < 128
	DB	$2C,$63,$C0		; CW	49251
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$46			; ISLT
	DB	$4C,<C0251,>C0251	; SKPFLS	C0251
; 1378:             key = $FF
	DB	$2A,$FF			; CB	255
	DB	$74,$02			; SLB	2
; 1379:         fin
C0251:
C0252:
; 1380:     elsif key >= $C0 and flags < shiftlock
	DB	$50,<C0247,>C0247	; SKIP	C0247
C0250:
	DB	$64,$02			; LLB	2
	DB	$2A,$C0			; CB	192
	DB	$48			; ISGE
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$80			; CB	128
	DB	$46			; ISLT
	DB	$24			; LAND
	DB	$4C,<C0253,>C0253	; SKPFLS	C0253
; 1381:         if ^pushbttn3 < 128
	DB	$2C,$63,$C0		; CW	49251
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$46			; ISLT
	DB	$4C,<C0254,>C0254	; SKPFLS	C0254
; 1382:             if key == $C0
	DB	$64,$02			; LLB	2
	DB	$2A,$C0			; CB	192
	DB	$40			; ISEQ
	DB	$4C,<C0256,>C0256	; SKPFLS	C0256
; 1383:                 key = $D0 ; P
	DB	$2A,$D0			; CB	208
	DB	$74,$02			; SLB	2
; 1384:             elsif key == $DD
	DB	$50,<C0257,>C0257	; SKIP	C0257
C0256:
	DB	$64,$02			; LLB	2
	DB	$2A,$DD			; CB	221
	DB	$40			; ISEQ
	DB	$4C,<C0258,>C0258	; SKPFLS	C0258
; 1385:                 key = $CD ; M
	DB	$2A,$CD			; CB	205
	DB	$74,$02			; SLB	2
; 1386:             elsif key == $DE
	DB	$50,<C0257,>C0257	; SKIP	C0257
C0258:
	DB	$64,$02			; LLB	2
	DB	$2A,$DE			; CB	222
	DB	$40			; ISEQ
	DB	$4C,<C0259,>C0259	; SKPFLS	C0259
; 1387:                 key = $CE ; N
	DB	$2A,$CE			; CB	206
	DB	$74,$02			; SLB	2
; 1388:             fin
C0259:
C0257:
; 1389:         else
	DB	$50,<C0255,>C0255	; SKIP	C0255
C0254:
; 1390:            key = key ? $E0
	DB	$64,$02			; LLB	2
	DB	$2A,$E0			; CB	224
	DB	$16			; IOR
	DB	$74,$02			; SLB	2
; 1391:         fin
C0255:
; 1392:     fin
C0253:
C0247:
; 1393:     return key
	DB	$64,$02			; LLB	2
	DB	$5A			; LEAVE
; 1394: end
; 1395: ;
; 1396: ; Printer routines
; 1397: ;
; 1398: def printtxt_10(slot)
C0260:					; printtxt_10()
					; slot = 2
; 1399:     byte txtbuf[80]
					; txtbuf = 4
; 1400:     word i, scrncsw
					; i = 84
					; scrncsw = 86
; 1401: 
; 1402:     scrncsw = *(csw)
	JSR	INTERP
	DB	$58,$58,$01		; ENTER	88,1
	DB	$2A,$36			; CB	54
	DB	$62			; LW
	DB	$76,$56			; SLW	86
; 1403:     *(csw)  = $C000 ? (slot << 8)
	DB	$2A,$36			; CB	54
	DB	$2C,$00,$C0		; CW	49152
	DB	$66,$02			; LLW	2
	DB	$2A,$08			; CB	8
	DB	$1A			; SHL
	DB	$16			; IOR
	DB	$72			; SW
; 1404:     for i = 0 to numlines - 1
	DB	$00			; ZERO
C0263:
	DB	$6E,$54			; DLW	84
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$3A,<C0262,>C0262	; SKPGT	C0262
	DB	$0C			; INCR
; 1405:         cpyln_20(strlinbuf:[i], @txtbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$54			; LLW	84
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$04			; LLA	4
	DB	$54,<C0133,>C0133	; CALL	C0133
; 1406:         prstr(@txtbuf)
	DB	$28,$04			; LLA	4
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1407:         crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1408:     next
	DB	$50,<C0263,>C0263	; SKIP	C0263
C0262:
	DB	$30			; DROP
; 1409:     *(csw) = scrncsw
	DB	$2A,$36			; CB	54
	DB	$66,$56			; LLW	86
	DB	$72			; SW
; 1410: end
	DB	$5A			; LEAVE
; 1411: def openline_11(row)
C0264:					; openline_11()
					; row = 2
; 1412:     if numlines < maxlines
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2C,$72,$02		; CW	626
	DB	$46			; ISLT
	DB	$4C,<C0266,>C0266	; SKPFLS	C0266
; 1413:         memcpy(@strlinbuf:[row], @strlinbuf:[row + 1], (numlines - row) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$66,$02			; LLW	2
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1414:         strlinbuf:[row] = @nullstr
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$72			; SW
; 1415:         numlines = numlines + 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 1416:         flags = flags ? changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1417:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1418:     fin
C0266:
C0267:
; 1419:     bell()
	DB	$54,<C0045,>C0045	; CALL	C0045
; 1420:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1421: end
; 1422: def cutline
C0268:					; cutline()
; 1423:     freestr_10(cutbuf)
	JSR	INTERP
	DB	$6A,<D0208,>D0208	; LAW	D0208
	DB	$54,<C0073,>C0073	; CALL	C0073
; 1424:     cutbuf = strlinbuf:[cursrow]
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$7A,<D0208,>D0208	; SAW	D0208
; 1425:     memcpy(@strlinbuf:[cursrow + 1], @strlinbuf:[cursrow], (numlines - cursrow) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1426:     if numlines > 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$44			; ISGT
	DB	$4C,<C0270,>C0270	; SKPFLS	C0270
; 1427:         numlines = numlines - 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 1428:     fin
C0270:
C0271:
; 1429:     flags = flags ? changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1430:     if cursrow == numlines
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$40			; ISEQ
	DB	$4C,<C0272,>C0272	; SKPFLS	C0272
; 1431:         cursup()
	DB	$54,<C0196,>C0196	; CALL	C0196
; 1432:     fin
C0272:
C0273:
; 1433:     redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1434: end
	DB	$5C			; RET
; 1435: def pasteline
C0274:					; pasteline()
; 1436:     if cutbuf and numlines < maxlines
	JSR	INTERP
	DB	$6A,<D0208,>D0208	; LAW	D0208
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2C,$72,$02		; CW	626
	DB	$46			; ISLT
	DB	$24			; LAND
	DB	$4C,<C0276,>C0276	; SKPFLS	C0276
; 1437:         memcpy(@strlinbuf:[cursrow], @strlinbuf:[cursrow + 1], (numlines - cursrow) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1438:         strlinbuf:[cursrow] = newstr_11(cutbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$6A,<D0208,>D0208	; LAW	D0208
	DB	$54,<C0077,>C0077	; CALL	C0077
	DB	$72			; SW
; 1439:         numlines = numlines + 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 1440:         flags = flags ? changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1441:         redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1442:     else
	DB	$50,<C0277,>C0277	; SKIP	C0277
C0276:
; 1443:         bell()
	DB	$54,<C0045,>C0045	; CALL	C0045
; 1444:     fin
C0277:
; 1445: end
	DB	$5C			; RET
; 1446: def joinline
C0278:					; joinline()
; 1447:     byte joinstr[80], joinlen
					; joinstr = 2
					; joinlen = 82
; 1448: 
; 1449:     if cursrow < numlines - 1
	JSR	INTERP
	DB	$58,$53,$00		; ENTER	83,0
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$46			; ISLT
	DB	$4C,<C0280,>C0280	; SKPFLS	C0280
; 1450:         strcpy_20(strlinbuf:[cursrow], @joinstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$02			; LLA	2
	DB	$54,<C0047,>C0047	; CALL	C0047
; 1451:         joinlen = joinstr + ^(strlinbuf:[cursrow + 1])
	DB	$64,$02			; LLB	2
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$60			; LB
	DB	$02			; ADD
	DB	$74,$52			; SLB	82
; 1452:         if joinlen < 80
	DB	$64,$52			; LLB	82
	DB	$2A,$50			; CB	80
	DB	$46			; ISLT
	DB	$4C,<C0282,>C0282	; SKPFLS	C0282
; 1453:             memcpy(strlinbuf:[cursrow + 1] + 1, @joinstr + joinstr + 1, ^(strlinbuf:[cursrow + 1]))
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$28,$02			; LLA	2
	DB	$64,$02			; LLB	2
	DB	$02			; ADD
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$60			; LB
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1454:             joinstr = joinlen
	DB	$64,$52			; LLB	82
	DB	$74,$02			; SLB	2
; 1455:             freestr_10(strlinbuf:[cursrow])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0073,>C0073	; CALL	C0073
; 1456:             strlinbuf:[cursrow] = newstr_11(@joinstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$28,$02			; LLA	2
	DB	$54,<C0077,>C0077	; CALL	C0077
	DB	$72			; SW
; 1457:             freestr_10(strlinbuf:[cursrow + 1])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0073,>C0073	; CALL	C0073
; 1458:             numlines = numlines - 1
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 1459:             memcpy(@strlinbuf:[cursrow + 2], @strlinbuf:[cursrow + 1], (numlines - cursrow) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$6A,<D0206,>D0206	; LAW	D0206
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1460:             flags = flags ? changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1461:             redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1462:         else
	DB	$50,<C0283,>C0283	; SKIP	C0283
C0282:
; 1463:             bell()
	DB	$54,<C0045,>C0045	; CALL	C0045
; 1464:         fin
C0283:
; 1465:     fin
C0280:
C0281:
; 1466: end
	DB	$5A			; LEAVE
; 1467: def splitline
C0284:					; splitline()
; 1468:     byte splitstr[80], splitlen
					; splitstr = 2
					; splitlen = 82
; 1469: 
; 1470:     if openline_11(cursrow + 1)
	JSR	INTERP
	DB	$58,$53,$00		; ENTER	83,0
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0264,>C0264	; CALL	C0264
	DB	$4C,<C0286,>C0286	; SKPFLS	C0286
; 1471:         if curscol
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$4C,<C0288,>C0288	; SKPFLS	C0288
; 1472:             splitlen = ^(strlinbuf:[cursrow])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$60			; LB
	DB	$74,$52			; SLB	82
; 1473:             if curscol < splitlen - 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$64,$52			; LLB	82
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$46			; ISLT
	DB	$4C,<C0290,>C0290	; SKPFLS	C0290
; 1474:                 memcpy(strlinbuf:[cursrow] + curscol + 1, @splitstr + 1, splitlen - curscol)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$02			; ADD
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$28,$02			; LLA	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$64,$52			; LLB	82
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$04			; SUB
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1475:                 splitstr = splitlen - curscol
	DB	$64,$52			; LLB	82
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$04			; SUB
	DB	$74,$02			; SLB	2
; 1476:                 strlinbuf:[cursrow + 1] = newstr_11(@splitstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$28,$02			; LLA	2
	DB	$54,<C0077,>C0077	; CALL	C0077
	DB	$72			; SW
; 1477:                 memcpy(strlinbuf:[cursrow] + 1, @splitstr + 1, curscol)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$28,$02			; LLA	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1478:                 splitstr = curscol
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$74,$02			; SLB	2
; 1479:                 freestr_10(strlinbuf:[cursrow])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0073,>C0073	; CALL	C0073
; 1480:                 strlinbuf:[cursrow] = newstr_11(@splitstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$28,$02			; LLA	2
	DB	$54,<C0077,>C0077	; CALL	C0077
	DB	$72			; SW
; 1481:             fin
C0290:
C0291:
; 1482:         else
	DB	$50,<C0289,>C0289	; SKIP	C0289
C0288:
; 1483:             strlinbuf:[cursrow + 1] = strlinbuf:[cursrow]
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$72			; SW
; 1484:             strlinbuf:[cursrow]     = @nullstr
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$72			; SW
; 1485:         fin
C0289:
; 1486:         curscol  = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1487:         cursx    = 0
	DB	$00			; ZERO
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1488:         scrnleft = 0
	DB	$00			; ZERO
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1489:         redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1490:         cursdown()
	DB	$54,<C0206,>C0206	; CALL	C0206
; 1491:     fin
C0286:
C0287:
; 1492: end
	DB	$5A			; LEAVE
; 1493: def editkey_11(key)
C0292:					; editkey_11()
					; key = 2
; 1494:     if key >= keyspace
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$2A,$A0			; CB	160
	DB	$48			; ISGE
	DB	$4C,<C0294,>C0294	; SKPFLS	C0294
; 1495:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1496:     elsif key == keydelete
	DB	$50,<C0295,>C0295	; SKIP	C0295
C0294:
	DB	$66,$02			; LLW	2
	DB	$2A,$FF			; CB	255
	DB	$40			; ISEQ
	DB	$4C,<C0296,>C0296	; SKPFLS	C0296
; 1497:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1498:     elsif key == keyctrld
	DB	$50,<C0295,>C0295	; SKIP	C0295
C0296:
	DB	$66,$02			; LLW	2
	DB	$2A,$84			; CB	132
	DB	$40			; ISEQ
	DB	$4C,<C0297,>C0297	; SKPFLS	C0297
; 1499:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1500:     elsif key == keyctrlr
	DB	$50,<C0295,>C0295	; SKIP	C0295
C0297:
	DB	$66,$02			; LLW	2
	DB	$2A,$92			; CB	146
	DB	$40			; ISEQ
	DB	$4C,<C0298,>C0298	; SKPFLS	C0298
; 1501:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1502:     fin
C0298:
C0295:
; 1503:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1504: end
; 1505: def editline_11(key)
C0299:					; editline_11()
					; key = 2
; 1506:     byte editstr[80]
					; editstr = 4
; 1507:     word undoline
					; undoline = 84
; 1508: 
; 1509:     if (editkey_11(key))
	JSR	INTERP
	DB	$58,$56,$01		; ENTER	86,1
	DB	$66,$02			; LLW	2
	DB	$54,<C0292,>C0292	; CALL	C0292
	DB	$4C,<C0301,>C0301	; SKPFLS	C0301
; 1510:         flags = flags ? changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1511:         memset($A0A0, @editstr, 80)
	DB	$2C,$A0,$A0		; CW	41120
	DB	$28,$04			; LLA	4
	DB	$2A,$50			; CB	80
	DB	$54,<C0011,>C0011	; CALL	C0011
; 1512:         strcpy_20(strlinbuf:[cursrow], @editstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$04			; LLA	4
	DB	$54,<C0047,>C0047	; CALL	C0047
; 1513:         undoline = strlinbuf:[cursrow]
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$76,$54			; SLW	84
; 1514:         strlinbuf:[cursrow] = @editstr
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$28,$04			; LLA	4
	DB	$72			; SW
; 1515:         repeat
C0304:
; 1516:             if key >= keyspace
	DB	$66,$02			; LLW	2
	DB	$2A,$A0			; CB	160
	DB	$48			; ISGE
	DB	$4C,<C0305,>C0305	; SKPFLS	C0305
; 1517:                 if key == keydelete
	DB	$66,$02			; LLW	2
	DB	$2A,$FF			; CB	255
	DB	$40			; ISEQ
	DB	$4C,<C0307,>C0307	; SKPFLS	C0307
; 1518:                     if curscol > 0
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0309,>C0309	; SKPFLS	C0309
; 1519:                         if curscol <= editstr
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$64,$04			; LLB	4
	DB	$4A			; ISLE
	DB	$4C,<C0311,>C0311	; SKPFLS	C0311
; 1520:                             memcpy(@editstr[curscol + 1], @editstr[curscol], editstr - curscol)
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$02			; IDXB
	DB	$64,$04			; LLB	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$04			; SUB
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1521:                             editstr = editstr - 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$04			; SLB	4
; 1522:                         fin
C0311:
C0312:
; 1523:                         curscol = curscol - 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1524:                         cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1525:                         if cursx > 0
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0313,>C0313	; SKPFLS	C0313
; 1526:                             cursx = cursx - 1
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1527:                             drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$28,$04			; LLA	4
	DB	$54,<C0159,>C0159	; CALL	C0159
; 1528:                         else
	DB	$50,<C0314,>C0314	; SKIP	C0314
C0313:
; 1529:                             scrnleft = scrnleft - 1
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1530:                             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1531:                         fin
C0314:
; 1532:                         curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1533:                     fin
C0309:
C0310:
; 1534:                 elsif curscol < maxlnlen
	DB	$50,<C0308,>C0308	; SKIP	C0308
C0307:
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$4F			; CB	79
	DB	$46			; ISLT
	DB	$4C,<C0315,>C0315	; SKPFLS	C0315
; 1535:                     curscol = curscol + 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1536:                     cursx   = cursx   + 1
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1537:                     if flags & insmode
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0316,>C0316	; SKPFLS	C0316
; 1538:                         if editstr < maxlnlen or editstr.maxlnlen == $A0
	DB	$64,$04			; LLB	4
	DB	$2A,$4F			; CB	79
	DB	$46			; ISLT
	DB	$28,$53			; LLA	83
	DB	$60			; LB
	DB	$2A,$A0			; CB	160
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0318,>C0318	; SKPFLS	C0318
; 1539:                             editstr = editstr + 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$04			; SLB	4
; 1540:                             if curscol >= editstr
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$64,$04			; LLB	4
	DB	$48			; ISGE
	DB	$4C,<C0320,>C0320	; SKPFLS	C0320
; 1541:                                 editstr = curscol
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$74,$04			; SLB	4
; 1542:                             else
	DB	$50,<C0321,>C0321	; SKIP	C0321
C0320:
; 1543:                                 memcpy(@editstr[curscol], @editstr[curscol + 1], editstr - curscol)
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$02			; IDXB
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$64,$04			; LLB	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$04			; SUB
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1544:                             fin
C0321:
; 1545:                         else
	DB	$50,<C0319,>C0319	; SKIP	C0319
C0318:
; 1546:                             curscol = curscol - 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1547:                             cursx   = cursx   - 1
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1548:                             key     = editstr[curscol]
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$76,$02			; SLW	2
; 1549:                             bell()
	DB	$54,<C0045,>C0045	; CALL	C0045
; 1550:                         fin
C0319:
; 1551:                     else
	DB	$50,<C0317,>C0317	; SKIP	C0317
C0316:
; 1552:                         if curscol > editstr
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$64,$04			; LLB	4
	DB	$44			; ISGT
	DB	$4C,<C0322,>C0322	; SKPFLS	C0322
; 1553:                             editstr = curscol
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$74,$04			; SLB	4
; 1554:                         fin
C0322:
C0323:
; 1555:                     fin
C0317:
; 1556:                     editstr[curscol] = caseconv_11(key)
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$54,<C0087,>C0087	; CALL	C0087
	DB	$70			; SB
; 1557:                     cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1558:                     if cursx <= 39
	DB	$68,<D0194,>D0194	; LAB	D0194
	DB	$2A,$27			; CB	39
	DB	$4A			; ISLE
	DB	$4C,<C0324,>C0324	; SKPFLS	C0324
; 1559:                         drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$28,$04			; LLA	4
	DB	$54,<C0159,>C0159	; CALL	C0159
; 1560:                     else
	DB	$50,<C0325,>C0325	; SKIP	C0325
C0324:
; 1561:                         scrnleft = scrnleft + 1
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1562:                         cursx    = 39
	DB	$2A,$27			; CB	39
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1563:                         drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 1564:                     fin
C0325:
; 1565:                     curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1566:                 else
	DB	$50,<C0308,>C0308	; SKIP	C0308
C0315:
; 1567:                     bell()
	DB	$54,<C0045,>C0045	; CALL	C0045
; 1568:                 fin
C0308:
; 1569:             elsif key == keyctrld
	DB	$50,<C0306,>C0306	; SKIP	C0306
C0305:
	DB	$66,$02			; LLW	2
	DB	$2A,$84			; CB	132
	DB	$40			; ISEQ
	DB	$4C,<C0326,>C0326	; SKPFLS	C0326
; 1570:                 if curscol < editstr
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$64,$04			; LLB	4
	DB	$46			; ISLT
	DB	$4C,<C0327,>C0327	; SKPFLS	C0327
; 1571:                     memcpy(@editstr[curscol + 2], @editstr[curscol + 1], editstr - curscol)
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$28,$04			; LLA	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$64,$04			; LLB	4
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$04			; SUB
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1572:                     editstr = editstr - 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$04			; SLB	4
; 1573:                     cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1574:                     drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$28,$04			; LLA	4
	DB	$54,<C0159,>C0159	; CALL	C0159
; 1575:                     curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1576:                 fin
C0327:
C0328:
; 1577:             elsif key == keyctrlr
	DB	$50,<C0306,>C0306	; SKIP	C0306
C0326:
	DB	$66,$02			; LLW	2
	DB	$2A,$92			; CB	146
	DB	$40			; ISEQ
	DB	$4C,<C0329,>C0329	; SKPFLS	C0329
; 1578:                 strcpy_20(undoline, @editstr)
	DB	$66,$54			; LLW	84
	DB	$28,$04			; LLA	4
	DB	$54,<C0047,>C0047	; CALL	C0047
; 1579:                 cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1580:                 drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$28,$04			; LLA	4
	DB	$54,<C0159,>C0159	; CALL	C0159
; 1581:                 curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 1582:             fin
C0329:
C0306:
; 1583:             key = keyin_01()
	DB	$26,<D0210,>D0210	; LA	D0210
	DB	$62			; LW
	DB	$34			; PUSH
	DB	$36			; PULL
	DB	$56			; ICAL
	DB	$76,$02			; SLW	2
; 1584:         until !editkey_11(key)
	DB	$66,$02			; LLW	2
	DB	$54,<C0292,>C0292	; CALL	C0292
	DB	$20			; NOT
	DB	$4C,<C0304,>C0304	; SKPFLS	C0304
C0303:
; 1585:         if editstr
	DB	$64,$04			; LLB	4
	DB	$4C,<C0330,>C0330	; SKPFLS	C0330
; 1586:             strlinbuf:[cursrow] = newstr_11(@editstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$28,$04			; LLA	4
	DB	$54,<C0077,>C0077	; CALL	C0077
	DB	$72			; SW
; 1587:         else
	DB	$50,<C0331,>C0331	; SKIP	C0331
C0330:
; 1588:             strlinbuf:[cursrow] = @nullstr
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$1E			; IDXW
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$72			; SW
; 1589:         fin
C0331:
; 1590:         freestr_10(undoline)
	DB	$66,$54			; LLW	84
	DB	$54,<C0073,>C0073	; CALL	C0073
; 1591:     fin
C0301:
C0302:
; 1592:     return key
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
; 1593: end
; 1594: def editmode
C0332:					; editmode()
; 1595:     repeat
	JSR	INTERP
C0335:
; 1596:         when editline_11(keyin_01())
	DB	$26,<D0210,>D0210	; LA	D0210
	DB	$62			; LW
	DB	$34			; PUSH
	DB	$36			; PULL
	DB	$56			; ICAL
	DB	$54,<C0299,>C0299	; CALL	C0299
; 1597:             is keyarrowup
	DB	$2A,$8B			; CB	139
	DB	$3E,<C0337,>C0337	; SKPNE	C0337
; 1598:                 cursup()
	DB	$54,<C0196,>C0196	; CALL	C0196
; 1599:             is keyarrowdown
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0337:
	DB	$2A,$8A			; CB	138
	DB	$3E,<C0338,>C0338	; SKPNE	C0338
; 1600:                 cursdown()
	DB	$54,<C0206,>C0206	; CALL	C0206
; 1601:             is keyarrowleft
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0338:
	DB	$2A,$88			; CB	136
	DB	$3E,<C0339,>C0339	; SKPNE	C0339
; 1602:                 cursleft()
	DB	$54,<C0216,>C0216	; CALL	C0216
; 1603:             is keyarrowright
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0339:
	DB	$2A,$95			; CB	149
	DB	$3E,<C0340,>C0340	; SKPNE	C0340
; 1604:                 cursright()
	DB	$54,<C0226,>C0226	; CALL	C0226
; 1605:             is keyctrlw
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0340:
	DB	$2A,$97			; CB	151
	DB	$3E,<C0341,>C0341	; SKPNE	C0341
; 1606:                 pgup()
	DB	$54,<C0202,>C0202	; CALL	C0202
; 1607:             is keyctrlz
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0341:
	DB	$2A,$9A			; CB	154
	DB	$3E,<C0342,>C0342	; SKPNE	C0342
; 1608:                 pgdown()
	DB	$54,<C0212,>C0212	; CALL	C0212
; 1609:             is keyctrla
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0342:
	DB	$2A,$81			; CB	129
	DB	$3E,<C0343,>C0343	; SKPNE	C0343
; 1610:                 pgleft()
	DB	$54,<C0222,>C0222	; CALL	C0222
; 1611:             is keyctrls
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0343:
	DB	$2A,$93			; CB	147
	DB	$3E,<C0344,>C0344	; SKPNE	C0344
; 1612:                 pgright()
	DB	$54,<C0232,>C0232	; CALL	C0232
; 1613:             is keyctrlq
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0344:
	DB	$2A,$91			; CB	145
	DB	$3E,<C0345,>C0345	; SKPNE	C0345
; 1614:                 curshome()
	DB	$54,<C0190,>C0190	; CALL	C0190
; 1615:             is keyctrle
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0345:
	DB	$2A,$85			; CB	133
	DB	$3E,<C0346,>C0346	; SKPNE	C0346
; 1616:                 cursend()
	DB	$54,<C0192,>C0192	; CALL	C0192
; 1617:             is keyctrlx
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0346:
	DB	$2A,$98			; CB	152
	DB	$3E,<C0347,>C0347	; SKPNE	C0347
; 1618:                 cutline()
	DB	$54,<C0268,>C0268	; CALL	C0268
; 1619:             is keyctrlv
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0347:
	DB	$2A,$96			; CB	150
	DB	$3E,<C0348,>C0348	; SKPNE	C0348
; 1620:                 pasteline()
	DB	$54,<C0274,>C0274	; CALL	C0274
; 1621:             is keyctrlo
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0348:
	DB	$2A,$8F			; CB	143
	DB	$3E,<C0349,>C0349	; SKPNE	C0349
; 1622:                 drop openline_11(cursrow)
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$54,<C0264,>C0264	; CALL	C0264
	DB	$30			; DROP
; 1623:                 redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1624:             is keyenter
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0349:
	DB	$2A,$8D			; CB	141
	DB	$3E,<C0350,>C0350	; SKPNE	C0350
; 1625:                 if flags & insmode
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0351,>C0351	; SKPFLS	C0351
; 1626:                     splitline()
	DB	$54,<C0284,>C0284	; CALL	C0284
; 1627:                 else
	DB	$50,<C0352,>C0352	; SKIP	C0352
C0351:
; 1628:                     drop openline_11(cursrow + 1)
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0264,>C0264	; CALL	C0264
	DB	$30			; DROP
; 1629:                     cursdown()
	DB	$54,<C0206,>C0206	; CALL	C0206
; 1630:                     redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1631:                 fin
C0352:
; 1632:             is keyctrlt
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0350:
	DB	$2A,$94			; CB	148
	DB	$3E,<C0353,>C0353	; SKPNE	C0353
; 1633:                 joinline()
	DB	$54,<C0278,>C0278	; CALL	C0278
; 1634:             is keyctrli
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0353:
	DB	$2A,$89			; CB	137
	DB	$3E,<C0354,>C0354	; SKPNE	C0354
; 1635:                 if flags & insmode
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0355,>C0355	; SKPFLS	C0355
; 1636:                     flags = flags & #insmode
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2C,$FD,$FF		; CW	-3
	DB	$14			; BAND
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1637:                     curschr = ' '
	DB	$2A,$20			; CB	32
	DB	$78,<D0199,>D0199	; SAB	D0199
; 1638:                 else
	DB	$50,<C0356,>C0356	; SKIP	C0356
C0355:
; 1639:                     flags = flags ? insmode
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$02			; CB	2
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1640:                     curschr = '+'
	DB	$2A,$2B			; CB	43
	DB	$78,<D0199,>D0199	; SAB	D0199
; 1641:                 fin
C0356:
; 1642:             is keyctrlc
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0354:
	DB	$2A,$83			; CB	131
	DB	$3E,<C0357,>C0357	; SKPNE	C0357
; 1643:                 if flags & uppercase
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0358,>C0358	; SKPFLS	C0358
; 1644:                     txtlower()
	DB	$54,<C0109,>C0109	; CALL	C0109
; 1645:                 else
	DB	$50,<C0359,>C0359	; SKIP	C0359
C0358:
; 1646:                     txtupper()
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1647:                 fin
C0359:
; 1648:                 redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1649:             is keyescape
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0357:
	DB	$2A,$9B			; CB	155
	DB	$3E,<C0360,>C0360	; SKPNE	C0360
; 1650:                 cursoff()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1651:                 cmdmode()
	DB	$54,<C0000,>C0000	; CALL	C0000
; 1652:                 redraw()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1653:         wend
	DB	$50,<C0336,>C0336	; SKIP	C0336
C0360:
C0336:
	DB	$30			; DROP
; 1654:     until 0
	DB	$00			; ZERO
	DB	$4C,<C0335,>C0335	; SKPFLS	C0335
C0334:
; 1655: end
	DB	$5C			; RET
; 1656: ;
; 1657: ; Command mode
; 1658: ;
; 1659: def prfiles_11(optpath)
C0362:					; prfiles_11()
					; optpath = 2
; 1660:     byte path[64]
					; path = 4
; 1661:     byte refnum
					; refnum = 68
; 1662:     byte firstblk
					; firstblk = 69
; 1663:     byte entrylen, entriesblk
					; entrylen = 70
					; entriesblk = 71
; 1664:     byte i, type, len
					; i = 72
					; type = 73
					; len = 74
; 1665:     word entry, filecnt
					; entry = 75
					; filecnt = 77
; 1666: 
; 1667:     if ^optpath
	JSR	INTERP
	DB	$58,$4F,$01		; ENTER	79,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0364,>C0364	; SKPFLS	C0364
; 1668:         strcpy_20(optpath, @path)
	DB	$66,$02			; LLW	2
	DB	$28,$04			; LLA	4
	DB	$54,<C0047,>C0047	; CALL	C0047
; 1669:     else
	DB	$50,<C0365,>C0365	; SKIP	C0365
C0364:
; 1670:         drop getpfx_11(@path)
	DB	$28,$04			; LLA	4
	DB	$54,<C0025,>C0025	; CALL	C0025
	DB	$30			; DROP
; 1671:         prstr(@path)
	DB	$28,$04			; LLA	4
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1672:         crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1673:     fin
C0365:
; 1674:     refnum = open_21(@path, iobuffer);
	DB	$28,$04			; LLA	4
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0029,>C0029	; CALL	C0029
	DB	$74,$44			; SLB	68
; 1675:     if perr
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$4C,<C0366,>C0366	; SKPFLS	C0366
; 1676:         return perr
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$5A			; LEAVE
; 1677:     fin
C0366:
C0367:
; 1678:     firstblk = 1
	DB	$2A,$01			; CB	1
	DB	$74,$45			; SLB	69
; 1679:     repeat
C0369:
; 1680:         if read_31(refnum, databuff, 512) == 512
	DB	$64,$44			; LLB	68
	DB	$2C,$00,$0C		; CW	3072
	DB	$2C,$00,$02		; CW	512
	DB	$54,<C0033,>C0033	; CALL	C0033
	DB	$2C,$00,$02		; CW	512
	DB	$40			; ISEQ
	DB	$4C,<C0370,>C0370	; SKPFLS	C0370
; 1681:             entry = databuff + 4
	DB	$2C,$00,$0C		; CW	3072
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
; 1682:             if firstblk
	DB	$64,$45			; LLB	69
	DB	$4C,<C0372,>C0372	; SKPFLS	C0372
; 1683:                 entrylen   = databuff.$23
	DB	$2C,$23,$0C		; CW	3107
	DB	$60			; LB
	DB	$74,$46			; SLB	70
; 1684:                 entriesblk = databuff.$24
	DB	$2C,$24,$0C		; CW	3108
	DB	$60			; LB
	DB	$74,$47			; SLB	71
; 1685:                 filecnt    = databuff:$25
	DB	$2C,$25,$0C		; CW	3109
	DB	$62			; LW
	DB	$76,$4D			; SLW	77
; 1686:                 entry      = entry + entrylen
	DB	$66,$4B			; LLW	75
	DB	$64,$46			; LLB	70
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
; 1687:             fin
C0372:
C0373:
; 1688:             for i = firstblk to entriesblk
	DB	$64,$45			; LLB	69
C0375:
	DB	$6C,$48			; DLB	72
	DB	$64,$47			; LLB	71
	DB	$3A,<C0374,>C0374	; SKPGT	C0374
	DB	$0C			; INCR
; 1689:                 type = ^entry
	DB	$66,$4B			; LLW	75
	DB	$60			; LB
	DB	$74,$49			; SLB	73
; 1690:                 if type <> 0
	DB	$64,$49			; LLB	73
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0376,>C0376	; SKPFLS	C0376
; 1691:                     len = type & $0F
	DB	$64,$49			; LLB	73
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$74,$4A			; SLB	74
; 1692:                     ^entry = len
	DB	$66,$4B			; LLW	75
	DB	$64,$4A			; LLB	74
	DB	$70			; SB
; 1693:                     prstr(entry)
	DB	$66,$4B			; LLW	75
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1694:                     if type & $F0 == $D0 ; Is it a directory?
	DB	$64,$49			; LLB	73
	DB	$2A,$F0			; CB	240
	DB	$14			; BAND
	DB	$2A,$D0			; CB	208
	DB	$40			; ISEQ
	DB	$4C,<C0378,>C0378	; SKPFLS	C0378
; 1695:                         cout('/')
	DB	$2A,$2F			; CB	47
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1696:                         len = len + 1
	DB	$64,$4A			; LLB	74
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$4A			; SLB	74
; 1697:                     fin
C0378:
C0379:
; 1698:                     for len = 20 - len downto 1
	DB	$2A,$14			; CB	20
	DB	$64,$4A			; LLB	74
	DB	$04			; SUB
C0381:
	DB	$6C,$4A			; DLB	74
	DB	$2A,$01			; CB	1
	DB	$38,<C0380,>C0380	; SKPLT	C0380
	DB	$0E			; DECR
; 1699:                         cout(' ')
	DB	$2A,$20			; CB	32
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1700:                     next
	DB	$50,<C0381,>C0381	; SKIP	C0381
C0380:
	DB	$30			; DROP
; 1701:                     filecnt = filecnt - 1
	DB	$66,$4D			; LLW	77
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$4D			; SLW	77
; 1702:                 fin
C0376:
C0377:
; 1703:                 entry = entry + entrylen
	DB	$66,$4B			; LLW	75
	DB	$64,$46			; LLB	70
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
; 1704:             next
	DB	$50,<C0375,>C0375	; SKIP	C0375
C0374:
	DB	$30			; DROP
; 1705:             firstblk = 0
	DB	$00			; ZERO
	DB	$74,$45			; SLB	69
; 1706:         else
	DB	$50,<C0371,>C0371	; SKIP	C0371
C0370:
; 1707:             filecnt = 0
	DB	$00			; ZERO
	DB	$76,$4D			; SLW	77
; 1708:         fin
C0371:
; 1709:     until filecnt == 0
	DB	$66,$4D			; LLW	77
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0369,>C0369	; SKPFLS	C0369
C0368:
; 1710:     drop close_11(refnum)
	DB	$64,$44			; LLB	68
	DB	$54,<C0031,>C0031	; CALL	C0031
	DB	$30			; DROP
; 1711:     crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1712:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1713: end
; 1714: def striplead_20(strptr, chr)
C0382:					; striplead_20()
					; strptr = 2
					; chr = 4
; 1715:     while ^strptr and ^(strptr + 1) == chr
	JSR	INTERP
	DB	$58,$06,$02		; ENTER	6,2
C0384:
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$66,$04			; LLW	4
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0385,>C0385	; SKPFLS	C0385
; 1716:         memcpy(strptr + 2, strptr + 1, ^strptr)
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1717:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
; 1718:     loop
	DB	$50,<C0384,>C0384	; SKIP	C0384
C0385:
; 1719: end
	DB	$5A			; LEAVE
; 1720: def parsecmd_11(strptr)
C0386:					; parsecmd_11()
					; strptr = 2
; 1721:     byte cmd
					; cmd = 4
; 1722: 
; 1723:     cmd = 0
	JSR	INTERP
	DB	$58,$05,$01		; ENTER	5,1
	DB	$00			; ZERO
	DB	$74,$04			; SLB	4
; 1724:     striplead_20(strptr, ' ')
	DB	$66,$02			; LLW	2
	DB	$2A,$20			; CB	32
	DB	$54,<C0382,>C0382	; CALL	C0382
; 1725:     if ^strptr
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0388,>C0388	; SKPFLS	C0388
; 1726:         cmd = ^(strptr + 1)
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$04			; SLB	4
; 1727:         memcpy(strptr + 2, strptr + 1, ^strptr)
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$54,<C0013,>C0013	; CALL	C0013
; 1728:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
; 1729:     fin
C0388:
C0389:
; 1730:     if ^strptr
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0390,>C0390	; SKPFLS	C0390
; 1731:         striplead_20(strptr, ' ')
	DB	$66,$02			; LLW	2
	DB	$2A,$20			; CB	32
	DB	$54,<C0382,>C0382	; CALL	C0382
; 1732:     fin
C0390:
C0391:
; 1733:     return cmd
	DB	$64,$04			; LLB	4
	DB	$5A			; LEAVE
; 1734: end
; 1735: def chkchng_01
C0392:					; chkchng_01()
; 1736:     if flags & changed
	JSR	INTERP
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0394,>C0394	; SKPFLS	C0394
; 1737:         prstr(@losechng)
	DB	$26,<D0104,>D0104	; LA	D0104
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1738:         if toupper_11(keyin_01()) == 'N'
	DB	$26,<D0210,>D0210	; LA	D0210
	DB	$62			; LW
	DB	$34			; PUSH
	DB	$36			; PULL
	DB	$56			; ICAL
	DB	$54,<C0127,>C0127	; CALL	C0127
	DB	$2A,$4E			; CB	78
	DB	$40			; ISEQ
	DB	$4C,<C0396,>C0396	; SKPFLS	C0396
; 1739:             crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1740:             return 0
	DB	$00			; ZERO
	DB	$5C			; RET
; 1741:         fin
C0396:
C0397:
; 1742:         crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1743:     fin
C0394:
C0395:
; 1744:     return 1
	DB	$2A,$01			; CB	1
	DB	$5C			; RET
; 1745: end
; 1746: def exec
C0398:					; exec()
; 1747: 	when execentry()
	JSR	INTERP
	DB	$54,<C0005,>C0005	; CALL	C0005
; 1748: 		is 1
	DB	$2A,$01			; CB	1
	DB	$3E,<C0401,>C0401	; SKPNE	C0401
; 1749: 			crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1750: 			prstr(@brkmsg)
	DB	$26,<D0956,>D0956	; LA	D0956
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1751: 			crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1752: 		is 2
	DB	$50,<C0400,>C0400	; SKIP	C0400
C0401:
	DB	$2A,$02			; CB	2
	DB	$3E,<C0402,>C0402	; SKPNE	C0402
; 1753: 			crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1754: 			prstr(@stkovflwmsg)
	DB	$26,<D0969,>D0969	; LA	D0969
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1755: 			crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1756: 	wend
	DB	$50,<C0400,>C0400	; SKIP	C0400
C0402:
C0400:
	DB	$30			; DROP
; 1757: 	;
; 1758: 	; Close all files
; 1759: 	;
; 1760: 	^$BFD8 = 0
	DB	$2C,$D8,$BF		; CW	49112
	DB	$00			; ZERO
	DB	$70			; SB
; 1761: 	drop close_11(0)
	DB	$00			; ZERO
	DB	$54,<C0031,>C0031	; CALL	C0031
	DB	$30			; DROP
; 1762: end
	DB	$5C			; RET
; 1763: def quit
C0404:					; quit()
; 1764:     if chkchng_01()
	JSR	INTERP
	DB	$54,<C0392,>C0392	; CALL	C0392
	DB	$4C,<C0406,>C0406	; SKPFLS	C0406
; 1765:         exit
	DB	$54,<C0023,>C0023	; CALL	C0023
; 1766:     fin
C0406:
C0407:
; 1767: end
	DB	$5C			; RET
; 1768: def cmdmode
C0000:					; cmdmode()
; 1769:     byte slot
					; slot = 2
; 1770:     word cmdptr
					; cmdptr = 3
; 1771: 
; 1772:     clrscrn();
	JSR	INTERP
	DB	$58,$05,$00		; ENTER	5,0
	DB	$54,<C0157,>C0157	; CALL	C0157
; 1773:     prstr(@version)
	DB	$26,<D0049,>D0049	; LA	D0049
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1774:     crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1775:     while 1
C0409:
	DB	$2A,$01			; CB	1
	DB	$4C,<C0410,>C0410	; SKPFLS	C0410
; 1776:         prstr(@txtfile)
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1777:         cmdptr = rdstr($BA)
	DB	$2A,$BA			; CB	186
	DB	$54,<C0021,>C0021	; CALL	C0021
	DB	$76,$03			; SLW	3
; 1778:         when toupper_11(parsecmd_11(cmdptr))
	DB	$66,$03			; LLW	3
	DB	$54,<C0386,>C0386	; CALL	C0386
	DB	$54,<C0127,>C0127	; CALL	C0127
; 1779:             is 'A'
	DB	$2A,$41			; CB	65
	DB	$3E,<C0412,>C0412	; SKPNE	C0412
; 1780:                 readtxt_10(cmdptr)
	DB	$66,$03			; LLW	3
	DB	$54,<C0135,>C0135	; CALL	C0135
; 1781:                 flags = flags ? changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1782:             is 'R'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0412:
	DB	$2A,$52			; CB	82
	DB	$3E,<C0413,>C0413	; SKPNE	C0413
; 1783:                 if chkchng_01()
	DB	$54,<C0392,>C0392	; CALL	C0392
	DB	$4C,<C0414,>C0414	; SKPFLS	C0414
; 1784:                     inittxtbuf()
	DB	$54,<C0085,>C0085	; CALL	C0085
; 1785:                     strcpy_20(cmdptr, @txtfile)
	DB	$66,$03			; LLW	3
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0047,>C0047	; CALL	C0047
; 1786:                     readtxt_10(@txtfile)
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0135,>C0135	; CALL	C0135
; 1787:                     entrypoint = 0
	DB	$00			; ZERO
	DB	$7A,<D0492,>D0492	; SAW	D0492
; 1788:                     flags = flags & #changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2C,$FE,$FF		; CW	-2
	DB	$14			; BAND
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1789:                 fin
C0414:
C0415:
; 1790:             is 'W'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0413:
	DB	$2A,$57			; CB	87
	DB	$3E,<C0416,>C0416	; SKPNE	C0416
; 1791:                 if ^cmdptr
	DB	$66,$03			; LLW	3
	DB	$60			; LB
	DB	$4C,<C0417,>C0417	; SKPFLS	C0417
; 1792:                     strcpy_20(cmdptr, @txtfile)
	DB	$66,$03			; LLW	3
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0047,>C0047	; CALL	C0047
; 1793:                 fin
C0417:
C0418:
; 1794:                 writetxt_10(@txtfile)
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0149,>C0149	; CALL	C0149
; 1795:                 if flags & changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0419,>C0419	; SKPFLS	C0419
; 1796:                     entrypoint = 0
	DB	$00			; ZERO
	DB	$7A,<D0492,>D0492	; SAW	D0492
; 1797:                 fin
C0419:
C0420:
; 1798:                 flags = flags & #changed
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2C,$FE,$FF		; CW	-2
	DB	$14			; BAND
	DB	$78,<D0192,>D0192	; SAB	D0192
; 1799:             is 'Q'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0416:
	DB	$2A,$51			; CB	81
	DB	$3E,<C0421,>C0421	; SKPNE	C0421
; 1800:                 quit()
	DB	$54,<C0404,>C0404	; CALL	C0404
; 1801:             is 'C'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0421:
	DB	$2A,$43			; CB	67
	DB	$3E,<C0422,>C0422	; SKPNE	C0422
; 1802:                 drop prfiles_11(cmdptr)
	DB	$66,$03			; LLW	3
	DB	$54,<C0362,>C0362	; CALL	C0362
	DB	$30			; DROP
; 1803:             is 'P'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0422:
	DB	$2A,$50			; CB	80
	DB	$3E,<C0423,>C0423	; SKPNE	C0423
; 1804:                 drop setpfx_11(cmdptr)
	DB	$66,$03			; LLW	3
	DB	$54,<C0027,>C0027	; CALL	C0027
	DB	$30			; DROP
; 1805:             is 'H'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0423:
	DB	$2A,$48			; CB	72
	DB	$3E,<C0424,>C0424	; SKPNE	C0424
; 1806:                 if ^cmdptr
	DB	$66,$03			; LLW	3
	DB	$60			; LB
	DB	$4C,<C0425,>C0425	; SKPFLS	C0425
; 1807:                     slot = cmdptr.1 - '0'
	DB	$28,$04			; LLA	4
	DB	$60			; LB
	DB	$2A,$30			; CB	48
	DB	$04			; SUB
	DB	$74,$02			; SLB	2
; 1808:                 else
	DB	$50,<C0426,>C0426	; SKIP	C0426
C0425:
; 1809:                     slot = 1
	DB	$2A,$01			; CB	1
	DB	$74,$02			; SLB	2
; 1810:                 fin
C0426:
; 1811:                 printtxt_10(slot)
	DB	$64,$02			; LLB	2
	DB	$54,<C0260,>C0260	; CALL	C0260
; 1812:             is 'E'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0424:
	DB	$2A,$45			; CB	69
	DB	$3E,<C0427,>C0427	; SKPNE	C0427
; 1813:                 return
	DB	$30			; DROP
	DB	$5A			; LEAVE
; 1814:             is 0
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0427:
	DB	$00			; ZERO
	DB	$3E,<C0428,>C0428	; SKPNE	C0428
; 1815:                 return
	DB	$30			; DROP
	DB	$5A			; LEAVE
; 1816:             is 'N'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0428:
	DB	$2A,$4E			; CB	78
	DB	$3E,<C0429,>C0429	; SKPNE	C0429
; 1817:                 if chkchng_01()
	DB	$54,<C0392,>C0392	; CALL	C0392
	DB	$4C,<C0430,>C0430	; SKPFLS	C0430
; 1818:                     inittxtbuf()
	DB	$54,<C0085,>C0085	; CALL	C0085
; 1819:                     numlines = 1
	DB	$2A,$01			; CB	1
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 1820:                     strcpy_20(@untitled, @txtfile)
	DB	$26,<D0132,>D0132	; LA	D0132
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0047,>C0047	; CALL	C0047
; 1821:                 fin
C0430:
C0431:
; 1822:             is 'X'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0429:
	DB	$2A,$58			; CB	88
	DB	$3E,<C0432,>C0432	; SKPNE	C0432
; 1823:                 if flags & changed or !entrypoint
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$6A,<D0492,>D0492	; LAW	D0492
	DB	$20			; NOT
	DB	$22			; LOR
	DB	$4C,<C0433,>C0433	; SKPFLS	C0433
; 1824:                     drop parse_module_01()
	DB	$54,<C0002,>C0002	; CALL	C0002
	DB	$30			; DROP
; 1825:                     if parserr
	DB	$68,<D0498,>D0498	; LAB	D0498
	DB	$4C,<C0435,>C0435	; SKPFLS	C0435
; 1826:                         bell()
	DB	$54,<C0045,>C0045	; CALL	C0045
; 1827:                         cursrow  = parserrln
	DB	$6A,<D0503,>D0503	; LAW	D0503
	DB	$7A,<D0200,>D0200	; SAW	D0200
; 1828:                         scrntop  = cursrow & $FFF8
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$2C,$F8,$FF		; CW	65528
	DB	$14			; BAND
	DB	$7A,<D0202,>D0202	; SAW	D0202
; 1829:                         cursy    = cursrow - scrntop
	DB	$6A,<D0200,>D0200	; LAW	D0200
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$04			; SUB
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1830:                         curscol  = parserrpos
	DB	$68,<D0497,>D0497	; LAB	D0497
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1831:                         scrnleft = curscol & $FFE0
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2C,$E0,$FF		; CW	65504
	DB	$14			; BAND
	DB	$78,<D0196,>D0196	; SAB	D0196
; 1832:                         cursx    = curscol - scrnleft
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$04			; SUB
	DB	$78,<D0194,>D0194	; SAB	D0194
; 1833:                     else
	DB	$50,<C0436,>C0436	; SKIP	C0436
C0435:
; 1834:                         crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1835:                         exec(entrypoint)
	DB	$6A,<D0492,>D0492	; LAW	D0492
	DB	$54,<C0398,>C0398	; CALL	C0398
; 1836:                     fin
C0436:
; 1837:                 else
	DB	$50,<C0434,>C0434	; SKIP	C0434
C0433:
; 1838:                     exec(entrypoint)
	DB	$6A,<D0492,>D0492	; LAW	D0492
	DB	$54,<C0398,>C0398	; CALL	C0398
; 1839:                 fin
C0434:
; 1840:                 crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1841:             is 'V'
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0432:
	DB	$2A,$56			; CB	86
	DB	$3E,<C0437,>C0437	; SKPNE	C0437
; 1842: 				prstr(@version)
	DB	$26,<D0049,>D0049	; LA	D0049
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1843:         wend
	DB	$50,<C0411,>C0411	; SKIP	C0411
C0437:
C0411:
	DB	$30			; DROP
; 1844:         if perr
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$4C,<C0439,>C0439	; SKPFLS	C0439
; 1845:             prstr(@errorstr)
	DB	$26,<D0076,>D0076	; LA	D0076
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1846:             drop romcall(perr, 0, 0, 0, $FDDA)
	DB	$68,<D0088,>D0088	; LAB	D0088
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DA,$FD		; CW	64986
	DB	$54,<C0007,>C0007	; CALL	C0007
	DB	$30			; DROP
; 1847:         else
	DB	$50,<C0440,>C0440	; SKIP	C0440
C0439:
; 1848:             prstr(@okstr)
	DB	$26,<D0085,>D0085	; LA	D0085
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1849:         fin
C0440:
; 1850:         crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1851:     loop
	DB	$50,<C0409,>C0409	; SKIP	C0409
C0410:
; 1852: end
	DB	$5A			; LEAVE
; 1853: 
; 1854: ;=====================================
; 1855: ;
; 1856: ;           PLASMA Compiler
; 1857: ;
; 1858: ;=====================================
; 1859: 
; 1860: ;
; 1861: ; Error handler
; 1862: ;
; 1863: def parse_err_11(err)
C0441:					; parse_err_11()
					; err = 2
; 1864:     if !parserr
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$68,<D0498,>D0498	; LAB	D0498
	DB	$20			; NOT
	DB	$4C,<C0443,>C0443	; SKPFLS	C0443
; 1865:         parserr    = TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$78,<D0498,>D0498	; SAB	D0498
; 1866:         parserrln  = lineno - 1
	DB	$6A,<D0507,>D0507	; LAW	D0507
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0503,>D0503	; SAW	D0503
; 1867:         parserrpos = tknptr - inbuff
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$2C,$00,$02		; CW	512
	DB	$04			; SUB
	DB	$78,<D0497,>D0497	; SAB	D0497
; 1868:         print_10(lineno)
	DB	$6A,<D0507,>D0507	; LAW	D0507
	DB	$54,<C0117,>C0117	; CALL	C0117
; 1869:         cout(':')
	DB	$2A,$3A			; CB	58
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1870:         prstr(err)
	DB	$66,$02			; LLW	2
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1871:         crout()
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1872:     fin
C0443:
C0444:
; 1873:     return ERR_TKN
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1874: end
; 1875: ;
; 1876: ; Emit bytecode
; 1877: ;
; 1878: def ctag_new_01
C0445:					; ctag_new_01()
; 1879:     if codetag >= ctag_max
	JSR	INTERP
	DB	$6A,<D0488,>D0488	; LAW	D0488
	DB	$2C,$80,$02		; CW	640
	DB	$48			; ISGE
	DB	$4C,<C0447,>C0447	; SKPFLS	C0447
; 1880:         return parse_err_11(@ctag_full)
	DB	$26,<D0800,>D0800	; LA	D0800
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5C			; RET
; 1881:     fin
C0447:
C0448:
; 1882:     codetag = codetag + 1
	DB	$6A,<D0488,>D0488	; LAW	D0488
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0488,>D0488	; SAW	D0488
; 1883:     ctag_value:[codetag] = 0
	DB	$2C,$00,$08		; CW	2048
	DB	$6A,<D0488,>D0488	; LAW	D0488
	DB	$1E			; IDXW
	DB	$00			; ZERO
	DB	$72			; SW
; 1884:     ctag_flags.[codetag] = 0
	DB	$2C,$80,$0D		; CW	3456
	DB	$6A,<D0488,>D0488	; LAW	D0488
	DB	$02			; IDXB
	DB	$00			; ZERO
	DB	$70			; SB
; 1885:     return codetag ? is_ctag
	DB	$6A,<D0488,>D0488	; LAW	D0488
	DB	$2C,$00,$80		; CW	32768
	DB	$16			; IOR
	DB	$5C			; RET
; 1886: end
; 1887: defopt ctag_resolve_21(tag, addr)
C0449:					; ctag_resolve_21()
					; tag = 2
					; addr = 4
; 1888:     word updtptr, nextptr
					; updtptr = 6
					; nextptr = 8
; 1889: 
; 1890:     tag = tag & mask_ctag
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
; 1891:     if ctag_flags.[tag] & resolved
	LDA	#$80
	STA	ESTKL,X
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
	JMP	C0451
:
; 1892:         return parse_err_11(@dup_id)
	DEX
	LDA	#<D0538
	STA	ESTKL,X
	LDA	#>D0538
	STA	ESTKH,X
	JSR	C0441
	JMP	LEAVE
; 1893:     fin
C0451:
C0452:
; 1894:     updtptr = ctag_value:[tag]
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$08
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
; 1895:     while updtptr
	INX
C0453:
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
	JMP	C0454
:
; 1896:         ;
; 1897:         ; Update list of addresses needing resolution
; 1898:         ;
; 1899:         nextptr  = *updtptr
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
; 1900:         *updtptr = addr
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
; 1901:         updtptr  = nextptr
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
; 1902:     loop
	INX
	JMP	C0453
C0454:
; 1903:     ctag_value:[tag] = addr
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$08
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
; 1904:     ctag_flags.[tag] = ctag_flags.[tag] ? resolved
	DEX
	LDA	#$80
	STA	ESTKL,X
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
	LDA	#$80
	STA	ESTKL,X
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
; 1905:     return 0
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 1906: end
; 1907: defopt emit_byte_10(bval)
C0455:					; emit_byte_10()
					; bval = 2
; 1908:     ^codeptr = bval
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
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
; 1909:     codeptr  = codeptr + 1
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0490
	LDA	ESTKH,X
	STA	D0490+1
; 1910: end
	INX
	JMP	LEAVE
; 1911: defopt emit_word_10(wval)
C0457:					; emit_word_10()
					; wval = 2
; 1912:     *codeptr = wval
	LDY	#4
	LDA	#1
	JSR	ENTER
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
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
; 1913:     codeptr  = codeptr + 2
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0490
	LDA	ESTKH,X
	STA	D0490+1
; 1914: end
	INX
	JMP	LEAVE
; 1915: def emit_fill_10(size)
C0459:					; emit_fill_10()
					; size = 2
; 1916:     memset(0, codeptr, size)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$00			; ZERO
	DB	$6A,<D0490,>D0490	; LAW	D0490
	DB	$66,$02			; LLW	2
	DB	$54,<C0011,>C0011	; CALL	C0011
; 1917:     codeptr = codeptr + size
	DB	$6A,<D0490,>D0490	; LAW	D0490
	DB	$66,$02			; LLW	2
	DB	$02			; ADD
	DB	$7A,<D0490,>D0490	; SAW	D0490
; 1918: end
	DB	$5A			; LEAVE
; 1919: def emit_codetag_10(tag)
C0461:					; emit_codetag_10()
					; tag = 2
; 1920:     drop ctag_resolve_21(tag, codeptr)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$6A,<D0490,>D0490	; LAW	D0490
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 1921: end
	DB	$5A			; LEAVE
; 1922: defopt emit_op_10(op)
C0463:					; emit_op_10()
					; op = 2
; 1923:     lastop   = op
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
	STA	D0494
; 1924:     ^codeptr = op
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
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
; 1925:     codeptr  = codeptr + 1
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0490
	LDA	ESTKH,X
	STA	D0490+1
; 1926: end
	INX
	JMP	LEAVE
; 1927: def emit_tag_10(tag)
C0465:					; emit_tag_10()
					; tag = 2
; 1928:     word updtptr
					; updtptr = 4
; 1929: 
; 1930:     if tag & is_ctag
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$80		; CW	32768
	DB	$14			; BAND
	DB	$4C,<C0467,>C0467	; SKPFLS	C0467
; 1931:         tag = tag & mask_ctag
	DB	$66,$02			; LLW	2
	DB	$2C,$FF,$7F		; CW	32767
	DB	$14			; BAND
	DB	$76,$02			; SLW	2
; 1932:         updtptr = ctag_value:[tag]
	DB	$2C,$00,$08		; CW	2048
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$76,$04			; SLW	4
; 1933:         if !(ctag_flags.[tag] & resolved)
	DB	$2C,$80,$0D		; CW	3456
	DB	$66,$02			; LLW	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0469,>C0469	; SKPFLS	C0469
; 1934:             ;
; 1935:             ; Add to list of tags needing resolution
; 1936:             ;
; 1937:             ctag_value:[tag] = codeptr
	DB	$2C,$00,$08		; CW	2048
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$6A,<D0490,>D0490	; LAW	D0490
	DB	$72			; SW
; 1938:         fin
C0469:
C0470:
; 1939:         emit_word_10(updtptr)
	DB	$66,$04			; LLW	4
	DB	$54,<C0457,>C0457	; CALL	C0457
; 1940:     else
	DB	$50,<C0468,>C0468	; SKIP	C0468
C0467:
; 1941:         emit_word_10(tag + codebuff)
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$A8		; CW	43008
	DB	$02			; ADD
	DB	$54,<C0457,>C0457	; CALL	C0457
; 1942:     fin
C0468:
; 1943: end
	DB	$5A			; LEAVE
; 1944: def emit_iddata_30(value, size, namestr)
C0471:					; emit_iddata_30()
					; value = 2
					; size = 4
					; namestr = 6
; 1945:     emit_fill_10(size)
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$04			; LLW	4
	DB	$54,<C0459,>C0459	; CALL	C0459
; 1946: end
	DB	$5A			; LEAVE
; 1947: def emit_data_41(vartype, consttype, constval, constsize)
C0473:					; emit_data_41()
					; vartype = 2
					; consttype = 4
					; constval = 6
					; constsize = 8
; 1948:     byte i
					; i = 10
; 1949:     word size, chrptr
					; size = 11
					; chrptr = 13
; 1950: 
; 1951:     if consttype == 0
	JSR	INTERP
	DB	$58,$0F,$04		; ENTER	15,4
	DB	$66,$04			; LLW	4
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0475,>C0475	; SKPFLS	C0475
; 1952:         size = constsize
	DB	$66,$08			; LLW	8
	DB	$76,$0B			; SLW	11
; 1953:         emit_fill_10(constsize)
	DB	$66,$08			; LLW	8
	DB	$54,<C0459,>C0459	; CALL	C0459
; 1954:     elsif consttype == STR_TYPE
	DB	$50,<C0476,>C0476	; SKIP	C0476
C0475:
	DB	$66,$04			; LLW	4
	DB	$2A,$80			; CB	128
	DB	$40			; ISEQ
	DB	$4C,<C0477,>C0477	; SKPFLS	C0477
; 1955:         size = constsize
	DB	$66,$08			; LLW	8
	DB	$76,$0B			; SLW	11
; 1956:         chrptr = constval
	DB	$66,$06			; LLW	6
	DB	$76,$0D			; SLW	13
; 1957:         constsize = constsize - 1
	DB	$66,$08			; LLW	8
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$08			; SLW	8
; 1958:         emit_byte_10(constsize)
	DB	$66,$08			; LLW	8
	DB	$54,<C0455,>C0455	; CALL	C0455
; 1959:         while constsize > 0
C0478:
	DB	$66,$08			; LLW	8
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0479,>C0479	; SKPFLS	C0479
; 1960:             emit_byte_10(^chrptr)
	DB	$66,$0D			; LLW	13
	DB	$60			; LB
	DB	$54,<C0455,>C0455	; CALL	C0455
; 1961:             chrptr    = chrptr + 1
	DB	$66,$0D			; LLW	13
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$76,$0D			; SLW	13
; 1962:             constsize = constsize - 1
	DB	$66,$08			; LLW	8
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$08			; SLW	8
; 1963:         loop
	DB	$50,<C0478,>C0478	; SKIP	C0478
C0479:
; 1964:     else
	DB	$50,<C0476,>C0476	; SKIP	C0476
C0477:
; 1965:         if vartype == WORD_TYPE
	DB	$66,$02			; LLW	2
	DB	$2A,$04			; CB	4
	DB	$40			; ISEQ
	DB	$4C,<C0480,>C0480	; SKPFLS	C0480
; 1966:             size = 2
	DB	$2A,$02			; CB	2
	DB	$76,$0B			; SLW	11
; 1967:             emit_word_10(constval)
	DB	$66,$06			; LLW	6
	DB	$54,<C0457,>C0457	; CALL	C0457
; 1968:         else
	DB	$50,<C0481,>C0481	; SKIP	C0481
C0480:
; 1969:             size = 1
	DB	$2A,$01			; CB	1
	DB	$76,$0B			; SLW	11
; 1970:             emit_byte_10(constval)
	DB	$66,$06			; LLW	6
	DB	$54,<C0455,>C0455	; CALL	C0455
; 1971:         fin
C0481:
; 1972:     fin
C0476:
; 1973:     return size
	DB	$66,$0B			; LLW	11
	DB	$5A			; LEAVE
; 1974: end
; 1975: def emit_const_10(cval)
C0482:					; emit_const_10()
					; cval = 2
; 1976:     if cval == 0
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0484,>C0484	; SKPFLS	C0484
; 1977:         emit_op_10($00)
	DB	$00			; ZERO
	DB	$54,<C0463,>C0463	; CALL	C0463
; 1978:     elsif cval > 0 and cval < 256
	DB	$50,<C0485,>C0485	; SKIP	C0485
C0484:
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$01		; CW	256
	DB	$46			; ISLT
	DB	$24			; LAND
	DB	$4C,<C0486,>C0486	; SKPFLS	C0486
; 1979:         emit_op_10($2A)
	DB	$2A,$2A			; CB	42
	DB	$54,<C0463,>C0463	; CALL	C0463
; 1980:         emit_byte_10(cval)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 1981:     else
	DB	$50,<C0485,>C0485	; SKIP	C0485
C0486:
; 1982:         emit_op_10($2C)
	DB	$2A,$2C			; CB	44
	DB	$54,<C0463,>C0463	; CALL	C0463
; 1983:         emit_word_10(cval)
	DB	$66,$02			; LLW	2
	DB	$54,<C0457,>C0457	; CALL	C0457
; 1984:     fin
C0485:
; 1985: end
	DB	$5A			; LEAVE
; 1986: def emit_lb
C0487:					; emit_lb()
; 1987:     emit_op_10($60)
	JSR	INTERP
	DB	$2A,$60			; CB	96
	DB	$54,<C0463,>C0463	; CALL	C0463
; 1988: end
	DB	$5C			; RET
; 1989: def emit_lw
C0489:					; emit_lw()
; 1990:     emit_op_10($62)
	JSR	INTERP
	DB	$2A,$62			; CB	98
	DB	$54,<C0463,>C0463	; CALL	C0463
; 1991: end
	DB	$5C			; RET
; 1992: def emit_llb_10(index)
C0491:					; emit_llb_10()
					; index = 2
; 1993:     emit_op_10($64)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$64			; CB	100
	DB	$54,<C0463,>C0463	; CALL	C0463
; 1994:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 1995: end
	DB	$5A			; LEAVE
; 1996: def emit_llw_10(index)
C0493:					; emit_llw_10()
					; index = 2
; 1997:     emit_op_10($66)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$66			; CB	102
	DB	$54,<C0463,>C0463	; CALL	C0463
; 1998:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 1999: end
	DB	$5A			; LEAVE
; 2000: def emit_lab_10(tag)
C0495:					; emit_lab_10()
					; tag = 2
; 2001:     emit_op_10($68)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$68			; CB	104
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2002:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2003: end
	DB	$5A			; LEAVE
; 2004: def emit_law_10(tag)
C0497:					; emit_law_10()
					; tag = 2
; 2005:     emit_op_10($6A)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6A			; CB	106
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2006:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2007: end
	DB	$5A			; LEAVE
; 2008: def emit_sb
C0499:					; emit_sb()
; 2009:     emit_op_10($70)
	JSR	INTERP
	DB	$2A,$70			; CB	112
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2010: end
	DB	$5C			; RET
; 2011: def emit_sw
C0501:					; emit_sw()
; 2012:     emit_op_10($72)
	JSR	INTERP
	DB	$2A,$72			; CB	114
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2013: end
	DB	$5C			; RET
; 2014: def emit_slb_10(index)
C0503:					; emit_slb_10()
					; index = 2
; 2015:     emit_op_10($74)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$74			; CB	116
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2016:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2017: end
	DB	$5A			; LEAVE
; 2018: def emit_slw_10(index)
C0505:					; emit_slw_10()
					; index = 2
; 2019:     emit_op_10($76)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$76			; CB	118
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2020:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2021: end
	DB	$5A			; LEAVE
; 2022: def emit_dlb_10(index)
C0507:					; emit_dlb_10()
					; index = 2
; 2023:     emit_op_10($6C)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6C			; CB	108
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2024:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2025: end
	DB	$5A			; LEAVE
; 2026: def emit_dlw_10(index)
C0509:					; emit_dlw_10()
					; index = 2
; 2027:     emit_op_10($6E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$6E			; CB	110
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2028:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2029: end
	DB	$5A			; LEAVE
; 2030: def emit_sab_10(tag)
C0511:					; emit_sab_10()
					; tag = 2
; 2031:     emit_op_10($78)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$78			; CB	120
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2032:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2033: end
	DB	$5A			; LEAVE
; 2034: def emit_saw_10(tag)
C0513:					; emit_saw_10()
					; tag = 2
; 2035:     emit_op_10($7A)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7A			; CB	122
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2036:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2037: end
	DB	$5A			; LEAVE
; 2038: def emit_dab_10(tag)
C0515:					; emit_dab_10()
					; tag = 2
; 2039:     emit_op_10($7C)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7C			; CB	124
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2040:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2041: end
	DB	$5A			; LEAVE
; 2042: def emit_daw_10(tag)
C0517:					; emit_daw_10()
					; tag = 2
; 2043:     emit_op_10($7E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$7E			; CB	126
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2044:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2045: end
	DB	$5A			; LEAVE
; 2046: def emit_call_10(tag)
C0519:					; emit_call_10()
					; tag = 2
; 2047:     emit_op_10($54)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$54			; CB	84
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2048:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2049: end
	DB	$5A			; LEAVE
; 2050: def emit_ical
C0521:					; emit_ical()
; 2051:     emit_op_10($56)
	JSR	INTERP
	DB	$2A,$56			; CB	86
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2052: end
	DB	$5C			; RET
; 2053: def emit_push
C0523:					; emit_push()
; 2054:     emit_op_10($34)
	JSR	INTERP
	DB	$2A,$34			; CB	52
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2055: end
	DB	$5C			; RET
; 2056: def emit_pull
C0525:					; emit_pull()
; 2057:     ;
; 2058:     ; Skip if last op was push
; 2059:     ;
; 2060:     if lastop == $34
	JSR	INTERP
	DB	$68,<D0494,>D0494	; LAB	D0494
	DB	$2A,$34			; CB	52
	DB	$40			; ISEQ
	DB	$4C,<C0527,>C0527	; SKPFLS	C0527
; 2061:         codeptr = codeptr - 1
	DB	$6A,<D0490,>D0490	; LAW	D0490
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0490,>D0490	; SAW	D0490
; 2062:         lastop = $FF
	DB	$2A,$FF			; CB	255
	DB	$78,<D0494,>D0494	; SAB	D0494
; 2063:     else
	DB	$50,<C0528,>C0528	; SKIP	C0528
C0527:
; 2064:         emit_op_10($36)
	DB	$2A,$36			; CB	54
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2065:     fin
C0528:
; 2066: end
	DB	$5C			; RET
; 2067: def emit_localaddr_10(index)
C0529:					; emit_localaddr_10()
					; index = 2
; 2068:     emit_op_10($28)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$28			; CB	40
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2069:     emit_byte_10(index)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2070: end
	DB	$5A			; LEAVE
; 2071: def emit_globaladdr_10(tag)
C0531:					; emit_globaladdr_10()
					; tag = 2
; 2072:     emit_op_10($26)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$26			; CB	38
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2073:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2074: end
	DB	$5A			; LEAVE
; 2075: def emit_indexbyte
C0533:					; emit_indexbyte()
; 2076:     emit_op_10($02)
	JSR	INTERP
	DB	$2A,$02			; CB	2
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2077: end
	DB	$5C			; RET
; 2078: def emit_indexword
C0535:					; emit_indexword()
; 2079:     emit_op_10($1E)
	JSR	INTERP
	DB	$2A,$1E			; CB	30
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2080: end
	DB	$5C			; RET
; 2081: defopt emit_unaryop_11(op)
C0537:					; emit_unaryop_11()
					; op = 2
; 2082:     when op
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
; 2083:         is NEG_TKN
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
:	JMP	C0540
:
; 2084:             emit_op_10($10)
	DEX
	LDA	#$10
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2085:         is COMP_TKN
	JMP	C0539
C0540:
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
:	JMP	C0541
:
; 2086:             emit_op_10($12)
	DEX
	LDA	#$12
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2087:         is LOGIC_NOT_TKN
	JMP	C0539
C0541:
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
:	JMP	C0542
:
; 2088:             emit_op_10($20)
	DEX
	LDA	#$20
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2089:         is INC_TKN
	JMP	C0539
C0542:
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
:	JMP	C0543
:
; 2090:             emit_op_10($0C)
	DEX
	LDA	#$0C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2091:         is DEC_TKN
	JMP	C0539
C0543:
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
:	JMP	C0544
:
; 2092:             emit_op_10($0E)
	DEX
	LDA	#$0E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2093:         is BPTR_TKN
	JMP	C0539
C0544:
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
:	JMP	C0545
:
; 2094:             emit_op_10($60)
	DEX
	LDA	#$60
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2095:         is WPTR_TKN
	JMP	C0539
C0545:
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
:	JMP	C0546
:
; 2096:             emit_op_10($62)
	DEX
	LDA	#$62
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2097:         otherwise
	JMP	C0539
C0546:
; 2098:             return FALSE
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 2099:     wend
C0539:
; 2100:     return TRUE
	LDA	#$FF
	STA	ESTKL,X
	STA	ESTKH,X
	JMP	LEAVE
; 2101: end
; 2102: defopt emit_binaryop_11(op)
C0548:					; emit_binaryop_11()
					; op = 2
; 2103:     when op
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
; 2104:         is MUL_TKN
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
:	JMP	C0551
:
; 2105:             ;
; 2106:             ; Replace MUL 2 with SHL 1
; 2107:             ;
; 2108:             if lastop == $2A and ^(codeptr - 1) == 2 ; CB 2
	DEX
	LDA	D0494
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
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
	JMP	C0552
:
; 2109:                 codeptr = codeptr - 1
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0490
	LDA	ESTKH,X
	STA	D0490+1
; 2110:                 emit_byte_10(1) ; CB 1
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0455
; 2111:                 emit_op_10($1A) ; SHL
	DEX
	LDA	#$1A
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0463
; 2112:             else
	JMP	C0553
C0552:
; 2113:                 emit_op_10($06)
	DEX
	LDA	#$06
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0463
; 2114:             fin
C0553:
; 2115:         is DIV_TKN
	JMP	C0550
C0551:
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
:	JMP	C0554
:
; 2116:             ;
; 2117:             ; Replace DIV 2 with SHR 1
; 2118:             ;
; 2119:             if lastop == $2A and ^(codeptr - 1) == 2 ; CB 2
	DEX
	LDA	D0494
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
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
	JMP	C0555
:
; 2120:                 codeptr = codeptr - 1
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0490
	LDA	ESTKH,X
	STA	D0490+1
; 2121:                 emit_byte_10(1) ; CB 1
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0455
; 2122:                 emit_op_10($1C) ; SHR
	DEX
	LDA	#$1C
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0463
; 2123:             else
	JMP	C0556
C0555:
; 2124:                 emit_op_10($08)
	DEX
	LDA	#$08
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0463
; 2125:             fin
C0556:
; 2126:         is MOD_TKN
	JMP	C0550
C0554:
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
:	JMP	C0557
:
; 2127:             emit_op_10($0A)
	DEX
	LDA	#$0A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2128:         is ADD_TKN
	JMP	C0550
C0557:
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
:	JMP	C0558
:
; 2129:             ;
; 2130:             ; Replace ADD 1 with INCR
; 2131:             ;
; 2132:             if lastop == $2A and ^(codeptr - 1) == 1 ; CB 1
	DEX
	LDA	D0494
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
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
	JMP	C0559
:
; 2133:                 codeptr = codeptr - 2
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0490
	LDA	ESTKH,X
	STA	D0490+1
; 2134:                 emit_op_10($0C) ; INC_OP
	LDA	#$0C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2135:             else
	JMP	C0560
C0559:
; 2136:                 emit_op_10($02)
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0463
; 2137:             fin
C0560:
; 2138:         is SUB_TKN
	JMP	C0550
C0558:
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
:	JMP	C0561
:
; 2139:             ;
; 2140:             ; Replace SUB 1 with DECR
; 2141:             ;
; 2142:             if lastop == $2A and ^(codeptr - 1)  == 1 ; CB 1
	DEX
	LDA	D0494
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$2A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
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
	JMP	C0562
:
; 2143:                 codeptr = codeptr - 2
	DEX
	LDA	D0490
	STA	ESTKL,X
	LDA	D0490+1
	STA	ESTKH,X
	DEX
	LDA	#$02
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0490
	LDA	ESTKH,X
	STA	D0490+1
; 2144:                 emit_op_10($0E) ; DEC_OP
	LDA	#$0E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2145:             else
	JMP	C0563
C0562:
; 2146:                 emit_op_10($04)
	DEX
	LDA	#$04
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	C0463
; 2147:             fin
C0563:
; 2148:         is SHL_TKN
	JMP	C0550
C0561:
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
:	JMP	C0564
:
; 2149:             emit_op_10($1A)
	DEX
	LDA	#$1A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2150:         is SHR_TKN
	JMP	C0550
C0564:
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
:	JMP	C0565
:
; 2151:             emit_op_10($1C)
	DEX
	LDA	#$1C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2152:         is AND_TKN
	JMP	C0550
C0565:
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
:	JMP	C0566
:
; 2153:             emit_op_10($14)
	DEX
	LDA	#$14
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2154:         is OR_TKN
	JMP	C0550
C0566:
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
:	JMP	C0567
:
; 2155:             emit_op_10($16)
	DEX
	LDA	#$16
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2156:         is EOR_TKN
	JMP	C0550
C0567:
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
:	JMP	C0568
:
; 2157:             emit_op_10($18)
	DEX
	LDA	#$18
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2158:         is EQ_TKN
	JMP	C0550
C0568:
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
:	JMP	C0569
:
; 2159:             emit_op_10($40)
	DEX
	LDA	#$40
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2160:         is NE_TKN
	JMP	C0550
C0569:
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
:	JMP	C0570
:
; 2161:             emit_op_10($42)
	DEX
	LDA	#$42
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2162:         is GE_TKN
	JMP	C0550
C0570:
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
:	JMP	C0571
:
; 2163:             emit_op_10($48)
	DEX
	LDA	#$48
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2164:         is LT_TKN
	JMP	C0550
C0571:
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
:	JMP	C0572
:
; 2165:             emit_op_10($46)
	DEX
	LDA	#$46
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2166:         is GT_TKN
	JMP	C0550
C0572:
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
:	JMP	C0573
:
; 2167:             emit_op_10($44)
	DEX
	LDA	#$44
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2168:         is LE_TKN
	JMP	C0550
C0573:
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
:	JMP	C0574
:
; 2169:             emit_op_10($4A)
	DEX
	LDA	#$4A
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2170:         is LOGIC_OR_TKN
	JMP	C0550
C0574:
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
:	JMP	C0575
:
; 2171:             emit_op_10($22)
	DEX
	LDA	#$22
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2172:         is LOGIC_AND_TKN
	JMP	C0550
C0575:
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
:	JMP	C0576
:
; 2173:             emit_op_10($24)
	DEX
	LDA	#$24
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0463
; 2174:         is COMMA_TKN
	JMP	C0550
C0576:
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
:	JMP	C0577
:
; 2175:             ; Do nothing except move to next stanza in expression
; 2176:         otherwise
	JMP	C0550
C0577:
; 2177:             return FALSE
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 2178:     wend
C0550:
; 2179:     return TRUE
	LDA	#$FF
	STA	ESTKL,X
	STA	ESTKH,X
	JMP	LEAVE
; 2180: end
; 2181: def emit_brtru_10(tag)
C0579:					; emit_brtru_10()
					; tag = 2
; 2182:     emit_op_10($4E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$4E			; CB	78
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2183:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2184: end
	DB	$5A			; LEAVE
; 2185: def emit_brfls_10(tag)
C0581:					; emit_brfls_10()
					; tag = 2
; 2186:     emit_op_10($4C)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$4C			; CB	76
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2187:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2188: end
	DB	$5A			; LEAVE
; 2189: def emit_brgt_10(tag)
C0583:					; emit_brgt_10()
					; tag = 2
; 2190:     emit_op_10($3A)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$3A			; CB	58
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2191:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2192: end
	DB	$5A			; LEAVE
; 2193: def emit_brlt_10(tag)
C0585:					; emit_brlt_10()
					; tag = 2
; 2194:     emit_op_10($38)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$38			; CB	56
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2195:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2196: end
	DB	$5A			; LEAVE
; 2197: def emit_brne_10(tag)
C0587:					; emit_brne_10()
					; tag = 2
; 2198:     emit_op_10($3E)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$3E			; CB	62
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2199:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2200: end
	DB	$5A			; LEAVE
; 2201: def emit_jump_10(tag)
C0589:					; emit_jump_10()
					; tag = 2
; 2202:     emit_op_10($50)
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$50			; CB	80
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2203:     emit_tag_10(tag)
	DB	$66,$02			; LLW	2
	DB	$54,<C0465,>C0465	; CALL	C0465
; 2204: end
	DB	$5A			; LEAVE
; 2205: def emit_drop
C0591:					; emit_drop()
; 2206:     emit_op_10($30)
	JSR	INTERP
	DB	$2A,$30			; CB	48
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2207: end
	DB	$5C			; RET
; 2208: def emit_swap
C0593:					; emit_swap()
; 2209:     emit_op_10($2E)
	JSR	INTERP
	DB	$2A,$2E			; CB	46
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2210: end
	DB	$5C			; RET
; 2211: def emit_leave_10(framesize)
C0595:					; emit_leave_10()
					; framesize = 2
; 2212:     if framesize > 2
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$44			; ISGT
	DB	$4C,<C0597,>C0597	; SKPFLS	C0597
; 2213:         emit_op_10($5A)
	DB	$2A,$5A			; CB	90
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2214:     else
	DB	$50,<C0598,>C0598	; SKIP	C0598
C0597:
; 2215:         emit_op_10($5C)
	DB	$2A,$5C			; CB	92
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2216:     fin
C0598:
; 2217: end
	DB	$5A			; LEAVE
; 2218: def emit_enter_20(framesize, cparams)
C0599:					; emit_enter_20()
					; framesize = 2
					; cparams = 4
; 2219:     emit_byte_10(emit_enter_20.[0])
	JSR	INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$26,<C0599,>C0599	; LA	C0599
	DB	$00			; ZERO
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2220:     emit_byte_10(emit_enter_20.[1])
	DB	$26,<C0599,>C0599	; LA	C0599
	DB	$2A,$01			; CB	1
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2221:     emit_byte_10(emit_enter_20.[2])
	DB	$26,<C0599,>C0599	; LA	C0599
	DB	$2A,$02			; CB	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2222:     if framesize > 2
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$44			; ISGT
	DB	$4C,<C0601,>C0601	; SKPFLS	C0601
; 2223:         emit_op_10($58)
	DB	$2A,$58			; CB	88
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2224:         emit_byte_10(framesize)
	DB	$66,$02			; LLW	2
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2225:         emit_byte_10(cparams)
	DB	$66,$04			; LLW	4
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2226:     fin
C0601:
C0602:
; 2227: end
	DB	$5A			; LEAVE
; 2228: def emit_start
C0603:					; emit_start()
; 2229:     ;
; 2230:     ; Save address
; 2231:     ;
; 2232:     entrypoint = codeptr
	JSR	INTERP
	DB	$6A,<D0490,>D0490	; LAW	D0490
	DB	$7A,<D0492,>D0492	; SAW	D0492
; 2233:     emit_byte_10(emit_start.[0])
	DB	$26,<C0603,>C0603	; LA	C0603
	DB	$00			; ZERO
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2234:     emit_byte_10(emit_start.[1])
	DB	$26,<C0603,>C0603	; LA	C0603
	DB	$2A,$01			; CB	1
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2235:     emit_byte_10(emit_start.[2])
	DB	$26,<C0603,>C0603	; LA	C0603
	DB	$2A,$02			; CB	2
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0455,>C0455	; CALL	C0455
; 2236: end
	DB	$5C			; RET
; 2237: def emit_exit
C0605:					; emit_exit()
; 2238:     emit_op_10($00)
	JSR	INTERP
	DB	$00			; ZERO
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2239:     emit_op_10($5C)
	DB	$2A,$5C			; CB	92
	DB	$54,<C0463,>C0463	; CALL	C0463
; 2240: end
	DB	$5C			; RET
; 2241: ;
; 2242: ; Lexical anaylzer
; 2243: ;
; 2244: ;def isalpha_11(c)
; 2245: ;   if c >= 'A' and c <= 'Z'
; 2246: ;       return TRUE
; 2247: ;   elsif c >= 'a' and c <= 'z'
; 2248: ;       return TRUE
; 2249: ;   elsif c == '_'
; 2250: ;       return TRUE
; 2251: ;   fin
; 2252: ;   return FALSE
; 2253: ;end
; 2254: asm isalpha_11
C0607:					; isalpha_11()
; 2255:         LDY     #$00
        LDY     #$00
; 2256:         LDA     ESTKL,X
        LDA     ESTKL,X
; 2257:         CMP     #'A'
        CMP     #'A'
; 2258:         BCC     ISALRET
        BCC     ISALRET
; 2259:         CMP     #'Z'+1
        CMP     #'Z'+1
; 2260:         BCS     :+
        BCS     :+
; 2261:         DEY
        DEY
; 2262:         BNE     ISALRET
        BNE     ISALRET
; 2263: :       CMP     #'a'
:       CMP     #'a'
; 2264:         BCC     ISALRET
        BCC     ISALRET
; 2265:         CMP     #'z'+1
        CMP     #'z'+1
; 2266:         BCS     :+
        BCS     :+
; 2267:         DEY
        DEY
; 2268:         BNE     ISALRET
        BNE     ISALRET
; 2269: :       CMP     #'_'
:       CMP     #'_'
; 2270:         BNE     ISALRET
        BNE     ISALRET
; 2271:         DEY
        DEY
; 2272: ISALRET:
ISALRET:
; 2273:         STY     ESTKL,X
        STY     ESTKL,X
; 2274:         STY     ESTKH,X
        STY     ESTKH,X
; 2275:         RTS
        RTS
; 2276: end
	RTS
; 2277: ;def isnum_11(c)
; 2278: ;   if c >= '0' and c <= '9'
; 2279: ;       return TRUE
; 2280: ;   fin
; 2281: ;   return FALSE
; 2282: ;end
; 2283: asm isnum_11
C0609:					; isnum_11()
; 2284:         LDY     #$00
        LDY     #$00
; 2285:         LDA     ESTKL,X
        LDA     ESTKL,X
; 2286:         CMP     #'0'
        CMP     #'0'
; 2287:         BCC     :+
        BCC     :+
; 2288:         CMP     #'9'+1
        CMP     #'9'+1
; 2289:         BCS     :+
        BCS     :+
; 2290:         DEY
        DEY
; 2291: :       STY     ESTKL,X
:       STY     ESTKL,X
; 2292:         STY     ESTKH,X
        STY     ESTKH,X
; 2293:         RTS
        RTS
; 2294: end
	RTS
; 2295: ;def isalphanum_11(c)
; 2296: ;   if c >= 'A' and c <= 'Z'
; 2297: ;       return TRUE
; 2298: ;   elsif c >= '0' and c <= '9'
; 2299: ;       return TRUE
; 2300: ;   elsif c >= 'a' and c <= 'z'
; 2301: ;       return TRUE
; 2302: ;   elsif c == '_'
; 2303: ;       return TRUE
; 2304: ;   fin
; 2305: ;   return FALSE
; 2306: ;end
; 2307: asm isalphanum_11
C0611:					; isalphanum_11()
; 2308:         LDY     #$00
        LDY     #$00
; 2309:         LDA     ESTKL,X
        LDA     ESTKL,X
; 2310:         CMP     #'0'
        CMP     #'0'
; 2311:         BCC     ISANRET
        BCC     ISANRET
; 2312:         CMP     #'9'+1
        CMP     #'9'+1
; 2313:         BCS     :+
        BCS     :+
; 2314:         DEY
        DEY
; 2315:         BNE     ISANRET
        BNE     ISANRET
; 2316: :       CMP     #'A'
:       CMP     #'A'
; 2317:         BCC     ISANRET
        BCC     ISANRET
; 2318:         CMP     #'Z'+1
        CMP     #'Z'+1
; 2319:         BCS     :+
        BCS     :+
; 2320:         DEY
        DEY
; 2321:         BNE     ISANRET
        BNE     ISANRET
; 2322: :       CMP     #'a'
:       CMP     #'a'
; 2323:         BCC     :+
        BCC     :+
; 2324:         CMP     #'z'+1
        CMP     #'z'+1
; 2325:         BCS     ISANRET
        BCS     ISANRET
; 2326:         DEY
        DEY
; 2327:         BNE     ISANRET
        BNE     ISANRET
; 2328: :       CMP     #'_'
:       CMP     #'_'
; 2329:         BNE     ISANRET
        BNE     ISANRET
; 2330:         DEY
        DEY
; 2331: ISANRET:
ISANRET:
; 2332:         STY     ESTKL,X
        STY     ESTKL,X
; 2333:         STY     ESTKH,X
        STY     ESTKH,X
; 2334:         RTS
        RTS
; 2335: end
	RTS
; 2336: defopt keymatch_21(chrptr, len)
C0613:					; keymatch_21()
					; chrptr = 2
					; len = 4
; 2337:     byte i, keypos
					; i = 6
					; keypos = 7
; 2338: 
; 2339:     keypos = 0
	LDY	#8
	LDA	#2
	JSR	ENTER
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
; 2340:     while keywrds[keypos] < len
	INX
C0615:
	DEX
	LDA	#<D0212
	STA	ESTKL,X
	LDA	#>D0212
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
	JMP	C0616
:
; 2341:         keypos = keypos + keywrds[keypos] + 2
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0212
	STA	ESTKL,X
	LDA	#>D0212
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
; 2342:     loop
	INX
	JMP	C0615
C0616:
; 2343:     while keywrds[keypos] == len
C0617:
	DEX
	LDA	#<D0212
	STA	ESTKL,X
	LDA	#>D0212
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
	JMP	C0618
:
; 2344:         for i = 1 to len
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
C0620:
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
	JMP	C0619
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
; 2345:             if toupper_11((chrptr).[i - 1]) <> keywrds[keypos + i]
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
	JSR	C0127
	DEX
	LDA	#<D0212
	STA	ESTKL,X
	LDA	#>D0212
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
	JMP	C0621
:
; 2346:                 break
	JMP	C0619
; 2347:             fin
C0621:
C0622:
; 2348:         next
	JMP	C0620
C0619:
; 2349:         if i > len
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
	JMP	C0623
:
; 2350:             return keywrds[keypos + keywrds[keypos] + 1]
	DEX
	LDA	#<D0212
	STA	ESTKL,X
	LDA	#>D0212
	STA	ESTKH,X
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0212
	STA	ESTKL,X
	LDA	#>D0212
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
; 2351:         fin
C0623:
C0624:
; 2352:         keypos = keypos + keywrds[keypos] + 2
	DEX
	LDY	#$07
	LDA	(FRMP),Y
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#<D0212
	STA	ESTKL,X
	LDA	#>D0212
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
; 2353:     loop
	INX
	JMP	C0617
C0618:
; 2354:     return ID_TKN
	DEX
	LDA	#$D6
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
; 2355: end
; 2356: defopt skipspace_01
C0625:					; skipspace_01()
; 2357:     ;
; 2358:     ; Skip whitespace
; 2359:     ;
; 2360:     while ^scanptr and ^scanptr <= ' '
C0627:
	DEX
	LDA	D0499
	STA	ESTKL,X
	LDA	D0499+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	DEX
	LDA	D0499
	STA	ESTKL,X
	LDA	D0499+1
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
	JMP	C0628
:
; 2361:         scanptr = scanptr + 1
	DEX
	LDA	D0499
	STA	ESTKL,X
	LDA	D0499+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0499
	LDA	ESTKH,X
	STA	D0499+1
; 2362:     loop
	INX
	JMP	C0627
C0628:
; 2363:     tknptr = scanptr
	DEX
	LDA	D0499
	STA	ESTKL,X
	LDA	D0499+1
	STA	ESTKH,X
	LDA	ESTKL,X
	STA	D0501
	LDA	ESTKH,X
	STA	D0501+1
; 2364:     return !^scanptr or ^scanptr == ';'
	LDA	D0499
	STA	ESTKL,X
	LDA	D0499+1
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	JSR	NOT
	DEX
	LDA	D0499
	STA	ESTKL,X
	LDA	D0499+1
	STA	ESTKH,X
	JSR	LB
	DEX
	LDA	#$3B
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISEQ
	JSR	LOR
	RTS
; 2365: end
; 2366: def scan_01
C0629:					; scan_01()
; 2367:     ;
; 2368:     ; Scan for token based on first character
; 2369:     ;
; 2370:     if skipspace_01()
	JSR	INTERP
	DB	$54,<C0625,>C0625	; CALL	C0625
	DB	$4C,<C0631,>C0631	; SKPFLS	C0631
; 2371:         if token <> EOF_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$01			; CB	1
	DB	$42			; ISNE
	DB	$4C,<C0633,>C0633	; SKPFLS	C0633
; 2372:             token = EOL_TKN
	DB	$2A,$02			; CB	2
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2373:         fin
C0633:
C0634:
; 2374:     elsif isalpha_11(^scanptr)
	DB	$50,<C0632,>C0632	; SKIP	C0632
C0631:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$54,<C0607,>C0607	; CALL	C0607
	DB	$4C,<C0635,>C0635	; SKPFLS	C0635
; 2375:         ;
; 2376:         ; ID,    either variable name or reserved word
; 2377:         ;
; 2378:         repeat
C0637:
; 2379:             scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2380:         until !isalphanum_11(^scanptr)
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$54,<C0611,>C0611	; CALL	C0611
	DB	$20			; NOT
	DB	$4C,<C0637,>C0637	; SKPFLS	C0637
C0636:
; 2381:         tknlen = scanptr - tknptr;
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$04			; SUB
	DB	$78,<D0496,>D0496	; SAB	D0496
; 2382:         token = keymatch_21(tknptr, tknlen)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0613,>C0613	; CALL	C0613
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2383:     elsif isnum_11(^scanptr)
	DB	$50,<C0632,>C0632	; SKIP	C0632
C0635:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$54,<C0609,>C0609	; CALL	C0609
	DB	$4C,<C0638,>C0638	; SKPFLS	C0638
; 2384:         ;
; 2385:         ; Number constant
; 2386:         ;
; 2387:         token       = INT_TKN
	DB	$2A,$C9			; CB	201
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2388:         constval = 0
	DB	$00			; ZERO
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2389:         repeat
C0640:
; 2390:             constval = constval * 10 + ^scanptr - '0'
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$2A,$0A			; CB	10
	DB	$06			; MUL
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$02			; ADD
	DB	$2A,$30			; CB	48
	DB	$04			; SUB
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2391:             scanptr  = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2392:         until !isnum_11(^scanptr)
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$54,<C0609,>C0609	; CALL	C0609
	DB	$20			; NOT
	DB	$4C,<C0640,>C0640	; SKPFLS	C0640
C0639:
; 2393:     elsif ^scanptr == '$'
	DB	$50,<C0632,>C0632	; SKIP	C0632
C0638:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$24			; CB	36
	DB	$40			; ISEQ
	DB	$4C,<C0641,>C0641	; SKPFLS	C0641
; 2394:         ;
; 2395:         ; Hexadecimal constant
; 2396:         ;
; 2397:         token    = INT_TKN;
	DB	$2A,$C9			; CB	201
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2398:         constval = 0
	DB	$00			; ZERO
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2399:         repeat
C0643:
; 2400:             scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2401:             if ^scanptr >= '0' and ^scanptr <= '9'
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$30			; CB	48
	DB	$48			; ISGE
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$39			; CB	57
	DB	$4A			; ISLE
	DB	$24			; LAND
	DB	$4C,<C0644,>C0644	; SKPFLS	C0644
; 2402:                 constval = (constval << 4) + ^scanptr - '0'
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$2A,$04			; CB	4
	DB	$1A			; SHL
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$02			; ADD
	DB	$2A,$30			; CB	48
	DB	$04			; SUB
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2403:             elsif ^scanptr >= 'A' and ^scanptr <= 'F'
	DB	$50,<C0645,>C0645	; SKIP	C0645
C0644:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$41			; CB	65
	DB	$48			; ISGE
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$46			; CB	70
	DB	$4A			; ISLE
	DB	$24			; LAND
	DB	$4C,<C0646,>C0646	; SKPFLS	C0646
; 2404:                 constval = (constval << 4) + ^scanptr - '7'; 'A'-10
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$2A,$04			; CB	4
	DB	$1A			; SHL
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$02			; ADD
	DB	$2A,$37			; CB	55
	DB	$04			; SUB
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2405:             elsif ^scanptr >= 'a' and ^scanptr <= 'f'
	DB	$50,<C0645,>C0645	; SKIP	C0645
C0646:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$61			; CB	97
	DB	$48			; ISGE
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$66			; CB	102
	DB	$4A			; ISLE
	DB	$24			; LAND
	DB	$4C,<C0647,>C0647	; SKPFLS	C0647
; 2406:                 constval = (constval << 4) + ^scanptr - 'W'; 'a'-10
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$2A,$04			; CB	4
	DB	$1A			; SHL
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$02			; ADD
	DB	$2A,$57			; CB	87
	DB	$04			; SUB
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2407:             else
	DB	$50,<C0645,>C0645	; SKIP	C0645
C0647:
; 2408:                 break;
	DB	$50,<C0642,>C0642	; SKIP	C0642
; 2409:             fin
C0645:
; 2410:         until !^scanptr
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$20			; NOT
	DB	$4C,<C0643,>C0643	; SKPFLS	C0643
C0642:
; 2411:     elsif ^scanptr == $27 ; '
	DB	$50,<C0632,>C0632	; SKIP	C0632
C0641:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$27			; CB	39
	DB	$40			; ISEQ
	DB	$4C,<C0648,>C0648	; SKPFLS	C0648
; 2412:         ;
; 2413:         ; Character constant
; 2414:         ;
; 2415:         token = CHR_TKN
	DB	$2A,$C3			; CB	195
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2416:         if ^(scanptr + 1) <> $5C ; \
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$5C			; CB	92
	DB	$42			; ISNE
	DB	$4C,<C0649,>C0649	; SKPFLS	C0649
; 2417:             constval = ^(scanptr + 1)
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2418:             if ^(scanptr + 2) <> $27 ; '
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$27			; CB	39
	DB	$42			; ISNE
	DB	$4C,<C0651,>C0651	; SKPFLS	C0651
; 2419:                 return parse_err_11(@bad_cnst)
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5C			; RET
; 2420:             fin
C0651:
C0652:
; 2421:             scanptr = scanptr + 3
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2422:         else
	DB	$50,<C0650,>C0650	; SKIP	C0650
C0649:
; 2423:             when ^(scanptr + 2)
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
; 2424:                 is 'n'
	DB	$2A,$6E			; CB	110
	DB	$3E,<C0654,>C0654	; SKPNE	C0654
; 2425:                     constval = $0D
	DB	$2A,$0D			; CB	13
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2426:                 is 'r'
	DB	$50,<C0653,>C0653	; SKIP	C0653
C0654:
	DB	$2A,$72			; CB	114
	DB	$3E,<C0655,>C0655	; SKPNE	C0655
; 2427:                     constval = $0A
	DB	$2A,$0A			; CB	10
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2428:                 is 't'
	DB	$50,<C0653,>C0653	; SKIP	C0653
C0655:
	DB	$2A,$74			; CB	116
	DB	$3E,<C0656,>C0656	; SKPNE	C0656
; 2429:                     constval = $09
	DB	$2A,$09			; CB	9
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2430:                 otherwise
	DB	$50,<C0653,>C0653	; SKIP	C0653
C0656:
; 2431:                     constval = ^(scanptr + 2)
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2432:             wend
C0653:
	DB	$30			; DROP
; 2433:             if ^(scanptr + 3) <> $27 ; '
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$27			; CB	39
	DB	$42			; ISNE
	DB	$4C,<C0658,>C0658	; SKPFLS	C0658
; 2434:                 return parse_err_11(@bad_cnst)
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5C			; RET
; 2435:             fin
C0658:
C0659:
; 2436:             scanptr = scanptr + 4
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2437:         fin
C0650:
; 2438:     elsif ^scanptr == '"'
	DB	$50,<C0632,>C0632	; SKIP	C0632
C0648:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$22			; CB	34
	DB	$40			; ISEQ
	DB	$4C,<C0660,>C0660	; SKPFLS	C0660
; 2439:         ;
; 2440:         ; String constant
; 2441:         ;
; 2442:         token    = STR_TKN
	DB	$2A,$D3			; CB	211
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2443:         scanptr  = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2444:         constval = scanptr
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$7A,<D0505,>D0505	; SAW	D0505
; 2445:         while ^scanptr and ^scanptr <> '"'
C0661:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$22			; CB	34
	DB	$42			; ISNE
	DB	$24			; LAND
	DB	$4C,<C0662,>C0662	; SKPFLS	C0662
; 2446:             scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2447:         loop
	DB	$50,<C0661,>C0661	; SKIP	C0661
C0662:
; 2448:         if !^scanptr
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$20			; NOT
	DB	$4C,<C0663,>C0663	; SKPFLS	C0663
; 2449:             return parse_err_11(@bad_cnst)
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5C			; RET
; 2450:         fin
C0663:
C0664:
; 2451:         scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2452:     else
	DB	$50,<C0632,>C0632	; SKIP	C0632
C0660:
; 2453:         ;
; 2454:         ; Potential two and three character tokens
; 2455:         ;
; 2456:         when ^scanptr
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
; 2457:             is '>'
	DB	$2A,$3E			; CB	62
	DB	$3E,<C0666,>C0666	; SKPNE	C0666
; 2458:                 if ^(scanptr + 1) == '>'
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$3E			; CB	62
	DB	$40			; ISEQ
	DB	$4C,<C0667,>C0667	; SKPFLS	C0667
; 2459:                     token   = SHR_TKN
	DB	$2A,$D2			; CB	210
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2460:                     scanptr = scanptr + 2
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2461:                 elsif ^(scanptr + 1) == '='
	DB	$50,<C0668,>C0668	; SKIP	C0668
C0667:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$3D			; CB	61
	DB	$40			; ISEQ
	DB	$4C,<C0669,>C0669	; SKPFLS	C0669
; 2462:                     token   = GE_TKN
	DB	$2A,$C8			; CB	200
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2463:                     scanptr = scanptr + 2
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2464:                 else
	DB	$50,<C0668,>C0668	; SKIP	C0668
C0669:
; 2465:                     token   = GT_TKN
	DB	$2A,$BE			; CB	190
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2466:                     scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2467:                 fin
C0668:
; 2468:             is '<'
	DB	$50,<C0665,>C0665	; SKIP	C0665
C0666:
	DB	$2A,$3C			; CB	60
	DB	$3E,<C0670,>C0670	; SKPNE	C0670
; 2469:                 if ^(scanptr + 1) == '<'
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$3C			; CB	60
	DB	$40			; ISEQ
	DB	$4C,<C0671,>C0671	; SKPFLS	C0671
; 2470:                     token   = SHL_TKN
	DB	$2A,$CC			; CB	204
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2471:                     scanptr = scanptr + 2
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2472:                 elsif ^(scanptr + 1) == '='
	DB	$50,<C0672,>C0672	; SKIP	C0672
C0671:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$3D			; CB	61
	DB	$40			; ISEQ
	DB	$4C,<C0673,>C0673	; SKPFLS	C0673
; 2473:                     token   = LE_TKN
	DB	$2A,$C2			; CB	194
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2474:                     scanptr = scanptr + 2
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2475:                 elsif ^(scanptr + 1) == '>'
	DB	$50,<C0672,>C0672	; SKIP	C0672
C0673:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$3E			; CB	62
	DB	$40			; ISEQ
	DB	$4C,<C0674,>C0674	; SKPFLS	C0674
; 2476:                     token   = NE_TKN
	DB	$2A,$D5			; CB	213
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2477:                     scanptr = scanptr + 2
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2478:                 else
	DB	$50,<C0672,>C0672	; SKIP	C0672
C0674:
; 2479:                     token   = LT_TKN
	DB	$2A,$BC			; CB	188
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2480:                     scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2481:                 fin
C0672:
; 2482:             is '='
	DB	$50,<C0665,>C0665	; SKIP	C0665
C0670:
	DB	$2A,$3D			; CB	61
	DB	$3E,<C0675,>C0675	; SKPNE	C0675
; 2483:                 if ^(scanptr + 1) == '='
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$3D			; CB	61
	DB	$40			; ISEQ
	DB	$4C,<C0676,>C0676	; SKPFLS	C0676
; 2484:                     token   = EQ_TKN
	DB	$2A,$C5			; CB	197
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2485:                     scanptr = scanptr + 2;
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2486:                 elsif ^(scanptr + 1) == ','
	DB	$50,<C0677,>C0677	; SKIP	C0677
C0676:
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$2C			; CB	44
	DB	$40			; ISEQ
	DB	$4C,<C0678,>C0678	; SKPFLS	C0678
; 2487:                     token   = SETLIST_TKN
	DB	$2A,$B9			; CB	185
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2488:                     scanptr = scanptr + 2;
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2489:                 else
	DB	$50,<C0677,>C0677	; SKIP	C0677
C0678:
; 2490:                     token   = SET_TKN;
	DB	$2A,$BD			; CB	189
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2491:                     scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2492:                 fin
C0677:
; 2493:             otherwise
	DB	$50,<C0665,>C0665	; SKIP	C0665
C0675:
; 2494:                 ;
; 2495:                 ; Simple single character tokens
; 2496:                 ;
; 2497:                 token   = ^scanptr ? $80
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$16			; IOR
	DB	$78,<D0495,>D0495	; SAB	D0495
; 2498:                 scanptr = scanptr + 1
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2499:         wend
C0665:
	DB	$30			; DROP
; 2500:     fin
C0632:
; 2501:     tknlen = scanptr - tknptr
	DB	$6A,<D0499,>D0499	; LAW	D0499
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$04			; SUB
	DB	$78,<D0496,>D0496	; SAB	D0496
; 2502:     return token
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$5C			; RET
; 2503: end
; 2504: def rewind_10(ptr)
C0680:					; rewind_10()
					; ptr = 2
; 2505:     scanptr = ptr
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$7A,<D0499,>D0499	; SAW	D0499
; 2506: end
	DB	$5A			; LEAVE
; 2507: ;
; 2508: ; Get next line of input
; 2509: ;
; 2510: defopt nextln_01
C0682:					; nextln_01()
; 2511: ;   if ^keyboard == $A0
; 2512: ;        ^keystrobe
; 2513: ;        while ^keyboard < 128
; 2514: ;        loop
; 2515: ;        ^keystrobe
; 2516: ;    elsif ^keyboard == $82
; 2517: ;        lineno = numlines
; 2518: ;        ^keystrobe
; 2519: ;   fin
; 2520:     scanptr = inbuff
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$02
	STA	ESTKH,X
	LDA	ESTKL,X
	STA	D0499
	LDA	ESTKH,X
	STA	D0499+1
; 2521:     if lineno < numlines
	LDA	D0507
	STA	ESTKL,X
	LDA	D0507+1
	STA	ESTKH,X
	DEX
	LDA	D0206
	STA	ESTKL,X
	LDA	D0206+1
	STA	ESTKH,X
	JSR	ISLT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0684
:
; 2522:         cpyln_20(strlinbuf:[lineno], instr)
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$10
	STA	ESTKH,X
	DEX
	LDA	D0507
	STA	ESTKL,X
	LDA	D0507+1
	STA	ESTKH,X
	JSR	IDXW
	JSR	LW
	DEX
	LDA	#$FF
	STA	ESTKL,X
	LDA	#$01
	STA	ESTKH,X
	JSR	C0133
; 2523:         lineno = lineno + 1
	DEX
	LDA	D0507
	STA	ESTKL,X
	LDA	D0507+1
	STA	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0507
	LDA	ESTKH,X
	STA	D0507+1
; 2524:         if !(lineno & $0F)
	LDA	D0507
	STA	ESTKL,X
	LDA	D0507+1
	STA	ESTKH,X
	DEX
	LDA	#$0F
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	BAND
	JSR	NOT
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0686
:
; 2525:             cout('.')
	DEX
	LDA	#$2E
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0015
; 2526:         fin
C0686:
C0687:
; 2527:         ; cout('>')
; 2528:         ; prstr(instr)
; 2529:         ; crout()
; 2530:         drop scan_01()
	JSR	C0629
; 2531:     else
	INX
	JMP	C0685
C0684:
; 2532:         ^instr  = 0
	DEX
	LDA	#$FF
	STA	ESTKL,X
	LDA	#$01
	STA	ESTKH,X
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JSR	SB
; 2533:         ^inbuff = $00
	DEX
	STY	ESTKL,X
	LDA	#$02
	STA	ESTKH,X
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
	JSR	SB
; 2534:         token   = DONE_TKN
	DEX
	LDA	#$98
	STA	ESTKL,X
	STY	ESTKH,X
	LDA	ESTKL,X
	STA	D0495
; 2535:     fin
	INX
C0685:
; 2536:     return ^instr
	DEX
	LDA	#$FF
	STA	ESTKL,X
	LDA	#$01
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
	RTS
; 2537: end
; 2538: ;
; 2539: ; Alebraic op to stack op
; 2540: ;
; 2541: def push_op_21(op, prec)
C0688:					; push_op_21()
					; op = 2
					; prec = 4
; 2542:     opsp = opsp + 1
	JSR	INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0475,>D0475	; SAW	D0475
; 2543:     if opsp == 16
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$2A,$10			; CB	16
	DB	$40			; ISEQ
	DB	$4C,<C0690,>C0690	; SKPFLS	C0690
; 2544:         return parse_err_11(@estk_overflw)
	DB	$26,<D0681,>D0681	; LA	D0681
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2545:     fin
C0690:
C0691:
; 2546:     opstack[opsp]   = op
	DB	$26,<D0443,>D0443	; LA	D0443
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$70			; SB
; 2547:     precstack[opsp] = prec
	DB	$26,<D0459,>D0459	; LA	D0459
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$02			; IDXB
	DB	$66,$04			; LLW	4
	DB	$70			; SB
; 2548:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2549: end
; 2550: def pop_op_01
C0692:					; pop_op_01()
; 2551:     if opsp < 0
	JSR	INTERP
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0694,>C0694	; SKPFLS	C0694
; 2552:         return parse_err_11(@estk_underflw)
	DB	$26,<D0701,>D0701	; LA	D0701
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5C			; RET
; 2553:     fin
C0694:
C0695:
; 2554:     opsp = opsp - 1
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0475,>D0475	; SAW	D0475
; 2555:     return opstack[opsp + 1]
	DB	$26,<D0443,>D0443	; LA	D0443
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5C			; RET
; 2556: end
; 2557: def tos_op_01
C0696:					; tos_op_01()
; 2558:     if opsp < 0
	JSR	INTERP
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0698,>C0698	; SKPFLS	C0698
; 2559:         return 0
	DB	$00			; ZERO
	DB	$5C			; RET
; 2560:     fin
C0698:
C0699:
; 2561:     return opstack[opsp]
	DB	$26,<D0443,>D0443	; LA	D0443
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5C			; RET
; 2562: end
; 2563: def tos_op_prec_11(tos)
C0700:					; tos_op_prec_11()
					; tos = 2
; 2564:     if opsp <= tos
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$66,$02			; LLW	2
	DB	$4A			; ISLE
	DB	$4C,<C0702,>C0702	; SKPFLS	C0702
; 2565:         return 100
	DB	$2A,$64			; CB	100
	DB	$5A			; LEAVE
; 2566:     fin
C0702:
C0703:
; 2567:     return precstack[opsp]
	DB	$26,<D0459,>D0459	; LA	D0459
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$5A			; LEAVE
; 2568: end
; 2569: ;
; 2570: ; Symbol table
; 2571: ;
; 2572: defopt idmatch_41(nameptr, len, idptr, idcnt)
C0704:					; idmatch_41()
					; nameptr = 2
					; len = 4
					; idptr = 6
					; idcnt = 8
; 2573:     byte i
					; i = 10
; 2574: 
; 2575:     while idcnt
	LDY	#11
	LDA	#4
	JSR	ENTER
C0706:
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
	JMP	C0707
:
; 2576:         if len == (idptr).idname
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
	JMP	C0708
:
; 2577:             for i = 1 to len
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
C0711:
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
	JMP	C0710
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
; 2578:                 if (nameptr).[i - 1] <> (idptr).idname.[i]
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
	JMP	C0712
:
; 2579:                     break
	JMP	C0710
; 2580:                 fin
C0712:
C0713:
; 2581:             next
	JMP	C0711
C0710:
; 2582:             if i > len
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
	JMP	C0714
:
; 2583:                 return idptr
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JMP	LEAVE
; 2584:             fin
C0714:
C0715:
; 2585:         fin
C0708:
C0709:
; 2586:         idptr = idptr + (idptr).idname + idrecsz
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
; 2587:         idcnt = idcnt - 1
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
; 2588:     loop
	INX
	JMP	C0706
C0707:
; 2589:     return 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
; 2590: end
; 2591: ;def dumpsym_20(idptr, idcnt)
; 2592: ;   while idcnt
; 2593: ;       prword_10((idptr):idval)
; 2594: ;       cout(' ')
; 2595: ;       prbyte_10((idptr).idtype)
; 2596: ;       cout(' ')
; 2597: ;       prstr(@(idptr).idname)
; 2598: ;       cout('=')
; 2599: ;       if (idptr).idtype & ADDR_TYPE
; 2600: ;            if (idptr):idval & is_ctag
; 2601: ;                prword_10(ctag_value:[(idptr):idval & mask_ctag])
; 2602: ;            else
; 2603: ;                prword_10((idptr):idval + codebuff)
; 2604: ;            fin
; 2605: ;        else
; 2606: ;            prword_10((idptr):idval)
; 2607: ;        fin
; 2608: ;       crout()
; 2609: ;       idptr = idptr + (idptr).idname + idrecsz
; 2610: ;       idcnt = idcnt - 1
; 2611: ;   loop
; 2612: ;end
; 2613: def id_lookup_21(nameptr, len)
C0716:					; id_lookup_21()
					; nameptr = 2
					; len = 4
; 2614:     word idptr
					; idptr = 6
; 2615: 
; 2616:     idptr = idmatch_41(nameptr, len, idlocal_tbl, locals)
	JSR	INTERP
	DB	$58,$08,$02		; ENTER	8,2
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$1E		; CW	7680
	DB	$68,<D0483,>D0483	; LAB	D0483
	DB	$54,<C0704,>C0704	; CALL	C0704
	DB	$76,$06			; SLW	6
; 2617:     if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C0718,>C0718	; SKPFLS	C0718
; 2618:         return idptr
	DB	$66,$06			; LLW	6
	DB	$5A			; LEAVE
; 2619:     fin
C0718:
C0719:
; 2620:     idptr = idmatch_41(nameptr, len, idglobal_tbl, globals)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$16		; CW	5632
	DB	$6A,<D0477,>D0477	; LAW	D0477
	DB	$54,<C0704,>C0704	; CALL	C0704
	DB	$76,$06			; SLW	6
; 2621:     if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C0720,>C0720	; SKPFLS	C0720
; 2622:         return idptr
	DB	$66,$06			; LLW	6
	DB	$5A			; LEAVE
; 2623:     fin
C0720:
C0721:
; 2624:     return parse_err_11(@undecl_id)
	DB	$26,<D0559,>D0559	; LA	D0559
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2625: end
; 2626: def idglobal_lookup_21(nameptr, len)
C0722:					; idglobal_lookup_21()
					; nameptr = 2
					; len = 4
; 2627:     return idmatch_41(nameptr, len, idglobal_tbl, globals)
	JSR	INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$16		; CW	5632
	DB	$6A,<D0477,>D0477	; LAW	D0477
	DB	$54,<C0704,>C0704	; CALL	C0704
	DB	$5A			; LEAVE
; 2628: end
; 2629: def idlocal_add_41(namestr, len, type, size)
C0724:					; idlocal_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; size = 8
; 2630:     if idmatch_41(namestr, len, @idlocal_tbl, locals)
	JSR	INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$1E		; CW	7680
	DB	$68,<D0483,>D0483	; LAB	D0483
	DB	$54,<C0704,>C0704	; CALL	C0704
	DB	$4C,<C0726,>C0726	; SKPFLS	C0726
; 2631:         return parse_err_11(@dup_id)
	DB	$26,<D0538,>D0538	; LA	D0538
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2632:     fin
C0726:
C0727:
; 2633:     (lastlocal):idval  = framesize
	DB	$6A,<D0486,>D0486	; LAW	D0486
	DB	$6A,<D0484,>D0484	; LAW	D0484
	DB	$72			; SW
; 2634:     (lastlocal).idtype = type ? LOCAL_TYPE
	DB	$6A,<D0486,>D0486	; LAW	D0486
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$2A,$10			; CB	16
	DB	$16			; IOR
	DB	$70			; SB
; 2635:     nametostr_30(namestr, len, lastlocal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0486,>D0486	; LAW	D0486
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0125,>C0125	; CALL	C0125
; 2636:     locals    = locals + 1
	DB	$68,<D0483,>D0483	; LAB	D0483
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0483,>D0483	; SAB	D0483
; 2637:     lastlocal = lastlocal + idrecsz + len
	DB	$6A,<D0486,>D0486	; LAW	D0486
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0486,>D0486	; SAW	D0486
; 2638:     if lastlocal > idlocal_tbl + idlocal_tblsz
	DB	$6A,<D0486,>D0486	; LAW	D0486
	DB	$2C,$00,$1E		; CW	7680
	DB	$2C,$00,$02		; CW	512
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0728,>C0728	; SKPFLS	C0728
; 2639:         prstr(@local_sym_overflw)
	DB	$26,<D0772,>D0772	; LA	D0772
	DB	$54,<C0019,>C0019	; CALL	C0019
; 2640:         exit
	DB	$54,<C0023,>C0023	; CALL	C0023
; 2641:     fin
C0728:
C0729:
; 2642:     framesize = framesize + size
	DB	$6A,<D0484,>D0484	; LAW	D0484
	DB	$66,$08			; LLW	8
	DB	$02			; ADD
	DB	$7A,<D0484,>D0484	; SAW	D0484
; 2643:     if framesize > 255
	DB	$6A,<D0484,>D0484	; LAW	D0484
	DB	$2A,$FF			; CB	255
	DB	$44			; ISGT
	DB	$4C,<C0730,>C0730	; SKPFLS	C0730
; 2644:         prstr(@local_overflw)
	DB	$26,<D0722,>D0722	; LA	D0722
	DB	$54,<C0019,>C0019	; CALL	C0019
; 2645:         return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2646:     fin
C0730:
C0731:
; 2647:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2648: end
; 2649: def iddata_add_41(namestr, len, type, size)
C0732:					; iddata_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; size = 8
; 2650:     if idmatch_41(namestr, len, idglobal_tbl, globals)
	JSR	INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$16		; CW	5632
	DB	$6A,<D0477,>D0477	; LAW	D0477
	DB	$54,<C0704,>C0704	; CALL	C0704
	DB	$4C,<C0734,>C0734	; SKPFLS	C0734
; 2651:         return parse_err_11(@dup_id)
	DB	$26,<D0538,>D0538	; LA	D0538
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2652:     fin
C0734:
C0735:
; 2653:     (lastglobal):idval  = datasize
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$6A,<D0479,>D0479	; LAW	D0479
	DB	$72			; SW
; 2654:     (lastglobal).idtype = type
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$70			; SB
; 2655:     nametostr_30(namestr, len, lastglobal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0125,>C0125	; CALL	C0125
; 2656:     emit_iddata_30(datasize, size, lastglobal + idname)
	DB	$6A,<D0479,>D0479	; LAW	D0479
	DB	$66,$08			; LLW	8
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0471,>C0471	; CALL	C0471
; 2657:     globals    = globals + 1
	DB	$6A,<D0477,>D0477	; LAW	D0477
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0477,>D0477	; SAW	D0477
; 2658:     lastglobal = lastglobal + idrecsz + len
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0481,>D0481	; SAW	D0481
; 2659:     if lastglobal > idglobal_tbl + idglobal_tblsz
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2C,$00,$16		; CW	5632
	DB	$2C,$00,$08		; CW	2048
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0736,>C0736	; SKPFLS	C0736
; 2660:         prstr(@global_sym_overflw)
	DB	$26,<D0743,>D0743	; LA	D0743
	DB	$54,<C0019,>C0019	; CALL	C0019
; 2661:         exit
	DB	$54,<C0023,>C0023	; CALL	C0023
; 2662:     fin
C0736:
C0737:
; 2663:     datasize = datasize + size
	DB	$6A,<D0479,>D0479	; LAW	D0479
	DB	$66,$08			; LLW	8
	DB	$02			; ADD
	DB	$7A,<D0479,>D0479	; SAW	D0479
; 2664:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2665: end
; 2666: def iddata_size_30(type, varsize, initsize)
C0738:					; iddata_size_30()
					; type = 2
					; varsize = 4
					; initsize = 6
; 2667:     if varsize > initsize
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$04			; LLW	4
	DB	$66,$06			; LLW	6
	DB	$44			; ISGT
	DB	$4C,<C0740,>C0740	; SKPFLS	C0740
; 2668:         datasize = datasize + emit_data_41(0, 0, 0, varsize - initsize)
	DB	$6A,<D0479,>D0479	; LAW	D0479
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$66,$04			; LLW	4
	DB	$66,$06			; LLW	6
	DB	$04			; SUB
	DB	$54,<C0473,>C0473	; CALL	C0473
	DB	$02			; ADD
	DB	$7A,<D0479,>D0479	; SAW	D0479
; 2669:     else
	DB	$50,<C0741,>C0741	; SKIP	C0741
C0740:
; 2670:         datasize = datasize + initsize
	DB	$6A,<D0479,>D0479	; LAW	D0479
	DB	$66,$06			; LLW	6
	DB	$02			; ADD
	DB	$7A,<D0479,>D0479	; SAW	D0479
; 2671:     fin
C0741:
; 2672: ;   if datasize <> codeptr - codebuff
; 2673: ;       prstr(@emiterr)
; 2674: ;       keyin_01()
; 2675: ;   fin
; 2676: end
	DB	$5A			; LEAVE
; 2677: def idglobal_add_41(namestr, len, type, value)
C0742:					; idglobal_add_41()
					; namestr = 2
					; len = 4
					; type = 6
					; value = 8
; 2678:     if idmatch_41(namestr, len, idglobal_tbl, globals)
	JSR	INTERP
	DB	$58,$0A,$04		; ENTER	10,4
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2C,$00,$16		; CW	5632
	DB	$6A,<D0477,>D0477	; LAW	D0477
	DB	$54,<C0704,>C0704	; CALL	C0704
	DB	$4C,<C0744,>C0744	; SKPFLS	C0744
; 2679:         return parse_err_11(@dup_id)
	DB	$26,<D0538,>D0538	; LA	D0538
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2680:     fin
C0744:
C0745:
; 2681:     (lastglobal):idval  = value
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$66,$08			; LLW	8
	DB	$72			; SW
; 2682:     (lastglobal).idtype = type
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$06			; LLW	6
	DB	$70			; SB
; 2683:     nametostr_30(namestr, len, lastglobal + idname)
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2A,$03			; CB	3
	DB	$02			; ADD
	DB	$54,<C0125,>C0125	; CALL	C0125
; 2684:     globals    = globals + 1
	DB	$6A,<D0477,>D0477	; LAW	D0477
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0477,>D0477	; SAW	D0477
; 2685:     lastglobal = lastglobal + idrecsz + len
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$7A,<D0481,>D0481	; SAW	D0481
; 2686:     if lastglobal > idglobal_tbl + idglobal_tblsz
	DB	$6A,<D0481,>D0481	; LAW	D0481
	DB	$2C,$00,$16		; CW	5632
	DB	$2C,$00,$08		; CW	2048
	DB	$02			; ADD
	DB	$44			; ISGT
	DB	$4C,<C0746,>C0746	; SKPFLS	C0746
; 2687:         prstr(@global_sym_overflw)
	DB	$26,<D0743,>D0743	; LA	D0743
	DB	$54,<C0019,>C0019	; CALL	C0019
; 2688:         exit
	DB	$54,<C0023,>C0023	; CALL	C0023
; 2689:     fin
C0746:
C0747:
; 2690:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 2691: end
; 2692: def idfunc_add_31(namestr, len, tag)
C0748:					; idfunc_add_31()
					; namestr = 2
					; len = 4
					; tag = 6
; 2693:     return idglobal_add_41(namestr, len, FUNC_TYPE, tag)
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2A,$08			; CB	8
	DB	$66,$06			; LLW	6
	DB	$54,<C0742,>C0742	; CALL	C0742
	DB	$5A			; LEAVE
; 2694: end
; 2695: def idconst_add_31(namestr, len, value)
C0750:					; idconst_add_31()
					; namestr = 2
					; len = 4
					; value = 6
; 2696:     return idglobal_add_41(namestr, len, CONST_TYPE, value)
	JSR	INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$02			; LLW	2
	DB	$66,$04			; LLW	4
	DB	$2A,$01			; CB	1
	DB	$66,$06			; LLW	6
	DB	$54,<C0742,>C0742	; CALL	C0742
	DB	$5A			; LEAVE
; 2697: end
; 2698: def idglobal_init
C0752:					; idglobal_init()
; 2699:     word ctag
					; ctag = 2
; 2700: 
; 2701:     lineno       = 0
	JSR	INTERP
	DB	$58,$04,$00		; ENTER	4,0
	DB	$00			; ZERO
	DB	$7A,<D0507,>D0507	; SAW	D0507
; 2702:     parserr      = 0
	DB	$00			; ZERO
	DB	$78,<D0498,>D0498	; SAB	D0498
; 2703:     codeptr      = codebuff
	DB	$2C,$00,$A8		; CW	43008
	DB	$7A,<D0490,>D0490	; SAW	D0490
; 2704:     lastop       = $FF
	DB	$2A,$FF			; CB	255
	DB	$78,<D0494,>D0494	; SAB	D0494
; 2705:     entrypoint   = 0
	DB	$00			; ZERO
	DB	$7A,<D0492,>D0492	; SAW	D0492
; 2706:     datasize     = 0
	DB	$00			; ZERO
	DB	$7A,<D0479,>D0479	; SAW	D0479
; 2707:     globals      = 0
	DB	$00			; ZERO
	DB	$7A,<D0477,>D0477	; SAW	D0477
; 2708:     lastglobal   = idglobal_tbl
	DB	$2C,$00,$16		; CW	5632
	DB	$7A,<D0481,>D0481	; SAW	D0481
; 2709:     codetag      = -1
	DB	$2C,$FF,$FF		; CW	-1
	DB	$7A,<D0488,>D0488	; SAW	D0488
; 2710:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2711:     drop idfunc_add_31(@runtime0 + 1, runtime0, ctag)
	DB	$26,<D1000,>D1000	; LA	D1000
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1000,>D1000	; LAB	D1000
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2712:     drop idfunc_add_31(@RUNTIME0 + 1, RUNTIME0, ctag)
	DB	$26,<D1008,>D1008	; LA	D1008
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1008,>D1008	; LAB	D1008
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2713:     drop ctag_resolve_21(ctag, @romcall)
	DB	$66,$02			; LLW	2
	DB	$26,<C0007,>C0007	; LA	C0007
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2714:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2715:     drop idfunc_add_31(@runtime1 + 1, runtime1, ctag)
	DB	$26,<D1016,>D1016	; LA	D1016
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1016,>D1016	; LAB	D1016
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2716:     drop idfunc_add_31(@RUNTIME1 + 1, RUNTIME1, ctag)
	DB	$26,<D1024,>D1024	; LA	D1024
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1024,>D1024	; LAB	D1024
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2717:     drop ctag_resolve_21(ctag, @syscall)
	DB	$66,$02			; LLW	2
	DB	$26,<C0009,>C0009	; LA	C0009
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2718:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2719:     drop idfunc_add_31(@runtime2 + 1, runtime2, ctag)
	DB	$26,<D1032,>D1032	; LA	D1032
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1032,>D1032	; LAB	D1032
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2720:     drop idfunc_add_31(@RUNTIME2 + 1, RUNTIME2, ctag)
	DB	$26,<D1039,>D1039	; LA	D1039
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1039,>D1039	; LAB	D1039
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2721:     drop ctag_resolve_21(ctag, @memset)
	DB	$66,$02			; LLW	2
	DB	$26,<C0011,>C0011	; LA	C0011
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2722:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2723:     drop idfunc_add_31(@runtime3 + 1, runtime3, ctag)
	DB	$26,<D1046,>D1046	; LA	D1046
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1046,>D1046	; LAB	D1046
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2724:     drop idfunc_add_31(@RUNTIME3 + 1, RUNTIME3, ctag)
	DB	$26,<D1053,>D1053	; LA	D1053
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1053,>D1053	; LAB	D1053
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2725:     drop ctag_resolve_21(ctag, @memcpy)
	DB	$66,$02			; LLW	2
	DB	$26,<C0013,>C0013	; LA	C0013
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2726:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2727:     drop idfunc_add_31(@runtime4 + 1, runtime4, ctag)
	DB	$26,<D1060,>D1060	; LA	D1060
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1060,>D1060	; LAB	D1060
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2728:     drop idfunc_add_31(@RUNTIME4 + 1, RUNTIME4, ctag)
	DB	$26,<D1065,>D1065	; LA	D1065
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1065,>D1065	; LAB	D1065
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2729:     drop ctag_resolve_21(ctag, @cout)
	DB	$66,$02			; LLW	2
	DB	$26,<C0015,>C0015	; LA	C0015
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2730:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2731:     drop idfunc_add_31(@runtime5 + 1, runtime5, ctag)
	DB	$26,<D1070,>D1070	; LA	D1070
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1070,>D1070	; LAB	D1070
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2732:     drop idfunc_add_31(@RUNTIME5 + 1, RUNTIME5, ctag)
	DB	$26,<D1074,>D1074	; LA	D1074
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1074,>D1074	; LAB	D1074
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2733:     drop ctag_resolve_21(ctag, @cin)
	DB	$66,$02			; LLW	2
	DB	$26,<C0017,>C0017	; LA	C0017
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2734:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2735:     drop idfunc_add_31(@runtime6 + 1, runtime6, ctag)
	DB	$26,<D1078,>D1078	; LA	D1078
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1078,>D1078	; LAB	D1078
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2736:     drop idfunc_add_31(@RUNTIME6 + 1, RUNTIME6, ctag)
	DB	$26,<D1084,>D1084	; LA	D1084
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1084,>D1084	; LAB	D1084
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2737:     drop ctag_resolve_21(ctag, @prstr)
	DB	$66,$02			; LLW	2
	DB	$26,<C0019,>C0019	; LA	C0019
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2738:     ctag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$02			; SLW	2
; 2739:     drop idfunc_add_31(@runtime7 + 1, runtime7, ctag)
	DB	$26,<D1090,>D1090	; LA	D1090
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1090,>D1090	; LAB	D1090
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2740:     drop idfunc_add_31(@RUNTIME7 + 1, RUNTIME7, ctag)
	DB	$26,<D1096,>D1096	; LA	D1096
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D1096,>D1096	; LAB	D1096
	DB	$66,$02			; LLW	2
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 2741:     drop ctag_resolve_21(ctag, @rdstr)
	DB	$66,$02			; LLW	2
	DB	$26,<C0021,>C0021	; LA	C0021
	DB	$54,<C0449,>C0449	; CALL	C0449
	DB	$30			; DROP
; 2742: end
	DB	$5A			; LEAVE
; 2743: def idlocal_init
C0754:					; idlocal_init()
; 2744:     locals    = 0
	JSR	INTERP
	DB	$00			; ZERO
	DB	$78,<D0483,>D0483	; SAB	D0483
; 2745:     framesize = 2
	DB	$2A,$02			; CB	2
	DB	$7A,<D0484,>D0484	; SAW	D0484
; 2746:     lastlocal = idlocal_tbl
	DB	$2C,$00,$1E		; CW	7680
	DB	$7A,<D0486,>D0486	; SAW	D0486
; 2747: end
	DB	$5C			; RET
; 2748: ;
; 2749: ; Parser
; 2750: ;
; 2751: def parse_term_01
C0756:					; parse_term_01()
; 2752:     when scan_01()
	JSR	INTERP
	DB	$54,<C0629,>C0629	; CALL	C0629
; 2753:         is ID_TKN
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0759,>C0759	; SKPNE	C0759
; 2754:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2755:         is INT_TKN
	DB	$50,<C0758,>C0758	; SKIP	C0758
C0759:
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0760,>C0760	; SKPNE	C0760
; 2756:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2757:         is CHR_TKN
	DB	$50,<C0758,>C0758	; SKIP	C0758
C0760:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0761,>C0761	; SKPNE	C0761
; 2758:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2759:         is STR_TKN
	DB	$50,<C0758,>C0758	; SKIP	C0758
C0761:
	DB	$2A,$D3			; CB	211
	DB	$3E,<C0762,>C0762	; SKPNE	C0762
; 2760:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2761:         is OPEN_PAREN_TKN
	DB	$50,<C0758,>C0758	; SKIP	C0758
C0762:
	DB	$2A,$A8			; CB	168
	DB	$3E,<C0763,>C0763	; SKPNE	C0763
; 2762:             if !parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$20			; NOT
	DB	$4C,<C0764,>C0764	; SKPFLS	C0764
; 2763:                 return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5C			; RET
; 2764:             fin
C0764:
C0765:
; 2765:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C0766,>C0766	; SKPFLS	C0766
; 2766:                 return parse_err_11(@no_close_paren)
	DB	$30			; DROP
	DB	$26,<D0820,>D0820	; LA	D0820
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5C			; RET
; 2767:             fin
C0766:
C0767:
; 2768:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2769:     wend
	DB	$50,<C0758,>C0758	; SKIP	C0758
C0763:
C0758:
	DB	$30			; DROP
; 2770:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 2771: end
; 2772: def parse_constval_21(valptr, sizeptr)
C0769:					; parse_constval_21()
					; valptr = 2
					; sizeptr = 4
; 2773:     byte mod, type
					; mod = 6
					; type = 7
; 2774:     word idptr
					; idptr = 8
; 2775: 
; 2776:     mod         = 0
	JSR	INTERP
	DB	$58,$0A,$02		; ENTER	10,2
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 2777:     type        = 0
	DB	$00			; ZERO
	DB	$74,$07			; SLB	7
; 2778:     *valptr = 0
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$72			; SW
; 2779:     while !parse_term_01()
C0771:
	DB	$54,<C0756,>C0756	; CALL	C0756
	DB	$20			; NOT
	DB	$4C,<C0772,>C0772	; SKPFLS	C0772
; 2780:         when token
	DB	$68,<D0495,>D0495	; LAB	D0495
; 2781:             is SUB_TKN
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0774,>C0774	; SKPNE	C0774
; 2782:                 mod = mod ? 1
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2783:             is COMP_TKN
	DB	$50,<C0773,>C0773	; SKIP	C0773
C0774:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0775,>C0775	; SKPNE	C0775
; 2784:                 mod = mod ? 2
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2785:             is LOGIC_NOT_TKN
	DB	$50,<C0773,>C0773	; SKIP	C0773
C0775:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0776,>C0776	; SKPNE	C0776
; 2786:                 mod = mod ? 4
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2787:             is AT_TKN
	DB	$50,<C0773,>C0773	; SKIP	C0773
C0776:
	DB	$2A,$C0			; CB	192
	DB	$3E,<C0777,>C0777	; SKPNE	C0777
; 2788:                 mod = mod ? 8
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2789:             otherwise
	DB	$50,<C0773,>C0773	; SKIP	C0773
C0777:
; 2790:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2791:         wend
C0773:
	DB	$30			; DROP
; 2792:     loop
	DB	$50,<C0771,>C0771	; SKIP	C0771
C0772:
; 2793:     when token
	DB	$68,<D0495,>D0495	; LAB	D0495
; 2794:         is STR_TKN
	DB	$2A,$D3			; CB	211
	DB	$3E,<C0780,>C0780	; SKPNE	C0780
; 2795:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$72			; SW
; 2796:             ^sizeptr = tknlen - 1
	DB	$66,$04			; LLW	4
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
; 2797:             type         = STR_TYPE
	DB	$2A,$80			; CB	128
	DB	$74,$07			; SLB	7
; 2798:             if mod
	DB	$64,$06			; LLB	6
	DB	$4C,<C0781,>C0781	; SKPFLS	C0781
; 2799:                 return parse_err_11(@bad_op)
	DB	$30			; DROP
	DB	$26,<D0628,>D0628	; LA	D0628
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2800:             fin
C0781:
C0782:
; 2801:         is CHR_TKN
	DB	$50,<C0779,>C0779	; SKIP	C0779
C0780:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0783,>C0783	; SKPNE	C0783
; 2802:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$72			; SW
; 2803:             ^sizeptr = 1
	DB	$66,$04			; LLW	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
; 2804:             type         = BYTE_TYPE
	DB	$2A,$02			; CB	2
	DB	$74,$07			; SLB	7
; 2805:         is INT_TKN
	DB	$50,<C0779,>C0779	; SKIP	C0779
C0783:
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0784,>C0784	; SKPNE	C0784
; 2806:             *valptr  = constval
	DB	$66,$02			; LLW	2
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$72			; SW
; 2807:             ^sizeptr = 2
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$70			; SB
; 2808:             type         = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$07			; SLB	7
; 2809:         is ID_TKN
	DB	$50,<C0779,>C0779	; SKIP	C0779
C0784:
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0785,>C0785	; SKPNE	C0785
; 2810:             ^sizeptr = 2
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$70			; SB
; 2811:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0716,>C0716	; CALL	C0716
	DB	$76,$08			; SLW	8
; 2812:             if !idptr
	DB	$66,$08			; LLW	8
	DB	$20			; NOT
	DB	$4C,<C0786,>C0786	; SKPFLS	C0786
; 2813:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2814:             fin
C0786:
C0787:
; 2815:             type    = (idptr).idtype
	DB	$66,$08			; LLW	8
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$07			; SLB	7
; 2816:             *valptr = (idptr):idval
	DB	$66,$02			; LLW	2
	DB	$66,$08			; LLW	8
	DB	$62			; LW
	DB	$72			; SW
; 2817:             if type & VAR_TYPE and !(mod & 8)
	DB	$64,$07			; LLB	7
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$24			; LAND
	DB	$4C,<C0788,>C0788	; SKPFLS	C0788
; 2818:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2819:             fin
C0788:
C0789:
; 2820:         otherwise
	DB	$50,<C0779,>C0779	; SKIP	C0779
C0785:
; 2821:             return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2822:     wend
C0779:
	DB	$30			; DROP
; 2823:     if mod & 1
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0791,>C0791	; SKPFLS	C0791
; 2824:         *valptr = -*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$10			; NEG
	DB	$72			; SW
; 2825:     fin
C0791:
C0792:
; 2826:     if mod & 2
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0793,>C0793	; SKPFLS	C0793
; 2827:         *valptr = #*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$12			; COMP
	DB	$72			; SW
; 2828:     fin
C0793:
C0794:
; 2829:     if mod & 4
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0795,>C0795	; SKPFLS	C0795
; 2830:         *valptr = !*valptr
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$62			; LW
	DB	$20			; NOT
	DB	$72			; SW
; 2831:     fin
C0795:
C0796:
; 2832:     return type
	DB	$64,$07			; LLB	7
	DB	$5A			; LEAVE
; 2833: end
; 2834: def ispostop_01
C0797:					; ispostop_01()
; 2835:     when token
	JSR	INTERP
	DB	$68,<D0495,>D0495	; LAB	D0495
; 2836:         is OPEN_PAREN_TKN
	DB	$2A,$A8			; CB	168
	DB	$3E,<C0800,>C0800	; SKPNE	C0800
; 2837:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2838:         is OPEN_BRACKET_TKN
	DB	$50,<C0799,>C0799	; SKIP	C0799
C0800:
	DB	$2A,$DB			; CB	219
	DB	$3E,<C0801,>C0801	; SKPNE	C0801
; 2839:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2840:         is DOT_TKN
	DB	$50,<C0799,>C0799	; SKIP	C0799
C0801:
	DB	$2A,$AE			; CB	174
	DB	$3E,<C0802,>C0802	; SKPNE	C0802
; 2841:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2842:         is COLON_TKN
	DB	$50,<C0799,>C0799	; SKIP	C0799
C0802:
	DB	$2A,$BA			; CB	186
	DB	$3E,<C0803,>C0803	; SKPNE	C0803
; 2843:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 2844:     wend
	DB	$50,<C0799,>C0799	; SKIP	C0799
C0803:
C0799:
	DB	$30			; DROP
; 2845:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 2846: end
; 2847: def parse_value_11(rvalue)
C0805:					; parse_value_11()
					; rvalue = 2
; 2848:     byte cparams, deref, type, emit_val
					; cparams = 4
					; deref = 5
					; type = 6
					; emit_val = 7
; 2849:     word optos, idptr, value
					; optos = 8
					; idptr = 10
					; value = 12
; 2850:     byte elem_type, elem_size
					; elem_type = 14
					; elem_size = 15
; 2851:     word elem_offset
					; elem_offset = 16
; 2852: 
; 2853:     deref    = rvalue
	JSR	INTERP
	DB	$58,$12,$01		; ENTER	18,1
	DB	$66,$02			; LLW	2
	DB	$74,$05			; SLB	5
; 2854:     optos    = opsp
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$76,$08			; SLW	8
; 2855:     type     = 0
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 2856:     emit_val = 0
	DB	$00			; ZERO
	DB	$74,$07			; SLB	7
; 2857:     value    = 0
	DB	$00			; ZERO
	DB	$76,$0C			; SLW	12
; 2858: 
; 2859:     ;
; 2860:     ; Parse pre-ops
; 2861:     ;
; 2862:     while !parse_term_01()
C0807:
	DB	$54,<C0756,>C0756	; CALL	C0756
	DB	$20			; NOT
	DB	$4C,<C0808,>C0808	; SKPFLS	C0808
; 2863:         when token
	DB	$68,<D0495,>D0495	; LAB	D0495
; 2864:             is ADD_TKN
	DB	$2A,$AB			; CB	171
	DB	$3E,<C0810,>C0810	; SKPNE	C0810
; 2865:             is BPTR_TKN
	DB	$50,<C0809,>C0809	; SKIP	C0809
C0810:
	DB	$2A,$DE			; CB	222
	DB	$3E,<C0811,>C0811	; SKPNE	C0811
; 2866:                 if deref
	DB	$64,$05			; LLB	5
	DB	$4C,<C0812,>C0812	; SKPFLS	C0812
; 2867:                     drop push_op_21(token, 0)
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$00			; ZERO
	DB	$54,<C0688,>C0688	; CALL	C0688
	DB	$30			; DROP
; 2868:                 else
	DB	$50,<C0813,>C0813	; SKIP	C0813
C0812:
; 2869:                     type = type ? BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2870:                     deref = deref + 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$05			; SLB	5
; 2871:                 fin
C0813:
; 2872:             is WPTR_TKN
	DB	$50,<C0809,>C0809	; SKIP	C0809
C0811:
	DB	$2A,$AA			; CB	170
	DB	$3E,<C0814,>C0814	; SKPNE	C0814
; 2873:                 if deref
	DB	$64,$05			; LLB	5
	DB	$4C,<C0815,>C0815	; SKPFLS	C0815
; 2874:                     drop push_op_21(token, 0)
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$00			; ZERO
	DB	$54,<C0688,>C0688	; CALL	C0688
	DB	$30			; DROP
; 2875:                 else
	DB	$50,<C0816,>C0816	; SKIP	C0816
C0815:
; 2876:                     type = type ? WPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$40			; CB	64
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2877:                     deref = deref + 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$05			; SLB	5
; 2878:                 fin
C0816:
; 2879:             is AT_TKN
	DB	$50,<C0809,>C0809	; SKIP	C0809
C0814:
	DB	$2A,$C0			; CB	192
	DB	$3E,<C0817,>C0817	; SKPNE	C0817
; 2880:                 deref = deref - 1
	DB	$64,$05			; LLB	5
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$05			; SLB	5
; 2881:             is SUB_TKN
	DB	$50,<C0809,>C0809	; SKIP	C0809
C0817:
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0818,>C0818	; SKPNE	C0818
; 2882:                 drop push_op_21(token, 0)
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$00			; ZERO
	DB	$54,<C0688,>C0688	; CALL	C0688
	DB	$30			; DROP
; 2883:             is COMP_TKN
	DB	$50,<C0809,>C0809	; SKIP	C0809
C0818:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0819,>C0819	; SKPNE	C0819
; 2884:                 drop push_op_21(token, 0)
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$00			; ZERO
	DB	$54,<C0688,>C0688	; CALL	C0688
	DB	$30			; DROP
; 2885:             is LOGIC_NOT_TKN
	DB	$50,<C0809,>C0809	; SKIP	C0809
C0819:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0820,>C0820	; SKPNE	C0820
; 2886:                 drop push_op_21(token, 0)
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$00			; ZERO
	DB	$54,<C0688,>C0688	; CALL	C0688
	DB	$30			; DROP
; 2887:             otherwise
	DB	$50,<C0809,>C0809	; SKIP	C0809
C0820:
; 2888:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2889:         wend
C0809:
	DB	$30			; DROP
; 2890:     loop
	DB	$50,<C0807,>C0807	; SKIP	C0807
C0808:
; 2891:     ;
; 2892:     ; Determine terminal type
; 2893:     ;
; 2894:     when token
	DB	$68,<D0495,>D0495	; LAB	D0495
; 2895:         is INT_TKN
	DB	$2A,$C9			; CB	201
	DB	$3E,<C0823,>C0823	; SKPNE	C0823
; 2896:             type  = type ? CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2897:             value = constval
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$76,$0C			; SLW	12
; 2898:         is CHR_TKN
	DB	$50,<C0822,>C0822	; SKIP	C0822
C0823:
	DB	$2A,$C3			; CB	195
	DB	$3E,<C0824,>C0824	; SKPNE	C0824
; 2899:             type  = type ? CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2900:             value = constval
	DB	$6A,<D0505,>D0505	; LAW	D0505
	DB	$76,$0C			; SLW	12
; 2901:         is ID_TKN
	DB	$50,<C0822,>C0822	; SKIP	C0822
C0824:
	DB	$2A,$D6			; CB	214
	DB	$3E,<C0825,>C0825	; SKPNE	C0825
; 2902:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0716,>C0716	; CALL	C0716
	DB	$76,$0A			; SLW	10
; 2903:             if !idptr
	DB	$66,$0A			; LLW	10
	DB	$20			; NOT
	DB	$4C,<C0826,>C0826	; SKPFLS	C0826
; 2904:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2905:             fin
C0826:
C0827:
; 2906:             if !(idptr).idtype
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$20			; NOT
	DB	$4C,<C0828,>C0828	; SKPFLS	C0828
; 2907:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2908:             fin
C0828:
C0829:
; 2909:             type  = type ? (idptr).idtype
	DB	$64,$06			; LLB	6
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2910:             value = (idptr):idval
	DB	$66,$0A			; LLW	10
	DB	$62			; LW
	DB	$76,$0C			; SLW	12
; 2911:         is CLOSE_PAREN_TKN
	DB	$50,<C0822,>C0822	; SKIP	C0822
C0825:
	DB	$2A,$A9			; CB	169
	DB	$3E,<C0830,>C0830	; SKPNE	C0830
; 2912:             type     = type ? WORD_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$74,$06			; SLB	6
; 2913:             emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 2914:         otherwise
	DB	$50,<C0822,>C0822	; SKIP	C0822
C0830:
; 2915:             return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2916:     wend
C0822:
	DB	$30			; DROP
; 2917:     ;
; 2918:     ; Constant optimizations
; 2919:     ;
; 2920:     if type & CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0832,>C0832	; SKPFLS	C0832
; 2921:         cparams = TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$74,$04			; SLB	4
; 2922:         while optos < opsp and cparams
C0834:
	DB	$66,$08			; LLW	8
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$46			; ISLT
	DB	$64,$04			; LLB	4
	DB	$24			; LAND
	DB	$4C,<C0835,>C0835	; SKPFLS	C0835
; 2923:             when tos_op_01()
	DB	$54,<C0696,>C0696	; CALL	C0696
; 2924:                 is NEG_TKN
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0837,>C0837	; SKPNE	C0837
; 2925:                     drop pop_op_01()
	DB	$54,<C0692,>C0692	; CALL	C0692
	DB	$30			; DROP
; 2926:                     value = -value
	DB	$66,$0C			; LLW	12
	DB	$10			; NEG
	DB	$76,$0C			; SLW	12
; 2927:                 is COMP_TKN
	DB	$50,<C0836,>C0836	; SKIP	C0836
C0837:
	DB	$2A,$A3			; CB	163
	DB	$3E,<C0838,>C0838	; SKPNE	C0838
; 2928:                     drop pop_op_01()
	DB	$54,<C0692,>C0692	; CALL	C0692
	DB	$30			; DROP
; 2929:                     value = #value
	DB	$66,$0C			; LLW	12
	DB	$12			; COMP
	DB	$76,$0C			; SLW	12
; 2930:                 is LOGIC_NOT_TKN
	DB	$50,<C0836,>C0836	; SKIP	C0836
C0838:
	DB	$2A,$A1			; CB	161
	DB	$3E,<C0839,>C0839	; SKPNE	C0839
; 2931:                     drop pop_op_01()
	DB	$54,<C0692,>C0692	; CALL	C0692
	DB	$30			; DROP
; 2932:                     value = !value
	DB	$66,$0C			; LLW	12
	DB	$20			; NOT
	DB	$76,$0C			; SLW	12
; 2933:                 otherwise
	DB	$50,<C0836,>C0836	; SKIP	C0836
C0839:
; 2934:                     cparams = FALSE
	DB	$00			; ZERO
	DB	$74,$04			; SLB	4
; 2935:             wend
C0836:
	DB	$30			; DROP
; 2936:         loop
	DB	$50,<C0834,>C0834	; SKIP	C0834
C0835:
; 2937:     fin
C0832:
C0833:
; 2938:     ;
; 2939:     ; Parse post-ops
; 2940:     ;
; 2941:     drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 2942:     while ispostop_01()
C0841:
	DB	$54,<C0797,>C0797	; CALL	C0797
	DB	$4C,<C0842,>C0842	; SKPFLS	C0842
; 2943:         if token == OPEN_BRACKET_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C0843,>C0843	; SKPFLS	C0843
; 2944:             ;
; 2945:             ; Array
; 2946:             ;
; 2947:             if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0845,>C0845	; SKPFLS	C0845
; 2948:                 if type & ADDR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C0847,>C0847	; SKPFLS	C0847
; 2949:                     if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0849,>C0849	; SKPFLS	C0849
; 2950:                         emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0529,>C0529	; CALL	C0529
; 2951:                     else
	DB	$50,<C0850,>C0850	; SKIP	C0850
C0849:
; 2952:                         emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0531,>C0531	; CALL	C0531
; 2953:                     fin
C0850:
; 2954:                 elsif type & CONST_TYPE
	DB	$50,<C0848,>C0848	; SKIP	C0848
C0847:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0851,>C0851	; SKPFLS	C0851
; 2955:                     emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0482,>C0482	; CALL	C0482
; 2956:                 fin
C0851:
C0848:
; 2957:                 emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 2958:             fin ; !emit_val
C0845:
C0846:
; 2959:             if type & PTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$4C,<C0852,>C0852	; SKPFLS	C0852
; 2960:                 emit_lw()
	DB	$54,<C0489,>C0489	; CALL	C0489
; 2961:             fin
C0852:
C0853:
; 2962:             if !parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$20			; NOT
	DB	$4C,<C0854,>C0854	; SKPFLS	C0854
; 2963:                 return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 2964:             fin
C0854:
C0855:
; 2965:             if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C0856,>C0856	; SKPFLS	C0856
; 2966:                 return parse_err_11(@no_close_bracket)
	DB	$26,<D0842,>D0842	; LA	D0842
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 2967:             fin
C0856:
C0857:
; 2968:             if type & WORD_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0858,>C0858	; SKPFLS	C0858
; 2969:                 type = WPTR_TYPE
	DB	$2A,$40			; CB	64
	DB	$74,$06			; SLB	6
; 2970:                 emit_indexword()
	DB	$54,<C0535,>C0535	; CALL	C0535
; 2971:             else
	DB	$50,<C0859,>C0859	; SKIP	C0859
C0858:
; 2972:                 type = BPTR_TYPE
	DB	$2A,$20			; CB	32
	DB	$74,$06			; SLB	6
; 2973:                 emit_indexbyte()
	DB	$54,<C0533,>C0533	; CALL	C0533
; 2974:             fin
C0859:
; 2975:             drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 2976:         elsif token == DOT_TKN or token == COLON_TKN
	DB	$50,<C0844,>C0844	; SKIP	C0844
C0843:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$AE			; CB	174
	DB	$40			; ISEQ
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$BA			; CB	186
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0860,>C0860	; SKPFLS	C0860
; 2977:             ;
; 2978:             ; Dot and Colon
; 2979:             ;
; 2980:             if token == DOT_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$AE			; CB	174
	DB	$40			; ISEQ
	DB	$4C,<C0861,>C0861	; SKPFLS	C0861
; 2981:                 elem_type = BPTR_TYPE
	DB	$2A,$20			; CB	32
	DB	$74,$0E			; SLB	14
; 2982:             else
	DB	$50,<C0862,>C0862	; SKIP	C0862
C0861:
; 2983:                 elem_type = WPTR_TYPE
	DB	$2A,$40			; CB	64
	DB	$74,$0E			; SLB	14
; 2984:             fin
C0862:
; 2985:             if parse_constval_21(@elem_offset, @elem_size)
	DB	$28,$10			; LLA	16
	DB	$28,$0F			; LLA	15
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$4C,<C0863,>C0863	; SKPFLS	C0863
; 2986:                 ;
; 2987:                 ; Constant structure offset
; 2988:                 ;
; 2989:                 if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0865,>C0865	; SKPFLS	C0865
; 2990:                     if type & VAR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0867,>C0867	; SKPFLS	C0867
; 2991:                         if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0869,>C0869	; SKPFLS	C0869
; 2992:                             emit_localaddr_10(value + elem_offset)
	DB	$66,$0C			; LLW	12
	DB	$66,$10			; LLW	16
	DB	$02			; ADD
	DB	$54,<C0529,>C0529	; CALL	C0529
; 2993:                         else
	DB	$50,<C0870,>C0870	; SKIP	C0870
C0869:
; 2994:                             ; emit_globaladdr_10(value + elem_offset)
; 2995:                             emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0531,>C0531	; CALL	C0531
; 2996:                             emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0482,>C0482	; CALL	C0482
; 2997:                             drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0548,>C0548	; CALL	C0548
	DB	$30			; DROP
; 2998:                         fin
C0870:
; 2999:                     elsif type & CONST_TYPE
	DB	$50,<C0868,>C0868	; SKIP	C0868
C0867:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0871,>C0871	; SKPFLS	C0871
; 3000:                         value = value + elem_offset
	DB	$66,$0C			; LLW	12
	DB	$66,$10			; LLW	16
	DB	$02			; ADD
	DB	$76,$0C			; SLW	12
; 3001:                         emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0482,>C0482	; CALL	C0482
; 3002:                     else ; FUNC_TYPE
	DB	$50,<C0868,>C0868	; SKIP	C0868
C0871:
; 3003:                         emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0531,>C0531	; CALL	C0531
; 3004:                         emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0482,>C0482	; CALL	C0482
; 3005:                         drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0548,>C0548	; CALL	C0548
	DB	$30			; DROP
; 3006:                     fin
C0868:
; 3007:                     emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 3008:                 else
	DB	$50,<C0866,>C0866	; SKIP	C0866
C0865:
; 3009:                     if elem_offset <> 0
	DB	$66,$10			; LLW	16
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0872,>C0872	; SKPFLS	C0872
; 3010:                         emit_const_10(elem_offset)
	DB	$66,$10			; LLW	16
	DB	$54,<C0482,>C0482	; CALL	C0482
; 3011:                         drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0548,>C0548	; CALL	C0548
	DB	$30			; DROP
; 3012:                     fin
C0872:
C0873:
; 3013:                 fin ; !emit_val
C0866:
; 3014:                 drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3015:             elsif token == OPEN_BRACKET_TKN
	DB	$50,<C0864,>C0864	; SKIP	C0864
C0863:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C0874,>C0874	; SKPFLS	C0874
; 3016:                 ;
; 3017:                 ; Array of arrays
; 3018:                 ;
; 3019:                 if !emit_val
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$4C,<C0875,>C0875	; SKPFLS	C0875
; 3020:                     if type & ADDR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C0877,>C0877	; SKPFLS	C0877
; 3021:                         if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0879,>C0879	; SKPFLS	C0879
; 3022:                             emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0529,>C0529	; CALL	C0529
; 3023:                         else
	DB	$50,<C0880,>C0880	; SKIP	C0880
C0879:
; 3024:                             emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0531,>C0531	; CALL	C0531
; 3025:                         fin
C0880:
; 3026:                     elsif type & CONST_TYPE
	DB	$50,<C0878,>C0878	; SKIP	C0878
C0877:
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0881,>C0881	; SKPFLS	C0881
; 3027:                         emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0482,>C0482	; CALL	C0482
; 3028:                     fin
C0881:
C0878:
; 3029:                     emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 3030:                 fin ; !emit_val
C0875:
C0876:
; 3031:                 repeat
C0883:
; 3032:                         if emit_val > 1
	DB	$64,$07			; LLB	7
	DB	$2A,$01			; CB	1
	DB	$44			; ISGT
	DB	$4C,<C0884,>C0884	; SKPFLS	C0884
; 3033:                                 emit_indexword()
	DB	$54,<C0535,>C0535	; CALL	C0535
; 3034:                                 emit_lw()
	DB	$54,<C0489,>C0489	; CALL	C0489
; 3035:                         fin
C0884:
C0885:
; 3036:                         emit_val = emit_val + 1
	DB	$64,$07			; LLB	7
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$07			; SLB	7
; 3037:                         if !parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$20			; NOT
	DB	$4C,<C0886,>C0886	; SKPFLS	C0886
; 3038:                                 return parse_err_11(@bad_expr)
	DB	$26,<D0655,>D0655	; LA	D0655
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3039:                         fin
C0886:
C0887:
; 3040:                         if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C0888,>C0888	; SKPFLS	C0888
; 3041:                             return parse_err_11(@no_close_bracket)
	DB	$26,<D0842,>D0842	; LA	D0842
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3042:                         fin
C0888:
C0889:
; 3043:                 until scan_01() <> OPEN_BRACKET_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$DB			; CB	219
	DB	$42			; ISNE
	DB	$4C,<C0883,>C0883	; SKPFLS	C0883
C0882:
; 3044:                 if elem_type & WPTR_TYPE
	DB	$64,$0E			; LLB	14
	DB	$2A,$40			; CB	64
	DB	$14			; BAND
	DB	$4C,<C0890,>C0890	; SKPFLS	C0890
; 3045:                     emit_indexword()
	DB	$54,<C0535,>C0535	; CALL	C0535
; 3046:                 else
	DB	$50,<C0891,>C0891	; SKIP	C0891
C0890:
; 3047:                     emit_indexbyte()
	DB	$54,<C0533,>C0533	; CALL	C0533
; 3048:                 fin
C0891:
; 3049:             else
	DB	$50,<C0864,>C0864	; SKIP	C0864
C0874:
; 3050:                 return parse_err_11(@bad_offset)
	DB	$26,<D0594,>D0594	; LA	D0594
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3051:             fin
C0864:
; 3052:             type = elem_type
	DB	$64,$0E			; LLB	14
	DB	$74,$06			; SLB	6
; 3053:         elsif token == OPEN_PAREN_TKN
	DB	$50,<C0844,>C0844	; SKIP	C0844
C0860:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$A8			; CB	168
	DB	$40			; ISEQ
	DB	$4C,<C0892,>C0892	; SKPFLS	C0892
; 3054:             ;
; 3055:             ; Function call
; 3056:             ;
; 3057:             if !emit_val and type & VAR_TYPE
	DB	$64,$07			; LLB	7
	DB	$20			; NOT
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0893,>C0893	; SKPFLS	C0893
; 3058:                 if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0895,>C0895	; SKPFLS	C0895
; 3059:                     emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0529,>C0529	; CALL	C0529
; 3060:                 else
	DB	$50,<C0896,>C0896	; SKIP	C0896
C0895:
; 3061:                     emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0531,>C0531	; CALL	C0531
; 3062:                 fin
C0896:
; 3063:             fin
C0893:
C0894:
; 3064:             if !(type & FUNC_CONST_TYPE)
	DB	$64,$06			; LLB	6
	DB	$2A,$09			; CB	9
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0897,>C0897	; SKPFLS	C0897
; 3065:                     emit_push()
	DB	$54,<C0523,>C0523	; CALL	C0523
; 3066:             fin
C0897:
C0898:
; 3067:             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3068:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C0899,>C0899	; SKPFLS	C0899
; 3069:                 return parse_err_11(@no_close_paren)
	DB	$26,<D0820,>D0820	; LA	D0820
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3070:             fin
C0899:
C0900:
; 3071:             if type & FUNC_CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$09			; CB	9
	DB	$14			; BAND
	DB	$4C,<C0901,>C0901	; SKPFLS	C0901
; 3072:                 emit_call_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0519,>C0519	; CALL	C0519
; 3073:             else
	DB	$50,<C0902,>C0902	; SKIP	C0902
C0901:
; 3074:                 emit_pull()
	DB	$54,<C0525,>C0525	; CALL	C0525
; 3075:                 emit_ical()
	DB	$54,<C0521,>C0521	; CALL	C0521
; 3076:             fin
C0902:
; 3077:             emit_val = 1
	DB	$2A,$01			; CB	1
	DB	$74,$07			; SLB	7
; 3078:             type = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$06			; SLB	6
; 3079:             drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3080:         fin
C0892:
C0844:
; 3081:     loop
	DB	$50,<C0841,>C0841	; SKIP	C0841
C0842:
; 3082:     if emit_val
	DB	$64,$07			; LLB	7
	DB	$4C,<C0903,>C0903	; SKPFLS	C0903
; 3083:         if rvalue
	DB	$66,$02			; LLW	2
	DB	$4C,<C0905,>C0905	; SKPFLS	C0905
; 3084:             if deref and type & PTR_TYPE
	DB	$64,$05			; LLB	5
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C0907,>C0907	; SKPFLS	C0907
; 3085:                 if type & BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C0909,>C0909	; SKPFLS	C0909
; 3086:                     emit_lb()
	DB	$54,<C0487,>C0487	; CALL	C0487
; 3087:                 else
	DB	$50,<C0910,>C0910	; SKIP	C0910
C0909:
; 3088:                     emit_lw()
	DB	$54,<C0489,>C0489	; CALL	C0489
; 3089:                 fin
C0910:
; 3090:             fin
C0907:
C0908:
; 3091:         fin
C0905:
C0906:
; 3092:     else ; emit_val
	DB	$50,<C0904,>C0904	; SKIP	C0904
C0903:
; 3093:         if type & CONST_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0911,>C0911	; SKPFLS	C0911
; 3094:             emit_const_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0482,>C0482	; CALL	C0482
; 3095:         elsif deref
	DB	$50,<C0912,>C0912	; SKIP	C0912
C0911:
	DB	$64,$05			; LLB	5
	DB	$4C,<C0913,>C0913	; SKPFLS	C0913
; 3096:             if type & FUNC_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0914,>C0914	; SKPFLS	C0914
; 3097:                 emit_call_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0519,>C0519	; CALL	C0519
; 3098:             elsif type & VAR_TYPE
	DB	$50,<C0915,>C0915	; SKIP	C0915
C0914:
	DB	$64,$06			; LLB	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0916,>C0916	; SKPFLS	C0916
; 3099:                 if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0917,>C0917	; SKPFLS	C0917
; 3100:                     if type & BYTE_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0919,>C0919	; SKPFLS	C0919
; 3101:                         emit_llb_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0491,>C0491	; CALL	C0491
; 3102:                     else
	DB	$50,<C0920,>C0920	; SKIP	C0920
C0919:
; 3103:                         emit_llw_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0493,>C0493	; CALL	C0493
; 3104:                     fin
C0920:
; 3105:                 else
	DB	$50,<C0918,>C0918	; SKIP	C0918
C0917:
; 3106:                     if type & BYTE_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0921,>C0921	; SKPFLS	C0921
; 3107:                         emit_lab_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0495,>C0495	; CALL	C0495
; 3108:                     else
	DB	$50,<C0922,>C0922	; SKIP	C0922
C0921:
; 3109:                         emit_law_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0497,>C0497	; CALL	C0497
; 3110:                     fin
C0922:
; 3111:                 fin
C0918:
; 3112:             elsif type & PTR_TYPE
	DB	$50,<C0915,>C0915	; SKIP	C0915
C0916:
	DB	$64,$06			; LLB	6
	DB	$2A,$60			; CB	96
	DB	$14			; BAND
	DB	$4C,<C0923,>C0923	; SKPFLS	C0923
; 3113:                 if type & BPTR_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C0924,>C0924	; SKPFLS	C0924
; 3114:                     emit_lb()
	DB	$54,<C0487,>C0487	; CALL	C0487
; 3115:                 else
	DB	$50,<C0925,>C0925	; SKIP	C0925
C0924:
; 3116:                     emit_lw()
	DB	$54,<C0489,>C0489	; CALL	C0489
; 3117:                 fin
C0925:
; 3118:             fin
C0923:
C0915:
; 3119:         else
	DB	$50,<C0912,>C0912	; SKIP	C0912
C0913:
; 3120:             if type & LOCAL_TYPE
	DB	$64,$06			; LLB	6
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0926,>C0926	; SKPFLS	C0926
; 3121:                 emit_localaddr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0529,>C0529	; CALL	C0529
; 3122:             else
	DB	$50,<C0927,>C0927	; SKIP	C0927
C0926:
; 3123:                 emit_globaladdr_10(value)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0531,>C0531	; CALL	C0531
; 3124:             fin
C0927:
; 3125:         fin
C0912:
; 3126:     fin ; emit_val
C0904:
; 3127:     while optos < opsp
C0928:
	DB	$66,$08			; LLW	8
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$46			; ISLT
	DB	$4C,<C0929,>C0929	; SKPFLS	C0929
; 3128:         if !emit_unaryop_11(pop_op_01())
	DB	$54,<C0692,>C0692	; CALL	C0692
	DB	$54,<C0537,>C0537	; CALL	C0537
	DB	$20			; NOT
	DB	$4C,<C0930,>C0930	; SKPFLS	C0930
; 3129:             return parse_err_11(@bad_op)
	DB	$26,<D0628,>D0628	; LA	D0628
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3130:         fin
C0930:
C0931:
; 3131:     loop
	DB	$50,<C0928,>C0928	; SKIP	C0928
C0929:
; 3132:     return type
	DB	$64,$06			; LLB	6
	DB	$5A			; LEAVE
; 3133: end
; 3134: def parse_constexpr_21(valptr, sizeptr)
C0932:					; parse_constexpr_21()
					; valptr = 2
					; sizeptr = 4
; 3135:     byte type, size1, size2
					; type = 6
					; size1 = 7
					; size2 = 8
; 3136:     word val1, val2
					; val1 = 9
					; val2 = 11
; 3137: 
; 3138:     type = parse_constval_21(@val1, @size1)
	JSR	INTERP
	DB	$58,$0D,$02		; ENTER	13,2
	DB	$28,$09			; LLA	9
	DB	$28,$07			; LLA	7
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3139:     if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0934,>C0934	; SKPFLS	C0934
; 3140:         return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3141:     fin
C0934:
C0935:
; 3142:     size2 = 0
	DB	$00			; ZERO
	DB	$74,$08			; SLB	8
; 3143:     when scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
; 3144:         is ADD_TKN
	DB	$2A,$AB			; CB	171
	DB	$3E,<C0937,>C0937	; SKPNE	C0937
; 3145:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3146:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0938,>C0938	; SKPFLS	C0938
; 3147:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3148:             fin
C0938:
C0939:
; 3149:             *valptr = val1 + val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$02			; ADD
	DB	$72			; SW
; 3150:         is SUB_TKN
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0937:
	DB	$2A,$AD			; CB	173
	DB	$3E,<C0940,>C0940	; SKPNE	C0940
; 3151:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3152:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0941,>C0941	; SKPFLS	C0941
; 3153:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3154:             fin
C0941:
C0942:
; 3155:             *valptr = val1 - val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$04			; SUB
	DB	$72			; SW
; 3156:         is MUL_TKN
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0940:
	DB	$2A,$AA			; CB	170
	DB	$3E,<C0943,>C0943	; SKPNE	C0943
; 3157:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3158:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0944,>C0944	; SKPFLS	C0944
; 3159:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3160:             fin
C0944:
C0945:
; 3161:             *valptr = val1 * val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$06			; MUL
	DB	$72			; SW
; 3162:         is DIV_TKN
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0943:
	DB	$2A,$AF			; CB	175
	DB	$3E,<C0946,>C0946	; SKPNE	C0946
; 3163:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3164:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0947,>C0947	; SKPFLS	C0947
; 3165:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3166:             fin
C0947:
C0948:
; 3167:             *valptr = val1 + val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$02			; ADD
	DB	$72			; SW
; 3168:         is MOD_TKN
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0946:
	DB	$2A,$A5			; CB	165
	DB	$3E,<C0949,>C0949	; SKPNE	C0949
; 3169:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3170:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0950,>C0950	; SKPFLS	C0950
; 3171:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3172:             fin
C0950:
C0951:
; 3173:             *valptr = val1 % val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$0A			; DIV,MOD
	DB	$72			; SW
; 3174:             drop
	DB	$30			; DROP
; 3175:         is AND_TKN
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0949:
	DB	$2A,$A6			; CB	166
	DB	$3E,<C0952,>C0952	; SKPNE	C0952
; 3176:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3177:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0953,>C0953	; SKPFLS	C0953
; 3178:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3179:             fin
C0953:
C0954:
; 3180:             *valptr = val1 & val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$14			; BAND
	DB	$72			; SW
; 3181:         is OR_TKN
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0952:
	DB	$2A,$BF			; CB	191
	DB	$3E,<C0955,>C0955	; SKPNE	C0955
; 3182:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3183:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0956,>C0956	; SKPFLS	C0956
; 3184:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3185:             fin
C0956:
C0957:
; 3186:             *valptr = val1 ? val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$16			; IOR
	DB	$72			; SW
; 3187:         is EOR_TKN
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0955:
	DB	$2A,$DE			; CB	222
	DB	$3E,<C0958,>C0958	; SKPNE	C0958
; 3188:             type = parse_constval_21(@val2, @size2)
	DB	$28,$0B			; LLA	11
	DB	$28,$08			; LLA	8
	DB	$54,<C0769,>C0769	; CALL	C0769
	DB	$74,$06			; SLB	6
; 3189:             if !type
	DB	$64,$06			; LLB	6
	DB	$20			; NOT
	DB	$4C,<C0959,>C0959	; SKPFLS	C0959
; 3190:                 return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3191:             fin
C0959:
C0960:
; 3192:             *valptr = val1 ^ val2
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$66,$0B			; LLW	11
	DB	$18			; XOR
	DB	$72			; SW
; 3193:         otherwise
	DB	$50,<C0936,>C0936	; SKIP	C0936
C0958:
; 3194:             *valptr = val1
	DB	$66,$02			; LLW	2
	DB	$66,$09			; LLW	9
	DB	$72			; SW
; 3195:     wend
C0936:
	DB	$30			; DROP
; 3196:     if size1 > size2
	DB	$64,$07			; LLB	7
	DB	$64,$08			; LLB	8
	DB	$44			; ISGT
	DB	$4C,<C0962,>C0962	; SKPFLS	C0962
; 3197:         ^sizeptr = size1
	DB	$66,$04			; LLW	4
	DB	$64,$07			; LLB	7
	DB	$70			; SB
; 3198:     else
	DB	$50,<C0963,>C0963	; SKIP	C0963
C0962:
; 3199:         ^sizeptr = size2
	DB	$66,$04			; LLW	4
	DB	$64,$08			; LLB	8
	DB	$70			; SB
; 3200:     fin
C0963:
; 3201:     return type
	DB	$64,$06			; LLB	6
	DB	$5A			; LEAVE
; 3202: end
; 3203: def parse_expr_01
C0001:					; parse_expr_01()
; 3204:     byte prevmatch, matchop, i
					; prevmatch = 2
					; matchop = 3
					; i = 4
; 3205:     word optos
					; optos = 5
; 3206: 
; 3207:     matchop = 0
	JSR	INTERP
	DB	$58,$07,$00		; ENTER	7,0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 3208:     optos   = opsp
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$76,$05			; SLW	5
; 3209:     repeat
C0966:
; 3210:         prevmatch = matchop
	DB	$64,$03			; LLB	3
	DB	$74,$02			; SLB	2
; 3211:         matchop     = 0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 3212:         if parse_value_11(1)
	DB	$2A,$01			; CB	1
	DB	$54,<C0805,>C0805	; CALL	C0805
	DB	$4C,<C0967,>C0967	; SKPFLS	C0967
; 3213:             matchop = 1
	DB	$2A,$01			; CB	1
	DB	$74,$03			; SLB	3
; 3214:             for i = 0 to bops_tblsz
	DB	$00			; ZERO
C0970:
	DB	$6C,$04			; DLB	4
	DB	$2A,$12			; CB	18
	DB	$3A,<C0969,>C0969	; SKPGT	C0969
	DB	$0C			; INCR
; 3215:                 if token == bops_tbl[i]
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$26,<D0405,>D0405	; LA	D0405
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$40			; ISEQ
	DB	$4C,<C0971,>C0971	; SKPFLS	C0971
; 3216:                     matchop = 2
	DB	$2A,$02			; CB	2
	DB	$74,$03			; SLB	3
; 3217:                     if bops_prec[i] >= tos_op_prec_11(optos)
	DB	$26,<D0424,>D0424	; LA	D0424
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$66,$05			; LLW	5
	DB	$54,<C0700,>C0700	; CALL	C0700
	DB	$48			; ISGE
	DB	$4C,<C0973,>C0973	; SKPFLS	C0973
; 3218:                         if !emit_binaryop_11(pop_op_01())
	DB	$54,<C0692,>C0692	; CALL	C0692
	DB	$54,<C0548,>C0548	; CALL	C0548
	DB	$20			; NOT
	DB	$4C,<C0975,>C0975	; SKPFLS	C0975
; 3219:                             return parse_err_11(@bad_op)
	DB	$30			; DROP
	DB	$26,<D0628,>D0628	; LA	D0628
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3220:                         fin
C0975:
C0976:
; 3221:                     fin
C0973:
C0974:
; 3222:                     drop push_op_21(token, bops_prec[i])
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$26,<D0424,>D0424	; LA	D0424
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$54,<C0688,>C0688	; CALL	C0688
	DB	$30			; DROP
; 3223:                     break
	DB	$50,<C0969,>C0969	; SKIP	C0969
; 3224:                 fin
C0971:
C0972:
; 3225:             next
	DB	$50,<C0970,>C0970	; SKIP	C0970
C0969:
	DB	$30			; DROP
; 3226:         fin
C0967:
C0968:
; 3227:     until matchop <> 2
	DB	$64,$03			; LLB	3
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C0966,>C0966	; SKPFLS	C0966
C0965:
; 3228:     if matchop == 0 and prevmatch == 2
	DB	$64,$03			; LLB	3
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0977,>C0977	; SKPFLS	C0977
; 3229:         return parse_err_11(@missing_op)
	DB	$26,<D0866,>D0866	; LA	D0866
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3230:     fin
C0977:
C0978:
; 3231:     while optos < opsp
C0979:
	DB	$66,$05			; LLW	5
	DB	$6A,<D0475,>D0475	; LAW	D0475
	DB	$46			; ISLT
	DB	$4C,<C0980,>C0980	; SKPFLS	C0980
; 3232:         if !emit_binaryop_11(pop_op_01())
	DB	$54,<C0692,>C0692	; CALL	C0692
	DB	$54,<C0548,>C0548	; CALL	C0548
	DB	$20			; NOT
	DB	$4C,<C0981,>C0981	; SKPFLS	C0981
; 3233:             return parse_err_11(@bad_op)
	DB	$26,<D0628,>D0628	; LA	D0628
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3234:         fin
C0981:
C0982:
; 3235:     loop
	DB	$50,<C0979,>C0979	; SKIP	C0979
C0980:
; 3236:     return matchop or prevmatch
	DB	$64,$03			; LLB	3
	DB	$64,$02			; LLB	2
	DB	$22			; LOR
	DB	$5A			; LEAVE
; 3237: end
; 3238: def parse_setlist_21(addr, type)
C0983:					; parse_setlist_21()
					; addr = 2
					; type = 4
; 3239:     word nexttype, nextaddr, idptr, saveptr
					; nexttype = 6
					; nextaddr = 8
					; idptr = 10
					; saveptr = 12
; 3240: 
; 3241:     if !(type & VAR_TYPE)
	JSR	INTERP
	DB	$58,$0E,$02		; ENTER	14,2
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0985,>C0985	; SKPFLS	C0985
; 3242:         emit_push()
	DB	$54,<C0523,>C0523	; CALL	C0523
; 3243:     fin
C0985:
C0986:
; 3244:     nexttype = 0
	DB	$00			; ZERO
	DB	$76,$06			; SLW	6
; 3245:     nextaddr = 0
	DB	$00			; ZERO
	DB	$76,$08			; SLW	8
; 3246:     if scan_01() == ID_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C0987,>C0987	; SKPFLS	C0987
; 3247:         idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0716,>C0716	; CALL	C0716
	DB	$76,$0A			; SLW	10
; 3248:         if !idptr
	DB	$66,$0A			; LLW	10
	DB	$20			; NOT
	DB	$4C,<C0989,>C0989	; SKPFLS	C0989
; 3249:             return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3250:         fin
C0989:
C0990:
; 3251:         nexttype = (idptr).idtype
	DB	$66,$0A			; LLW	10
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$76,$06			; SLW	6
; 3252:         if type & VAR_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C0991,>C0991	; SKPFLS	C0991
; 3253:             nextaddr = (idptr):idval
	DB	$66,$0A			; LLW	10
	DB	$62			; LW
	DB	$76,$08			; SLW	8
; 3254:         fin
C0991:
C0992:
; 3255:     fin
C0987:
C0988:
; 3256:     saveptr = tknptr
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$76,$0C			; SLW	12
; 3257:     drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3258:     if nexttype & VAR_TYPE and token == SET_TKN
	DB	$66,$06			; LLW	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0993,>C0993	; SKPFLS	C0993
; 3259:         drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3260:         if type & LOCAL_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C0995,>C0995	; SKPFLS	C0995
; 3261:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0997,>C0997	; SKPFLS	C0997
; 3262:                 emit_slb_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0503,>C0503	; CALL	C0503
; 3263:             else
	DB	$50,<C0998,>C0998	; SKIP	C0998
C0997:
; 3264:                 emit_slw_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0505,>C0505	; CALL	C0505
; 3265:             fin
C0998:
; 3266:         else
	DB	$50,<C0996,>C0996	; SKIP	C0996
C0995:
; 3267:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0999,>C0999	; SKPFLS	C0999
; 3268:                 emit_sab_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0511,>C0511	; CALL	C0511
; 3269:             else
	DB	$50,<C1000,>C1000	; SKIP	C1000
C0999:
; 3270:                 emit_saw_10(nextaddr)
	DB	$66,$08			; LLW	8
	DB	$54,<C0513,>C0513	; CALL	C0513
; 3271:             fin
C1000:
; 3272:         fin
C0996:
; 3273:     elsif nexttype & VAR_TYPE and token == SETLIST_TKN
	DB	$50,<C0994,>C0994	; SKIP	C0994
C0993:
	DB	$66,$06			; LLW	6
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C1001,>C1001	; SKPFLS	C1001
; 3274:         if !parse_setlist_21(nextaddr, nexttype)
	DB	$66,$08			; LLW	8
	DB	$66,$06			; LLW	6
	DB	$54,<C0983,>C0983	; CALL	C0983
	DB	$20			; NOT
	DB	$4C,<C1002,>C1002	; SKPFLS	C1002
; 3275:             return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3276:         fin
C1002:
C1003:
; 3277:     else
	DB	$50,<C0994,>C0994	; SKIP	C0994
C1001:
; 3278:         tknptr = saveptr
	DB	$66,$0C			; LLW	12
	DB	$7A,<D0501,>D0501	; SAW	D0501
; 3279:         rewind_10(tknptr)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$54,<C0680,>C0680	; CALL	C0680
; 3280:         nexttype = parse_value_11(0)
	DB	$00			; ZERO
	DB	$54,<C0805,>C0805	; CALL	C0805
	DB	$76,$06			; SLW	6
; 3281:         if nexttype <> 0
	DB	$66,$06			; LLW	6
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C1004,>C1004	; SKPFLS	C1004
; 3282:             if token == SET_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C1006,>C1006	; SKPFLS	C1006
; 3283:                 emit_push()
	DB	$54,<C0523,>C0523	; CALL	C0523
; 3284:                 drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3285:                 emit_pull()
	DB	$54,<C0525,>C0525	; CALL	C0525
; 3286:                 emit_swap()
	DB	$54,<C0593,>C0593	; CALL	C0593
; 3287:                 if nexttype & (BYTE_TYPE ? BPTR_TYPE)
	DB	$66,$06			; LLW	6
	DB	$2A,$02			; CB	2
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$14			; BAND
	DB	$4C,<C1008,>C1008	; SKPFLS	C1008
; 3288:                     emit_sb()
	DB	$54,<C0499,>C0499	; CALL	C0499
; 3289:                 else
	DB	$50,<C1009,>C1009	; SKIP	C1009
C1008:
; 3290:                     emit_sw()
	DB	$54,<C0501,>C0501	; CALL	C0501
; 3291:                 fin
C1009:
; 3292:             fin
C1006:
C1007:
; 3293:         elsif token == SETLIST_TKN
	DB	$50,<C1005,>C1005	; SKIP	C1005
C1004:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$4C,<C1010,>C1010	; SKPFLS	C1010
; 3294:             if !parse_setlist_21(0, nexttype)
	DB	$00			; ZERO
	DB	$66,$06			; LLW	6
	DB	$54,<C0983,>C0983	; CALL	C0983
	DB	$20			; NOT
	DB	$4C,<C1011,>C1011	; SKPFLS	C1011
; 3295:                 return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3296:             fin
C1011:
C1012:
; 3297:         else
	DB	$50,<C1005,>C1005	; SKIP	C1005
C1010:
; 3298:             return parse_err_11(@bad_syntax)
	DB	$26,<D0670,>D0670	; LA	D0670
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3299:         fin
C1005:
; 3300:     fin
C0994:
; 3301:     if type & VAR_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C1013,>C1013	; SKPFLS	C1013
; 3302:         if type & LOCAL_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C1015,>C1015	; SKPFLS	C1015
; 3303:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C1017,>C1017	; SKPFLS	C1017
; 3304:                 emit_slb_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0503,>C0503	; CALL	C0503
; 3305:             else
	DB	$50,<C1018,>C1018	; SKIP	C1018
C1017:
; 3306:                 emit_slw_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0505,>C0505	; CALL	C0505
; 3307:             fin
C1018:
; 3308:         else
	DB	$50,<C1016,>C1016	; SKIP	C1016
C1015:
; 3309:             if type & BYTE_TYPE
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C1019,>C1019	; SKPFLS	C1019
; 3310:                 emit_sab_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0511,>C0511	; CALL	C0511
; 3311:             else
	DB	$50,<C1020,>C1020	; SKIP	C1020
C1019:
; 3312:                 emit_saw_10(addr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0513,>C0513	; CALL	C0513
; 3313:             fin
C1020:
; 3314:         fin
C1016:
; 3315:     else
	DB	$50,<C1014,>C1014	; SKIP	C1014
C1013:
; 3316:         emit_pull()
	DB	$54,<C0525,>C0525	; CALL	C0525
; 3317:         emit_swap()
	DB	$54,<C0593,>C0593	; CALL	C0593
; 3318:         if type & (BYTE_TYPE ? BPTR_TYPE)
	DB	$66,$04			; LLW	4
	DB	$2A,$02			; CB	2
	DB	$2A,$20			; CB	32
	DB	$16			; IOR
	DB	$14			; BAND
	DB	$4C,<C1021,>C1021	; SKPFLS	C1021
; 3319:             emit_sb()
	DB	$54,<C0499,>C0499	; CALL	C0499
; 3320:         else
	DB	$50,<C1022,>C1022	; SKIP	C1022
C1021:
; 3321:             emit_sw()
	DB	$54,<C0501,>C0501	; CALL	C0501
; 3322:         fin
C1022:
; 3323:     fin
C1014:
; 3324:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3325: end
; 3326: def parse_stmnt_01
C1023:					; parse_stmnt_01()
; 3327:     byte type, i
					; type = 2
					; i = 3
; 3328:     word tag_prevbrk, tag_else, tag_endif, tag_while, tag_wend
					; tag_prevbrk = 4
					; tag_else = 6
					; tag_endif = 8
					; tag_while = 10
					; tag_wend = 12
; 3329:     word tag_repeat, tag_for, tag_choice, idptr, saveptr, addr, stepdir
					; tag_repeat = 14
					; tag_for = 16
					; tag_choice = 18
					; idptr = 20
					; saveptr = 22
					; addr = 24
					; stepdir = 26
; 3330: 
; 3331:     if token <> END_TKN and token <> DONE_TKN
	JSR	INTERP
	DB	$58,$1C,$00		; ENTER	28,0
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$87			; CB	135
	DB	$42			; ISNE
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$98			; CB	152
	DB	$42			; ISNE
	DB	$24			; LAND
	DB	$4C,<C1025,>C1025	; SKPFLS	C1025
; 3332:         prevstmnt = token
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$78,<D1104,>D1104	; SAB	D1104
; 3333:     fin
C1025:
C1026:
; 3334:     when token
	DB	$68,<D0495,>D0495	; LAB	D0495
; 3335:         is IF_TKN
	DB	$2A,$83			; CB	131
	DB	$3E,<C1028,>C1028	; SKPNE	C1028
; 3336:             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3337:             tag_else  = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$06			; SLW	6
; 3338:             tag_endif = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$08			; SLW	8
; 3339:             emit_brfls_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0581,>C0581	; CALL	C0581
; 3340:             drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3341:             repeat
C1030:
; 3342:                 while parse_stmnt_01()
C1031:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1032,>C1032	; SKPFLS	C1032
; 3343:                     drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3344:                 loop
	DB	$50,<C1031,>C1031	; SKIP	C1031
C1032:
; 3345:                 if token <> ELSEIF_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$84			; CB	132
	DB	$42			; ISNE
	DB	$4C,<C1033,>C1033	; SKPFLS	C1033
; 3346:                     break
	DB	$50,<C1029,>C1029	; SKIP	C1029
; 3347:                 fin
C1033:
C1034:
; 3348:                 emit_jump_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0589,>C0589	; CALL	C0589
; 3349:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3350:                 if !parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$20			; NOT
	DB	$4C,<C1035,>C1035	; SKPFLS	C1035
; 3351:                     return 0
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3352:                 fin
C1035:
C1036:
; 3353:                 tag_else = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$06			; SLW	6
; 3354:                 emit_brfls_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0581,>C0581	; CALL	C0581
; 3355:             until FALSE
	DB	$00			; ZERO
	DB	$4C,<C1030,>C1030	; SKPFLS	C1030
C1029:
; 3356:             if token == ELSE_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$85			; CB	133
	DB	$40			; ISEQ
	DB	$4C,<C1037,>C1037	; SKPFLS	C1037
; 3357:                 emit_jump_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0589,>C0589	; CALL	C0589
; 3358:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3359:                 drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3360:                 while parse_stmnt_01()
C1039:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1040,>C1040	; SKPFLS	C1040
; 3361:                     drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3362:                 loop
	DB	$50,<C1039,>C1039	; SKIP	C1039
C1040:
; 3363:                 emit_codetag_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3364:             else
	DB	$50,<C1038,>C1038	; SKIP	C1038
C1037:
; 3365:                 emit_codetag_10(tag_else)
	DB	$66,$06			; LLW	6
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3366:                 emit_codetag_10(tag_endif)
	DB	$66,$08			; LLW	8
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3367:             fin
C1038:
; 3368:             if token <> FIN_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$86			; CB	134
	DB	$42			; ISNE
	DB	$4C,<C1041,>C1041	; SKPFLS	C1041
; 3369:                 return parse_err_11(@no_fin)
	DB	$30			; DROP
	DB	$26,<D0882,>D0882	; LA	D0882
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3370:             fin
C1041:
C1042:
; 3371:         is FOR_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1028:
	DB	$2A,$8E			; CB	142
	DB	$3E,<C1043,>C1043	; SKPNE	C1043
; 3372:             stack_loop  = stack_loop + 1
	DB	$68,<D1103,>D1103	; LAB	D1103
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D1103,>D1103	; SAB	D1103
; 3373:             tag_for     = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$10			; SLW	16
; 3374:             tag_prevbrk = break_tag
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$76,$04			; SLW	4
; 3375:             break_tag   = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3376:             if scan_01() <> ID_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$D6			; CB	214
	DB	$42			; ISNE
	DB	$4C,<C1044,>C1044	; SKPFLS	C1044
; 3377:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3378:             fin
C1044:
C1045:
; 3379:             idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0716,>C0716	; CALL	C0716
	DB	$76,$14			; SLW	20
; 3380:             if idptr
	DB	$66,$14			; LLW	20
	DB	$4C,<C1046,>C1046	; SKPFLS	C1046
; 3381:                 type  = (idptr).idtype
	DB	$66,$14			; LLW	20
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$02			; SLB	2
; 3382:                 addr  = (idptr):idval
	DB	$66,$14			; LLW	20
	DB	$62			; LW
	DB	$76,$18			; SLW	24
; 3383:             else
	DB	$50,<C1047,>C1047	; SKIP	C1047
C1046:
; 3384:                 return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3385:             fin
C1047:
; 3386:             if scan_01() <> SET_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$BD			; CB	189
	DB	$42			; ISNE
	DB	$4C,<C1048,>C1048	; SKPFLS	C1048
; 3387:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3388:             fin
C1048:
C1049:
; 3389:             if !parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$20			; NOT
	DB	$4C,<C1050,>C1050	; SKPFLS	C1050
; 3390:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3391:             fin
C1050:
C1051:
; 3392:             emit_codetag_10(tag_for)
	DB	$66,$10			; LLW	16
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3393:             if type & LOCAL_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C1052,>C1052	; SKPFLS	C1052
; 3394:                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C1054,>C1054	; SKPFLS	C1054
; 3395:                     emit_dlb_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0507,>C0507	; CALL	C0507
; 3396:                 else
	DB	$50,<C1055,>C1055	; SKIP	C1055
C1054:
; 3397:                     emit_dlw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0509,>C0509	; CALL	C0509
; 3398:                 fin
C1055:
; 3399:             else
	DB	$50,<C1053,>C1053	; SKIP	C1053
C1052:
; 3400:                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C1056,>C1056	; SKPFLS	C1056
; 3401:                     emit_dab_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0515,>C0515	; CALL	C0515
; 3402:                 else
	DB	$50,<C1057,>C1057	; SKIP	C1057
C1056:
; 3403:                     emit_daw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0517,>C0517	; CALL	C0517
; 3404:                 fin
C1057:
; 3405:             fin
C1053:
; 3406:             stepdir = 1
	DB	$2A,$01			; CB	1
	DB	$76,$1A			; SLW	26
; 3407:             if token == TO_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$8F			; CB	143
	DB	$40			; ISEQ
	DB	$4C,<C1058,>C1058	; SKPFLS	C1058
; 3408:                 drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3409:             elsif token == DOWNTO_TKN
	DB	$50,<C1059,>C1059	; SKIP	C1059
C1058:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$90			; CB	144
	DB	$40			; ISEQ
	DB	$4C,<C1060,>C1060	; SKPFLS	C1060
; 3410:                 drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3411:                 stepdir = -1
	DB	$2C,$FF,$FF		; CW	-1
	DB	$76,$1A			; SLW	26
; 3412:             fin
C1060:
C1059:
; 3413:             if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C1061,>C1061	; SKPFLS	C1061
; 3414:                 emit_brgt_10(break_tag)
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$54,<C0583,>C0583	; CALL	C0583
; 3415:             else
	DB	$50,<C1062,>C1062	; SKIP	C1062
C1061:
; 3416:                 emit_brlt_10(break_tag)
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$54,<C0585,>C0585	; CALL	C0585
; 3417:             fin
C1062:
; 3418:             if token == STEP_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$91			; CB	145
	DB	$40			; ISEQ
	DB	$4C,<C1063,>C1063	; SKPFLS	C1063
; 3419:                 drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3420:                 if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C1065,>C1065	; SKPFLS	C1065
; 3421:                     drop emit_binaryop_11(ADD_TKN)
	DB	$2A,$AB			; CB	171
	DB	$54,<C0548,>C0548	; CALL	C0548
	DB	$30			; DROP
; 3422:                 else
	DB	$50,<C1066,>C1066	; SKIP	C1066
C1065:
; 3423:                     drop emit_binaryop_11(SUB_TKN)
	DB	$2A,$AD			; CB	173
	DB	$54,<C0548,>C0548	; CALL	C0548
	DB	$30			; DROP
; 3424:                 fin
C1066:
; 3425:             else
	DB	$50,<C1064,>C1064	; SKIP	C1064
C1063:
; 3426:                 if stepdir > 0
	DB	$66,$1A			; LLW	26
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C1067,>C1067	; SKPFLS	C1067
; 3427:                     drop emit_unaryop_11(INC_TKN)
	DB	$2A,$C1			; CB	193
	DB	$54,<C0537,>C0537	; CALL	C0537
	DB	$30			; DROP
; 3428:                 else
	DB	$50,<C1068,>C1068	; SKIP	C1068
C1067:
; 3429:                     drop emit_unaryop_11(DEC_TKN)
	DB	$2A,$C4			; CB	196
	DB	$54,<C0537,>C0537	; CALL	C0537
	DB	$30			; DROP
; 3430:                 fin
C1068:
; 3431:             fin
C1064:
; 3432:             while parse_stmnt_01()
C1069:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1070,>C1070	; SKPFLS	C1070
; 3433:                 drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3434:             loop
	DB	$50,<C1069,>C1069	; SKIP	C1069
C1070:
; 3435:             if token <> NEXT_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$92			; CB	146
	DB	$42			; ISNE
	DB	$4C,<C1071,>C1071	; SKPFLS	C1071
; 3436:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3437:             fin
C1071:
C1072:
; 3438:             emit_jump_10(tag_for)
	DB	$66,$10			; LLW	16
	DB	$54,<C0589,>C0589	; CALL	C0589
; 3439:             emit_codetag_10(break_tag)
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3440:             emit_drop()
	DB	$54,<C0591,>C0591	; CALL	C0591
; 3441:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3442:             stack_loop = stack_loop - 1
	DB	$68,<D1103,>D1103	; LAB	D1103
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D1103,>D1103	; SAB	D1103
; 3443:         is WHILE_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1043:
	DB	$2A,$88			; CB	136
	DB	$3E,<C1073,>C1073	; SKPNE	C1073
; 3444:             tag_while   = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$0A			; SLW	10
; 3445:             tag_wend    = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$0C			; SLW	12
; 3446:             tag_prevbrk = break_tag
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$76,$04			; SLW	4
; 3447:             break_tag   = tag_wend
	DB	$66,$0C			; LLW	12
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3448:             emit_codetag_10(tag_while)
	DB	$66,$0A			; LLW	10
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3449:             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3450:             emit_brfls_10(tag_wend)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0581,>C0581	; CALL	C0581
; 3451:             while parse_stmnt_01()
C1074:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1075,>C1075	; SKPFLS	C1075
; 3452:                 drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3453:             loop
	DB	$50,<C1074,>C1074	; SKIP	C1074
C1075:
; 3454:             if token <> LOOP_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$89			; CB	137
	DB	$42			; ISNE
	DB	$4C,<C1076,>C1076	; SKPFLS	C1076
; 3455:                 return parse_err_11(@no_loop)
	DB	$30			; DROP
	DB	$26,<D0894,>D0894	; LA	D0894
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3456:             fin
C1076:
C1077:
; 3457:             emit_jump_10(tag_while)
	DB	$66,$0A			; LLW	10
	DB	$54,<C0589,>C0589	; CALL	C0589
; 3458:             emit_codetag_10(tag_wend)
	DB	$66,$0C			; LLW	12
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3459:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3460:         is REPEAT_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1073:
	DB	$2A,$93			; CB	147
	DB	$3E,<C1078,>C1078	; SKPNE	C1078
; 3461:             tag_repeat  = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$0E			; SLW	14
; 3462:             tag_prevbrk = break_tag
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$76,$04			; SLW	4
; 3463:             break_tag   = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3464:             emit_codetag_10(tag_repeat)
	DB	$66,$0E			; LLW	14
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3465:             drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3466:             while parse_stmnt_01()
C1079:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1080,>C1080	; SKPFLS	C1080
; 3467:                 drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3468:             loop
	DB	$50,<C1079,>C1079	; SKIP	C1079
C1080:
; 3469:             if token <> UNTIL_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$94			; CB	148
	DB	$42			; ISNE
	DB	$4C,<C1081,>C1081	; SKPFLS	C1081
; 3470:                 return parse_err_11(@no_until)
	DB	$30			; DROP
	DB	$26,<D0907,>D0907	; LA	D0907
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3471:             fin
C1081:
C1082:
; 3472:             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3473:             emit_brfls_10(tag_repeat)
	DB	$66,$0E			; LLW	14
	DB	$54,<C0581,>C0581	; CALL	C0581
; 3474:             emit_codetag_10(break_tag)
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3475:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3476:         is CASE_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1078:
	DB	$2A,$8A			; CB	138
	DB	$3E,<C1083,>C1083	; SKPNE	C1083
; 3477:             stack_loop  = stack_loop + 1
	DB	$68,<D1103,>D1103	; LAB	D1103
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D1103,>D1103	; SAB	D1103
; 3478:             tag_choice  = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$12			; SLW	18
; 3479:             tag_prevbrk = break_tag
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$76,$04			; SLW	4
; 3480:             break_tag   = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3481:             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3482:             drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3483:             while token <> ENDCASE_TKN
C1084:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$8D			; CB	141
	DB	$42			; ISNE
	DB	$4C,<C1085,>C1085	; SKPFLS	C1085
; 3484:                 when token
	DB	$68,<D0495,>D0495	; LAB	D0495
; 3485:                     is OF_TKN
	DB	$2A,$8B			; CB	139
	DB	$3E,<C1087,>C1087	; SKPNE	C1087
; 3486:                         if !parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$20			; NOT
	DB	$4C,<C1088,>C1088	; SKPFLS	C1088
; 3487:                             return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3488:                         fin
C1088:
C1089:
; 3489:                         emit_brne_10(tag_choice)
	DB	$66,$12			; LLW	18
	DB	$54,<C0587,>C0587	; CALL	C0587
; 3490:                         while parse_stmnt_01()
C1090:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1091,>C1091	; SKPFLS	C1091
; 3491:                             drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3492:                         loop
	DB	$50,<C1090,>C1090	; SKIP	C1090
C1091:
; 3493:                         emit_jump_10(break_tag)
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$54,<C0589,>C0589	; CALL	C0589
; 3494:                         emit_codetag_10(tag_choice)
	DB	$66,$12			; LLW	18
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3495:                         tag_choice = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$12			; SLW	18
; 3496:                     is DEFAULT_TKN
	DB	$50,<C1086,>C1086	; SKIP	C1086
C1087:
	DB	$2A,$8C			; CB	140
	DB	$3E,<C1092,>C1092	; SKPNE	C1092
; 3497:                         drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3498:                         while parse_stmnt_01()
C1093:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1094,>C1094	; SKPFLS	C1094
; 3499:                             drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3500:                         loop
	DB	$50,<C1093,>C1093	; SKIP	C1093
C1094:
; 3501:                         if token <> ENDCASE_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$8D			; CB	141
	DB	$42			; ISNE
	DB	$4C,<C1095,>C1095	; SKPFLS	C1095
; 3502:                             return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3503:                         fin
C1095:
C1096:
; 3504:                     otherwise
	DB	$50,<C1086,>C1086	; SKIP	C1086
C1092:
; 3505:                         return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3506:                 wend
C1086:
	DB	$30			; DROP
; 3507:             loop
	DB	$50,<C1084,>C1084	; SKIP	C1084
C1085:
; 3508:             emit_codetag_10(break_tag)
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3509:             emit_drop()
	DB	$54,<C0591,>C0591	; CALL	C0591
; 3510:             break_tag = tag_prevbrk
	DB	$66,$04			; LLW	4
	DB	$7A,<D1107,>D1107	; SAW	D1107
; 3511:             stack_loop = stack_loop - 1
	DB	$68,<D1103,>D1103	; LAB	D1103
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D1103,>D1103	; SAB	D1103
; 3512:         is BREAK_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1083:
	DB	$2A,$9A			; CB	154
	DB	$3E,<C1098,>C1098	; SKPNE	C1098
; 3513:             if break_tag
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$4C,<C1099,>C1099	; SKPFLS	C1099
; 3514:                 emit_jump_10(break_tag)
	DB	$6A,<D1107,>D1107	; LAW	D1107
	DB	$54,<C0589,>C0589	; CALL	C0589
; 3515:             else
	DB	$50,<C1100,>C1100	; SKIP	C1100
C1099:
; 3516:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3517:             fin
C1100:
; 3518:         is RETURN_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1098:
	DB	$2A,$99			; CB	153
	DB	$3E,<C1101,>C1101	; SKPNE	C1101
; 3519:             if infunc
	DB	$68,<D1102,>D1102	; LAB	D1102
	DB	$4C,<C1102,>C1102	; SKPFLS	C1102
; 3520:                 for i = 1 to stack_loop
	DB	$2A,$01			; CB	1
C1105:
	DB	$6C,$03			; DLB	3
	DB	$68,<D1103,>D1103	; LAB	D1103
	DB	$3A,<C1104,>C1104	; SKPGT	C1104
	DB	$0C			; INCR
; 3521:                     emit_drop()
	DB	$54,<C0591,>C0591	; CALL	C0591
; 3522:                 next
	DB	$50,<C1105,>C1105	; SKIP	C1105
C1104:
	DB	$30			; DROP
; 3523:                 drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3524:                 emit_leave_10(framesize)
	DB	$6A,<D0484,>D0484	; LAW	D0484
	DB	$54,<C0595,>C0595	; CALL	C0595
; 3525:             else
	DB	$50,<C1103,>C1103	; SKIP	C1103
C1102:
; 3526:                 return parse_err_11(@bad_stmnt)
	DB	$30			; DROP
	DB	$26,<D0642,>D0642	; LA	D0642
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3527:             fin
C1103:
; 3528:         is EXIT_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1101:
	DB	$2A,$9C			; CB	156
	DB	$3E,<C1106,>C1106	; SKPNE	C1106
; 3529:             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3530:             emit_exit()
	DB	$54,<C0605,>C0605	; CALL	C0605
; 3531:         is DROP_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1106:
	DB	$2A,$97			; CB	151
	DB	$3E,<C1107,>C1107	; SKPNE	C1107
; 3532:             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3533:             emit_drop()
	DB	$54,<C0591,>C0591	; CALL	C0591
; 3534:         is ELSE_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1107:
	DB	$2A,$85			; CB	133
	DB	$3E,<C1108,>C1108	; SKPNE	C1108
; 3535:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3536:         is ELSEIF_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1108:
	DB	$2A,$84			; CB	132
	DB	$3E,<C1109,>C1109	; SKPNE	C1109
; 3537:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3538:         is FIN_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1109:
	DB	$2A,$86			; CB	134
	DB	$3E,<C1110,>C1110	; SKPNE	C1110
; 3539:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3540:         is LOOP_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1110:
	DB	$2A,$89			; CB	137
	DB	$3E,<C1111,>C1111	; SKPNE	C1111
; 3541:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3542:         is UNTIL_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1111:
	DB	$2A,$94			; CB	148
	DB	$3E,<C1112,>C1112	; SKPNE	C1112
; 3543:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3544:         is NEXT_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1112:
	DB	$2A,$92			; CB	146
	DB	$3E,<C1113,>C1113	; SKPNE	C1113
; 3545:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3546:         is OF_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1113:
	DB	$2A,$8B			; CB	139
	DB	$3E,<C1114,>C1114	; SKPNE	C1114
; 3547:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3548:         is DEFAULT_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1114:
	DB	$2A,$8C			; CB	140
	DB	$3E,<C1115,>C1115	; SKPNE	C1115
; 3549:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3550:         is ENDCASE_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1115:
	DB	$2A,$8D			; CB	141
	DB	$3E,<C1116,>C1116	; SKPNE	C1116
; 3551:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3552:         is END_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1116:
	DB	$2A,$87			; CB	135
	DB	$3E,<C1117,>C1117	; SKPNE	C1117
; 3553:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3554:         is DONE_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1117:
	DB	$2A,$98			; CB	152
	DB	$3E,<C1118,>C1118	; SKPNE	C1118
; 3555:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3556:         is IFUNC_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1118:
	DB	$2A,$95			; CB	149
	DB	$3E,<C1119,>C1119	; SKPNE	C1119
; 3557:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3558:         is NFUNC_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1119:
	DB	$2A,$96			; CB	150
	DB	$3E,<C1120,>C1120	; SKPNE	C1120
; 3559:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3560:         is EOF_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1120:
	DB	$2A,$01			; CB	1
	DB	$3E,<C1121,>C1121	; SKPNE	C1121
; 3561:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3562:         is EOL_TKN
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1121:
	DB	$2A,$02			; CB	2
	DB	$3E,<C1122,>C1122	; SKPNE	C1122
; 3563:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3564:         otherwise
	DB	$50,<C1027,>C1027	; SKIP	C1027
C1122:
; 3565:             if token == ID_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C1124,>C1124	; SKPFLS	C1124
; 3566:                 saveptr = tknptr
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$76,$16			; SLW	22
; 3567:                 idptr = id_lookup_21(tknptr, tknlen)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0716,>C0716	; CALL	C0716
	DB	$76,$14			; SLW	20
; 3568:                 if !idptr
	DB	$66,$14			; LLW	20
	DB	$20			; NOT
	DB	$4C,<C1126,>C1126	; SKPFLS	C1126
; 3569:                     return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3570:                 fin
C1126:
C1127:
; 3571:                 type = (idptr).idtype
	DB	$66,$14			; LLW	20
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$02			; SLB	2
; 3572:                 if type & ADDR_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$0E			; CB	14
	DB	$14			; BAND
	DB	$4C,<C1128,>C1128	; SKPFLS	C1128
; 3573:                     addr = (idptr):idval
	DB	$66,$14			; LLW	20
	DB	$62			; LW
	DB	$76,$18			; SLW	24
; 3574:                     if scan_01() == SET_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C1130,>C1130	; SKPFLS	C1130
; 3575:                         if type & VAR_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$4C,<C1132,>C1132	; SKPFLS	C1132
; 3576:                             drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3577:                             if type & LOCAL_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$10			; CB	16
	DB	$14			; BAND
	DB	$4C,<C1134,>C1134	; SKPFLS	C1134
; 3578:                                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C1136,>C1136	; SKPFLS	C1136
; 3579:                                     emit_slb_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0503,>C0503	; CALL	C0503
; 3580:                                 else
	DB	$50,<C1137,>C1137	; SKIP	C1137
C1136:
; 3581:                                     emit_slw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0505,>C0505	; CALL	C0505
; 3582:                                 fin
C1137:
; 3583:                             else
	DB	$50,<C1135,>C1135	; SKIP	C1135
C1134:
; 3584:                                 if type & BYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C1138,>C1138	; SKPFLS	C1138
; 3585:                                     emit_sab_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0511,>C0511	; CALL	C0511
; 3586:                                 else
	DB	$50,<C1139,>C1139	; SKIP	C1139
C1138:
; 3587:                                     emit_saw_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0513,>C0513	; CALL	C0513
; 3588:                                 fin
C1139:
; 3589:                             fin
C1135:
; 3590:                             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3591:                         fin
C1132:
C1133:
; 3592:                     elsif token == SETLIST_TKN and type & VAR_TYPE
	DB	$50,<C1131,>C1131	; SKIP	C1131
C1130:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$64,$02			; LLB	2
	DB	$2A,$06			; CB	6
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C1140,>C1140	; SKPFLS	C1140
; 3593:                             return parse_setlist_21(addr, type);
	DB	$30			; DROP
	DB	$66,$18			; LLW	24
	DB	$64,$02			; LLB	2
	DB	$54,<C0983,>C0983	; CALL	C0983
	DB	$5A			; LEAVE
; 3594:                     elsif token == EOL_TKN and type & FUNC_TYPE
	DB	$50,<C1131,>C1131	; SKIP	C1131
C1140:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$02			; CB	2
	DB	$40			; ISEQ
	DB	$64,$02			; LLB	2
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$24			; LAND
	DB	$4C,<C1141,>C1141	; SKPFLS	C1141
; 3595:                         emit_call_10(addr)
	DB	$66,$18			; LLW	24
	DB	$54,<C0519,>C0519	; CALL	C0519
; 3596:                         return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3597:                     fin
C1141:
C1131:
; 3598:                 fin
C1128:
C1129:
; 3599:                 tknptr = saveptr
	DB	$66,$16			; LLW	22
	DB	$7A,<D0501,>D0501	; SAW	D0501
; 3600:             fin
C1124:
C1125:
; 3601:             rewind_10(tknptr)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$54,<C0680,>C0680	; CALL	C0680
; 3602:             type = parse_value_11(0)
	DB	$00			; ZERO
	DB	$54,<C0805,>C0805	; CALL	C0805
	DB	$74,$02			; SLB	2
; 3603:             if type
	DB	$64,$02			; LLB	2
	DB	$4C,<C1142,>C1142	; SKPFLS	C1142
; 3604:                 if token == SET_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C1144,>C1144	; SKPFLS	C1144
; 3605:                     drop parse_expr_01()
	DB	$54,<C0001,>C0001	; CALL	C0001
	DB	$30			; DROP
; 3606:                     if type & XBYTE_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$22			; CB	34
	DB	$14			; BAND
	DB	$4C,<C1146,>C1146	; SKPFLS	C1146
; 3607:                         emit_sb()
	DB	$54,<C0499,>C0499	; CALL	C0499
; 3608:                     else
	DB	$50,<C1147,>C1147	; SKIP	C1147
C1146:
; 3609:                         emit_sw()
	DB	$54,<C0501,>C0501	; CALL	C0501
; 3610:                     fin
C1147:
; 3611:                 elsif token == SETLIST_TKN
	DB	$50,<C1145,>C1145	; SKIP	C1145
C1144:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$B9			; CB	185
	DB	$40			; ISEQ
	DB	$4C,<C1148,>C1148	; SKPFLS	C1148
; 3612:                         return parse_setlist_21(0, type);
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$64,$02			; LLB	2
	DB	$54,<C0983,>C0983	; CALL	C0983
	DB	$5A			; LEAVE
; 3613:                 else
	DB	$50,<C1145,>C1145	; SKIP	C1145
C1148:
; 3614:                     if type & BPTR_TYPE
	DB	$64,$02			; LLB	2
	DB	$2A,$20			; CB	32
	DB	$14			; BAND
	DB	$4C,<C1149,>C1149	; SKPFLS	C1149
; 3615:                         emit_lb()
	DB	$54,<C0487,>C0487	; CALL	C0487
; 3616:                     elsif type & WPTR_TYPE
	DB	$50,<C1150,>C1150	; SKIP	C1150
C1149:
	DB	$64,$02			; LLB	2
	DB	$2A,$40			; CB	64
	DB	$14			; BAND
	DB	$4C,<C1151,>C1151	; SKPFLS	C1151
; 3617:                         emit_lw()
	DB	$54,<C0489,>C0489	; CALL	C0489
; 3618:                     fin
C1151:
C1150:
; 3619:                 fin
C1145:
; 3620:             else
	DB	$50,<C1143,>C1143	; SKIP	C1143
C1142:
; 3621:                 return parse_err_11(@bad_syntax)
	DB	$30			; DROP
	DB	$26,<D0670,>D0670	; LA	D0670
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3622:             fin
C1143:
; 3623:     wend
C1027:
	DB	$30			; DROP
; 3624:     if scan_01() <> EOL_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C1152,>C1152	; SKPFLS	C1152
; 3625:         return parse_err_11(@bad_syntax)
	DB	$26,<D0670,>D0670	; LA	D0670
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3626:     fin
C1152:
C1153:
; 3627:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3628: end
; 3629: def parse_var_11(type)
C1154:					; parse_var_11()
					; type = 2
; 3630:     byte consttype, constsize, idlen
					; consttype = 4
					; constsize = 5
					; idlen = 6
; 3631:     word idptr, constval, arraysize, size
					; idptr = 7
					; constval = 9
					; arraysize = 11
					; size = 13
; 3632: 
; 3633:     idlen = 0
	JSR	INTERP
	DB	$58,$0F,$01		; ENTER	15,1
	DB	$00			; ZERO
	DB	$74,$06			; SLB	6
; 3634:     size  = 1
	DB	$2A,$01			; CB	1
	DB	$76,$0D			; SLW	13
; 3635:     if scan_01() == ID_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C1156,>C1156	; SKPFLS	C1156
; 3636:         idptr = tknptr
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$76,$07			; SLW	7
; 3637:         idlen = tknlen
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$74,$06			; SLB	6
; 3638:         if scan_01() == OPEN_BRACKET_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$DB			; CB	219
	DB	$40			; ISEQ
	DB	$4C,<C1158,>C1158	; SKPFLS	C1158
; 3639:             size = 0
	DB	$00			; ZERO
	DB	$76,$0D			; SLW	13
; 3640:             drop parse_constexpr_21(@size, @constsize)
	DB	$28,$0D			; LLA	13
	DB	$28,$05			; LLA	5
	DB	$54,<C0932,>C0932	; CALL	C0932
	DB	$30			; DROP
; 3641:             if token <> CLOSE_BRACKET_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$DD			; CB	221
	DB	$42			; ISNE
	DB	$4C,<C1160,>C1160	; SKPFLS	C1160
; 3642:                 return parse_err_11(@no_close_bracket)
	DB	$26,<D0842,>D0842	; LA	D0842
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3643:             fin
C1160:
C1161:
; 3644:             drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3645:         fin
C1158:
C1159:
; 3646:     fin
C1156:
C1157:
; 3647:     if type == WORD_TYPE
	DB	$66,$02			; LLW	2
	DB	$2A,$04			; CB	4
	DB	$40			; ISEQ
	DB	$4C,<C1162,>C1162	; SKPFLS	C1162
; 3648:         size = size * 2
	DB	$66,$0D			; LLW	13
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$76,$0D			; SLW	13
; 3649:     fin
C1162:
C1163:
; 3650:     if token == SET_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$BD			; CB	189
	DB	$40			; ISEQ
	DB	$4C,<C1164,>C1164	; SKPFLS	C1164
; 3651:         if infunc
	DB	$68,<D1102,>D1102	; LAB	D1102
	DB	$4C,<C1166,>C1166	; SKPFLS	C1166
; 3652:             return parse_err_11(@no_local_init)
	DB	$26,<D0934,>D0934	; LA	D0934
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3653:         fin
C1166:
C1167:
; 3654:         if idlen
	DB	$64,$06			; LLB	6
	DB	$4C,<C1168,>C1168	; SKPFLS	C1168
; 3655:             drop iddata_add_41(idptr, idlen, type, 0)
	DB	$66,$07			; LLW	7
	DB	$64,$06			; LLB	6
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$54,<C0732,>C0732	; CALL	C0732
	DB	$30			; DROP
; 3656:         fin
C1168:
C1169:
; 3657:         consttype = parse_constexpr_21(@constval, @constsize)
	DB	$28,$09			; LLA	9
	DB	$28,$05			; LLA	5
	DB	$54,<C0932,>C0932	; CALL	C0932
	DB	$74,$04			; SLB	4
; 3658:         if consttype
	DB	$64,$04			; LLB	4
	DB	$4C,<C1170,>C1170	; SKPFLS	C1170
; 3659:             arraysize = emit_data_41(type, consttype, constval, constsize)
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$66,$09			; LLW	9
	DB	$64,$05			; LLB	5
	DB	$54,<C0473,>C0473	; CALL	C0473
	DB	$76,$0B			; SLW	11
; 3660:             while token == COMMA_TKN
C1172:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$AC			; CB	172
	DB	$40			; ISEQ
	DB	$4C,<C1173,>C1173	; SKPFLS	C1173
; 3661:                 consttype = parse_constexpr_21(@constval, @constsize)
	DB	$28,$09			; LLA	9
	DB	$28,$05			; LLA	5
	DB	$54,<C0932,>C0932	; CALL	C0932
	DB	$74,$04			; SLB	4
; 3662:                 if consttype
	DB	$64,$04			; LLB	4
	DB	$4C,<C1174,>C1174	; SKPFLS	C1174
; 3663:                     arraysize = arraysize + emit_data_41(type, consttype, constval, constsize)
	DB	$66,$0B			; LLW	11
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$66,$09			; LLW	9
	DB	$64,$05			; LLB	5
	DB	$54,<C0473,>C0473	; CALL	C0473
	DB	$02			; ADD
	DB	$76,$0B			; SLW	11
; 3664:                 else
	DB	$50,<C1175,>C1175	; SKIP	C1175
C1174:
; 3665:                     return parse_err_11(@bad_decl)
	DB	$26,<D0612,>D0612	; LA	D0612
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3666:                 fin
C1175:
; 3667:             loop
	DB	$50,<C1172,>C1172	; SKIP	C1172
C1173:
; 3668:             if token <> EOL_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C1176,>C1176	; SKPFLS	C1176
; 3669:                 return parse_err_11(@no_close_bracket)
	DB	$26,<D0842,>D0842	; LA	D0842
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3670:             fin
C1176:
C1177:
; 3671:             iddata_size_30(PTR_TYPE, size, arraysize);
	DB	$2A,$60			; CB	96
	DB	$66,$0D			; LLW	13
	DB	$66,$0B			; LLW	11
	DB	$54,<C0738,>C0738	; CALL	C0738
; 3672:         else
	DB	$50,<C1171,>C1171	; SKIP	C1171
C1170:
; 3673:             return parse_err_11(@bad_decl)
	DB	$26,<D0612,>D0612	; LA	D0612
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3674:         fin
C1171:
; 3675:     elsif idlen
	DB	$50,<C1165,>C1165	; SKIP	C1165
C1164:
	DB	$64,$06			; LLB	6
	DB	$4C,<C1178,>C1178	; SKPFLS	C1178
; 3676:         if infunc
	DB	$68,<D1102,>D1102	; LAB	D1102
	DB	$4C,<C1179,>C1179	; SKPFLS	C1179
; 3677:             drop idlocal_add_41(idptr, idlen, type, size)
	DB	$66,$07			; LLW	7
	DB	$64,$06			; LLB	6
	DB	$66,$02			; LLW	2
	DB	$66,$0D			; LLW	13
	DB	$54,<C0724,>C0724	; CALL	C0724
	DB	$30			; DROP
; 3678:         else
	DB	$50,<C1180,>C1180	; SKIP	C1180
C1179:
; 3679:             drop iddata_add_41(idptr, idlen, type, size)
	DB	$66,$07			; LLW	7
	DB	$64,$06			; LLB	6
	DB	$66,$02			; LLW	2
	DB	$66,$0D			; LLW	13
	DB	$54,<C0732,>C0732	; CALL	C0732
	DB	$30			; DROP
; 3680:         fin
C1180:
; 3681:     fin
C1178:
C1165:
; 3682:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3683: end
; 3684: def parse_vars_01
C1181:					; parse_vars_01()
; 3685:     byte idlen, type, size
					; idlen = 2
					; type = 3
					; size = 4
; 3686:     word value, idptr
					; value = 5
					; idptr = 7
; 3687: 
; 3688:     when token
	JSR	INTERP
	DB	$58,$09,$00		; ENTER	9,0
	DB	$68,<D0495,>D0495	; LAB	D0495
; 3689:         is CONST_TKN
	DB	$2A,$80			; CB	128
	DB	$3E,<C1184,>C1184	; SKPNE	C1184
; 3690:             if scan_01() <> ID_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$D6			; CB	214
	DB	$42			; ISNE
	DB	$4C,<C1185,>C1185	; SKPFLS	C1185
; 3691:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3692:             fin
C1185:
C1186:
; 3693:             idptr = tknptr;
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$76,$07			; SLW	7
; 3694:             idlen = tknlen
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$74,$02			; SLB	2
; 3695:             if scan_01() <> SET_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$BD			; CB	189
	DB	$42			; ISNE
	DB	$4C,<C1187,>C1187	; SKPFLS	C1187
; 3696:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3697:             fin
C1187:
C1188:
; 3698:             if !parse_constexpr_21(@value, @size)
	DB	$28,$05			; LLA	5
	DB	$28,$04			; LLA	4
	DB	$54,<C0932,>C0932	; CALL	C0932
	DB	$20			; NOT
	DB	$4C,<C1189,>C1189	; SKPFLS	C1189
; 3699:                 return parse_err_11(@bad_cnst)
	DB	$30			; DROP
	DB	$26,<D0581,>D0581	; LA	D0581
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3700:             fin
C1189:
C1190:
; 3701:             drop idconst_add_31(idptr, idlen, value)
	DB	$66,$07			; LLW	7
	DB	$64,$02			; LLB	2
	DB	$66,$05			; LLW	5
	DB	$54,<C0750,>C0750	; CALL	C0750
	DB	$30			; DROP
; 3702:         is BYTE_TKN
	DB	$50,<C1183,>C1183	; SKIP	C1183
C1184:
	DB	$2A,$81			; CB	129
	DB	$3E,<C1191,>C1191	; SKPNE	C1191
; 3703:             type = BYTE_TYPE
	DB	$2A,$02			; CB	2
	DB	$74,$03			; SLB	3
; 3704:             repeat
C1193:
; 3705:                 if !parse_var_11(type)
	DB	$64,$03			; LLB	3
	DB	$54,<C1154,>C1154	; CALL	C1154
	DB	$20			; NOT
	DB	$4C,<C1194,>C1194	; SKPFLS	C1194
; 3706:                     return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3707:                 fin
C1194:
C1195:
; 3708:             until token <> COMMA_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C1193,>C1193	; SKPFLS	C1193
C1192:
; 3709:         is WORD_TKN
	DB	$50,<C1183,>C1183	; SKIP	C1183
C1191:
	DB	$2A,$82			; CB	130
	DB	$3E,<C1196,>C1196	; SKPNE	C1196
; 3710:             type = WORD_TYPE
	DB	$2A,$04			; CB	4
	DB	$74,$03			; SLB	3
; 3711:             repeat
C1198:
; 3712:                 if !parse_var_11(type)
	DB	$64,$03			; LLB	3
	DB	$54,<C1154,>C1154	; CALL	C1154
	DB	$20			; NOT
	DB	$4C,<C1199,>C1199	; SKPFLS	C1199
; 3713:                     return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3714:                 fin
C1199:
C1200:
; 3715:             until token <> COMMA_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C1198,>C1198	; SKPFLS	C1198
C1197:
; 3716:         is FUNC_TKN
	DB	$50,<C1183,>C1183	; SKIP	C1183
C1196:
	DB	$2A,$9E			; CB	158
	DB	$3E,<C1201,>C1201	; SKPNE	C1201
; 3717:             repeat
C1203:
; 3718:                 if scan_01() == ID_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C1204,>C1204	; SKPFLS	C1204
; 3719:                     drop idfunc_add_31(tknptr, tknlen, ctag_new_01())
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 3720:                 else
	DB	$50,<C1205,>C1205	; SKIP	C1205
C1204:
; 3721:                     return parse_err_11(@bad_decl)
	DB	$30			; DROP
	DB	$26,<D0612,>D0612	; LA	D0612
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3722:                 fin
C1205:
; 3723:             until scan_01() <> COMMA_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C1203,>C1203	; SKPFLS	C1203
C1202:
; 3724:         is EOL_TKN
	DB	$50,<C1183,>C1183	; SKIP	C1183
C1201:
	DB	$2A,$02			; CB	2
	DB	$3E,<C1206,>C1206	; SKPNE	C1206
; 3725:             return TRUE
	DB	$30			; DROP
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3726:         otherwise
	DB	$50,<C1183,>C1183	; SKIP	C1183
C1206:
; 3727:             return FALSE
	DB	$30			; DROP
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3728:     wend
C1183:
	DB	$30			; DROP
; 3729:     return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3730: end
; 3731: def parse_func_01
C1208:					; parse_func_01()
; 3732:     byte opt, cfnparms
					; opt = 2
					; cfnparms = 3
; 3733:     word func_tag, idptr
					; func_tag = 4
					; idptr = 6
; 3734: 
; 3735:     if token == IFUNC_TKN or token == NFUNC_TKN
	JSR	INTERP
	DB	$58,$08,$00		; ENTER	8,0
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$95			; CB	149
	DB	$40			; ISEQ
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$96			; CB	150
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C1210,>C1210	; SKPFLS	C1210
; 3736:         opt = token - IFUNC_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$95			; CB	149
	DB	$04			; SUB
	DB	$74,$02			; SLB	2
; 3737:         if scan_01() <> ID_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$D6			; CB	214
	DB	$42			; ISNE
	DB	$4C,<C1212,>C1212	; SKPFLS	C1212
; 3738:             return parse_err_11(@bad_decl)
	DB	$26,<D0612,>D0612	; LA	D0612
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3739:         fin
C1212:
C1213:
; 3740:         cfnparms = 0
	DB	$00			; ZERO
	DB	$74,$03			; SLB	3
; 3741:         infunc   = TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$78,<D1102,>D1102	; SAB	D1102
; 3742:         idptr = idglobal_lookup_21(tknptr, tknlen)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$54,<C0722,>C0722	; CALL	C0722
	DB	$76,$06			; SLW	6
; 3743:         if idptr
	DB	$66,$06			; LLW	6
	DB	$4C,<C1214,>C1214	; SKPFLS	C1214
; 3744:             func_tag = (idptr):idval
	DB	$66,$06			; LLW	6
	DB	$62			; LW
	DB	$76,$04			; SLW	4
; 3745:         else
	DB	$50,<C1215,>C1215	; SKIP	C1215
C1214:
; 3746:             func_tag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$76,$04			; SLW	4
; 3747:             drop idfunc_add_31(tknptr, tknlen, func_tag)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$66,$04			; LLW	4
	DB	$54,<C0748,>C0748	; CALL	C0748
	DB	$30			; DROP
; 3748:         fin
C1215:
; 3749:         emit_codetag_10(func_tag)
	DB	$66,$04			; LLW	4
	DB	$54,<C0461,>C0461	; CALL	C0461
; 3750:         retfunc_tag = ctag_new_01()
	DB	$54,<C0445,>C0445	; CALL	C0445
	DB	$7A,<D1105,>D1105	; SAW	D1105
; 3751:         idlocal_init()
	DB	$54,<C0754,>C0754	; CALL	C0754
; 3752:         if scan_01() == OPEN_PAREN_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$A8			; CB	168
	DB	$40			; ISEQ
	DB	$4C,<C1216,>C1216	; SKPFLS	C1216
; 3753:           repeat
C1219:
; 3754:         if scan_01() == ID_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$D6			; CB	214
	DB	$40			; ISEQ
	DB	$4C,<C1220,>C1220	; SKPFLS	C1220
; 3755:           cfnparms = cfnparms + 1
	DB	$64,$03			; LLB	3
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$03			; SLB	3
; 3756:           drop idlocal_add_41(tknptr, tknlen, WORD_TYPE, 2)
	DB	$6A,<D0501,>D0501	; LAW	D0501
	DB	$68,<D0496,>D0496	; LAB	D0496
	DB	$2A,$04			; CB	4
	DB	$2A,$02			; CB	2
	DB	$54,<C0724,>C0724	; CALL	C0724
	DB	$30			; DROP
; 3757:           drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3758:         fin
C1220:
C1221:
; 3759:             until token <> COMMA_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$AC			; CB	172
	DB	$42			; ISNE
	DB	$4C,<C1219,>C1219	; SKPFLS	C1219
C1218:
; 3760:             if token <> CLOSE_PAREN_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$A9			; CB	169
	DB	$42			; ISNE
	DB	$4C,<C1222,>C1222	; SKPFLS	C1222
; 3761:                 return parse_err_11(@bad_decl)
	DB	$26,<D0612,>D0612	; LA	D0612
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3762:             fin
C1222:
C1223:
; 3763:             drop scan_01()
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$30			; DROP
; 3764:         fin
C1216:
C1217:
; 3765:         while parse_vars_01()
C1224:
	DB	$54,<C1181,>C1181	; CALL	C1181
	DB	$4C,<C1225,>C1225	; SKPFLS	C1225
; 3766:             drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3767:         loop
	DB	$50,<C1224,>C1224	; SKIP	C1224
C1225:
; 3768:         emit_enter_20(framesize, cfnparms)
	DB	$6A,<D0484,>D0484	; LAW	D0484
	DB	$64,$03			; LLB	3
	DB	$54,<C0599,>C0599	; CALL	C0599
; 3769:         prevstmnt = 0
	DB	$00			; ZERO
	DB	$78,<D1104,>D1104	; SAB	D1104
; 3770:         while parse_stmnt_01()
C1226:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1227,>C1227	; SKPFLS	C1227
; 3771:             drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3772:         loop
	DB	$50,<C1226,>C1226	; SKIP	C1226
C1227:
; 3773:         infunc = FALSE
	DB	$00			; ZERO
	DB	$78,<D1102,>D1102	; SAB	D1102
; 3774:         if token <> END_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$87			; CB	135
	DB	$42			; ISNE
	DB	$4C,<C1228,>C1228	; SKPFLS	C1228
; 3775:             return parse_err_11(@bad_syntax)
	DB	$26,<D0670,>D0670	; LA	D0670
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3776:         fin
C1228:
C1229:
; 3777:         if scan_01() <> EOL_TKN
	DB	$54,<C0629,>C0629	; CALL	C0629
	DB	$2A,$02			; CB	2
	DB	$42			; ISNE
	DB	$4C,<C1230,>C1230	; SKPFLS	C1230
; 3778:             return parse_err_11(@bad_syntax)
	DB	$26,<D0670,>D0670	; LA	D0670
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$5A			; LEAVE
; 3779:         fin
C1230:
C1231:
; 3780:         if prevstmnt <> RETURN_TKN
	DB	$68,<D1104,>D1104	; LAB	D1104
	DB	$2A,$99			; CB	153
	DB	$42			; ISNE
	DB	$4C,<C1232,>C1232	; SKPFLS	C1232
; 3781:             emit_leave_10(framesize)
	DB	$6A,<D0484,>D0484	; LAW	D0484
	DB	$54,<C0595,>C0595	; CALL	C0595
; 3782:         fin
C1232:
C1233:
; 3783:         return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3784:     elsif token == EOL_TKN
	DB	$50,<C1211,>C1211	; SKIP	C1211
C1210:
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$02			; CB	2
	DB	$40			; ISEQ
	DB	$4C,<C1234,>C1234	; SKPFLS	C1234
; 3785:         return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5A			; LEAVE
; 3786:     fin
C1234:
C1211:
; 3787:     return FALSE
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 3788: end
; 3789: def parse_module_01
C0002:					; parse_module_01()
; 3790:     entrypoint = 0
	JSR	INTERP
	DB	$00			; ZERO
	DB	$7A,<D0492,>D0492	; SAW	D0492
; 3791:     idglobal_init()
	DB	$54,<C0752,>C0752	; CALL	C0752
; 3792:     idlocal_init()
	DB	$54,<C0754,>C0754	; CALL	C0754
; 3793:     if nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$4C,<C1236,>C1236	; SKPFLS	C1236
; 3794:         while parse_vars_01()
C1238:
	DB	$54,<C1181,>C1181	; CALL	C1181
	DB	$4C,<C1239,>C1239	; SKPFLS	C1239
; 3795:             drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3796:         loop
	DB	$50,<C1238,>C1238	; SKIP	C1238
C1239:
; 3797:         while parse_func_01()
C1240:
	DB	$54,<C1208,>C1208	; CALL	C1208
	DB	$4C,<C1241,>C1241	; SKPFLS	C1241
; 3798:             drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3799:         loop
	DB	$50,<C1240,>C1240	; SKIP	C1240
C1241:
; 3800:         if token <> DONE_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$98			; CB	152
	DB	$42			; ISNE
	DB	$4C,<C1242,>C1242	; SKPFLS	C1242
; 3801:             emit_start()
	DB	$54,<C0603,>C0603	; CALL	C0603
; 3802:             prevstmnt = 0
	DB	$00			; ZERO
	DB	$78,<D1104,>D1104	; SAB	D1104
; 3803:             while parse_stmnt_01()
C1244:
	DB	$54,<C1023,>C1023	; CALL	C1023
	DB	$4C,<C1245,>C1245	; SKPFLS	C1245
; 3804:                 drop nextln_01()
	DB	$54,<C0682,>C0682	; CALL	C0682
	DB	$30			; DROP
; 3805:             loop
	DB	$50,<C1244,>C1244	; SKIP	C1244
C1245:
; 3806:             if token <> DONE_TKN
	DB	$68,<D0495,>D0495	; LAB	D0495
	DB	$2A,$98			; CB	152
	DB	$42			; ISNE
	DB	$4C,<C1246,>C1246	; SKPFLS	C1246
; 3807:                 drop parse_err_11(@no_done)
	DB	$26,<D0921,>D0921	; LA	D0921
	DB	$54,<C0441,>C0441	; CALL	C0441
	DB	$30			; DROP
; 3808:             fin
C1246:
C1247:
; 3809:             if prevstmnt <> EXIT_TKN
	DB	$68,<D1104,>D1104	; LAB	D1104
	DB	$2A,$9C			; CB	156
	DB	$42			; ISNE
	DB	$4C,<C1248,>C1248	; SKPFLS	C1248
; 3810:                 emit_const_10(0)
	DB	$00			; ZERO
	DB	$54,<C0482,>C0482	; CALL	C0482
; 3811:                 emit_exit()
	DB	$54,<C0605,>C0605	; CALL	C0605
; 3812:             fin
C1248:
C1249:
; 3813:         fin
C1242:
C1243:
; 3814:         ; dumpsym(idglobal_tbl, globals)
; 3815:         ; prstr(@entrypt_str)
; 3816:         ; prword(entrypoint)
; 3817:         ; crout()
; 3818:         ; keyin_01()
; 3819:         return TRUE
	DB	$2C,$FF,$FF		; CW	-1
	DB	$5C			; RET
; 3820:     fin
C1236:
C1237:
; 3821:     return FALSE
	DB	$00			; ZERO
	DB	$5C			; RET
; 3822: end
; 3823: ;
; 3824: ; Init editor
; 3825: ;
; 3826: if !(^machid & $80)
START:	; JSR	INTERP
	DB	$2C,$98,$BF		; CW	49048
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C1250,>C1250	; SKPFLS	C1250
; 3827:     flags = uppercase ? shiftlock
	DB	$2A,$08			; CB	8
	DB	$2A,$80			; CB	128
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 3828:     keyin_01 = @keyin2_01
	DB	$26,<C0240,>C0240	; LA	C0240
	DB	$7A,<D0210,>D0210	; SAW	D0210
; 3829: else
	DB	$50,<C1251,>C1251	; SKIP	C1251
C1250:
; 3830:     keyin_01 = @keyin2e_01
	DB	$26,<C0236,>C0236	; LA	C0236
	DB	$7A,<D0210,>D0210	; SAW	D0210
; 3831: fin
C1251:
; 3832: inittxtbuf()
	DB	$54,<C0085,>C0085	; CALL	C0085
; 3833: if ^argbuff
	DB	$2C,$06,$20		; CW	8198
	DB	$60			; LB
	DB	$4C,<C1252,>C1252	; SKPFLS	C1252
; 3834:     strcpy_20(argbuff, @txtfile)
	DB	$2C,$06,$20		; CW	8198
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0047,>C0047	; CALL	C0047
; 3835:     prstr(@txtfile)
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0019,>C0019	; CALL	C0019
; 3836:     readtxt_10(@txtfile)
	DB	$26,<D0141,>D0141	; LA	D0141
	DB	$54,<C0135,>C0135	; CALL	C0135
; 3837: else
	DB	$50,<C1253,>C1253	; SKIP	C1253
C1252:
; 3838:     numlines = 1
	DB	$2A,$01			; CB	1
	DB	$7A,<D0206,>D0206	; SAW	D0206
; 3839: fin
C1253:
; 3840: curschr  = '+'
	DB	$2A,$2B			; CB	43
	DB	$78,<D0199,>D0199	; SAB	D0199
; 3841: flags = flags ? insmode
	DB	$68,<D0192,>D0192	; LAB	D0192
	DB	$2A,$02			; CB	2
	DB	$16			; IOR
	DB	$78,<D0192,>D0192	; SAB	D0192
; 3842: drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0202,>D0202	; LAW	D0202
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$54,<C0165,>C0165	; CALL	C0165
; 3843: curson()
	DB	$54,<C0177,>C0177	; CALL	C0177
; 3844: editmode()
	DB	$54,<C0332,>C0332	; CALL	C0332
; 3845: done
	DB	$5C			; RET
