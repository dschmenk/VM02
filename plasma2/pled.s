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
;   61: const maxlines      = 1500
					; maxlines = 1500
;   62: const maxfill       = 1524
					; maxfill = 1524
;   63: const iobuffer      = $0800
					; iobuffer = 2048
;   64: const databuff      = $0C00
					; databuff = 3072
;   65: const strlinbuf     = $1000
					; strlinbuf = 4096
;   66: const strheapmap    = $1F00
					; strheapmap = 7936
;   67: const strheapmsz    = 224 ; $E0 = 28K is memory@16 bytes per bit map, 128 bytes per 8 bit map, 1K bytes per 8 byte map
					; strheapmsz = 224
;   68: const maxlnlen      = 79
					; maxlnlen = 79
;   69: const strheap       = $4800
					; strheap = 18432
;   70: const strheasz      = $7000
					; strheasz = 28672
;   71: const pgjmp         = 16
					; pgjmp = 16
;   72: const changed       = 1
					; changed = 1
;   73: const insmode       = 2
					; insmode = 2
;   74: const showcurs      = 4
					; showcurs = 4
;   75: const uppercase     = 8
					; uppercase = 8
;   76: const shiftlock     = 128
					; shiftlock = 128
;   77: ;
;   78: ; Editor variables
;   79: ;
;   80: byte nullstr[]      = ""
D0048:					; nullstr
	DB	$00
;   81: byte version[]      = "PLASMA ][ EDITOR VERSION 0.8 "
D0049:					; version
	DB	$1D
	DB	$50,$4C,$41,$53,$4D,$41,$20,$5D
	DB	$5B,$20,$45,$44,$49,$54,$4F,$52
	DB	$20,$56,$45,$52,$53,$49,$4F,$4E
	DB	$20,$30,$2E,$38,$20
;   82: byte errorstr[]     = "ERROR: $"
D0079:					; errorstr
	DB	$08
	DB	$45,$52,$52,$4F,$52,$3A,$20,$24
;   83: byte okstr[]        = "OK"
D0088:					; okstr
	DB	$02
	DB	$4F,$4B
;   84: byte perr
D0091:	DS	1			; perr
;   85: byte outofmem[]     = "OUT OF MEMORY!"
D0092:					; outofmem
	DB	$0E
	DB	$4F,$55,$54,$20,$4F,$46,$20,$4D
	DB	$45,$4D,$4F,$52,$59,$21
;   86: byte losechng[]     = "LOSE CHANGES TO FILE (Y/N)?"
D0107:					; losechng
	DB	$1B
	DB	$4C,$4F,$53,$45,$20,$43,$48,$41
	DB	$4E,$47,$45,$53,$20,$54,$4F,$20
	DB	$46,$49,$4C,$45,$20,$28,$59,$2F
	DB	$4E,$29,$3F
;   87: ;byte emiterr[]     = "EMIT CODE/DATA MISMATCH"
;   88: byte untitled[]     = "UNTITLED"
D0135:					; untitled
	DB	$08
	DB	$55,$4E,$54,$49,$54,$4C,$45,$44
;   89: byte txtfile[64]    = "UNTITLED.PLA"
D0144:					; txtfile
	DB	$0C
	DB	$55,$4E,$54,$49,$54,$4C,$45,$44
	DB	$2E,$50,$4C,$41
	DS	$33
;   90: byte flags          = 0
D0195:					; flags
	DB	$00
;   91: byte flash          = 0
D0196:					; flash
	DB	$00
;   92: byte cursx, cursy, scrnleft, curscol, underchr, curschr
D0197:	DS	1			; cursx
D0198:	DS	1			; cursy
D0199:	DS	1			; scrnleft
D0200:	DS	1			; curscol
D0201:	DS	1			; underchr
D0202:	DS	1			; curschr
;   93: word cursrow, scrntop, cursptr
D0203:	DS	2			; cursrow
D0205:	DS	2			; scrntop
D0207:	DS	2			; cursptr
;   94: word numlines       = 0
D0209:					; numlines
	DW	$0000
;   95: word cutbuf         = 0
D0211:					; cutbuf
	DW	$0000
;   96: word keyin_01
D0213:	DS	2			; keyin_01
;   97: ;
;   98: ; Predeclared functions
;   99: ;
;  100: func cmdmode
;  101: ;
;  102: ; Utility functions
;  103: ;
;  104: ; Defines for ASM routines
;  105: ;
;  106: asm equates
C0001:					; equates()
;  107:         TMP     EQU     $F0
        TMP     EQU     $F0
;  108:         TMPL    EQU     TMP
        TMPL    EQU     TMP
;  109:         TMPH    EQU     TMP+1
        TMPH    EQU     TMP+1
;  110:         SRC     EQU     TMP
        SRC     EQU     TMP
;  111:         SRCL    EQU     SRC
        SRCL    EQU     SRC
;  112:         SRCH    EQU     SRC+1
        SRCH    EQU     SRC+1
;  113:         DST     EQU     SRC+2
        DST     EQU     SRC+2
;  114:         DSTL    EQU     DST
        DSTL    EQU     DST
;  115:         DSTH    EQU     DST+1
        DSTH    EQU     DST+1
;  116:         ESP     EQU     DST+2
        ESP     EQU     DST+2
;  117: end
	RTS
;  118: ; CALL 6502 ROUTINE
;  119: ; ROMCALL(AREG, XREG, YREG, STATUS, ADDR)
;  120: ;
;  121: asm romcall
C0003:					; romcall()
;  122:         PHP
        PHP
;  123:         LDA     ESTKL,X
        LDA     ESTKL,X
;  124:         STA     TMPL
        STA     TMPL
;  125:         LDA     ESTKH,X
        LDA     ESTKH,X
;  126:         STA     TMPH
        STA     TMPH
;  127:         INX
        INX
;  128:         LDA     ESTKL,X
        LDA     ESTKL,X
;  129:         PHA
        PHA
;  130:         INX
        INX
;  131:         LDA     ESTKL,X
        LDA     ESTKL,X
;  132:         TAY
        TAY
;  133:         INX
        INX
;  134:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  135:         PHA
        PHA
;  136:         LDA     ESTKL,X
        LDA     ESTKL,X
;  137:         INX
        INX
;  138:         STX     ESP
        STX     ESP
;  139:         TAX
        TAX
;  140:         PLA
        PLA
;  141:         BIT     ROMIN
        BIT     ROMIN
;  142:         PLP
        PLP
;  143:         JSR     JMPTMP
        JSR     JMPTMP
;  144:         PHP
        PHP
;  145:         BIT     LCBNK2
        BIT     LCBNK2
;  146:         STA     REGVALS+0
        STA     REGVALS+0
;  147:         STX     REGVALS+1
        STX     REGVALS+1
;  148:         STY     REGVALS+2
        STY     REGVALS+2
;  149:         PLA
        PLA
;  150:         STA     REGVALS+3
        STA     REGVALS+3
;  151:         LDX     ESP
        LDX     ESP
;  152:         LDA     #<REGVALS
        LDA     #<REGVALS
;  153:         LDY     #>REGVALS
        LDY     #>REGVALS
;  154:         STA     ESTKL,X
        STA     ESTKL,X
;  155:         STY     ESTKH,X
        STY     ESTKH,X
;  156:         PLP
        PLP
;  157:         RTS
        RTS
;  158: JMPTMP: JMP (TMP)
JMPTMP: JMP (TMP)
;  159: REGVALS: DS 4
REGVALS: DS 4
;  160: end
	RTS
;  161: ;
;  162: ; CALL PRODOS
;  163: ; SYSCALL(CMD, PARAMS)
;  164: ;
;  165: asm syscall
C0005:					; syscall()
;  166:         LDA     ESTKL,X
        LDA     ESTKL,X
;  167:         LDY     ESTKH,X
        LDY     ESTKH,X
;  168:         STA     PARAMS
        STA     PARAMS
;  169:         STY     PARAMS+1
        STY     PARAMS+1
;  170:         INX
        INX
;  171:         LDA     ESTKL,X
        LDA     ESTKL,X
;  172:         STA     CMD
        STA     CMD
;  173:         STX     ESP
        STX     ESP
;  174:         JSR     $BF00
        JSR     $BF00
;  175: CMD:    DB      00
CMD:    DB      00
;  176: PARAMS: DW      0000
PARAMS: DW      0000
;  177:         BIT     LCBNK2
        BIT     LCBNK2
;  178:         LDX     ESP
        LDX     ESP
;  179:         STA     ESTKL,X
        STA     ESTKL,X
;  180:         LDY     #$00
        LDY     #$00
;  181:         STY     ESTKH,X
        STY     ESTKH,X
;  182: end
	RTS
;  183: ;
;  184: ; SET MEMORY TO VALUE
;  185: ; MEMSET(VALUE, ADDR, SIZE)
;  186: ;
;  187: asm memset
C0007:					; memset()
;  188:         LDY     #$00
        LDY     #$00
;  189:         LDA     ESTKL+1,X
        LDA     ESTKL+1,X
;  190:         STA     DSTL
        STA     DSTL
;  191:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  192:         STA     DSTH
        STA     DSTH
;  193:         INC     ESTKL,X
        INC     ESTKL,X
;  194:         INC     ESTKH,X
        INC     ESTKH,X
;  195: SETMEM: DEC     ESTKL,X
SETMEM: DEC     ESTKL,X
;  196:         BNE     :+
        BNE     :+
;  197:         DEC     ESTKH,X
        DEC     ESTKH,X
;  198:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  199: :       LDA     ESTKL+2,X
:       LDA     ESTKL+2,X
;  200:         STA     (DST),Y
        STA     (DST),Y
;  201:         INY
        INY
;  202:         BNE     :+
        BNE     :+
;  203:         INC     DSTH
        INC     DSTH
;  204: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  205:         BNE     :+
        BNE     :+
;  206:         DEC     ESTKH,X
        DEC     ESTKH,X
;  207:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  208: :       LDA     ESTKH+2,X
:       LDA     ESTKH+2,X
;  209:         STA     (DST),Y
        STA     (DST),Y
;  210:         INY
        INY
;  211:         BNE     SETMEM
        BNE     SETMEM
;  212:         INC     DSTH
        INC     DSTH
;  213:         BNE     SETMEM
        BNE     SETMEM
;  214: MEMEXIT: INX
MEMEXIT: INX
;  215:         INX
        INX
;  216:         INX
        INX
;  217: end
	RTS
;  218: ;
;  219: ; COPY MEMORY
;  220: ; MEMCPY(SRCADDR, DSTADDR, SIZE)
;  221: ;
;  222: asm memcpy
C0009:					; memcpy()
;  223:         LDY     #$00
        LDY     #$00
;  224:         LDA     ESTKL,X
        LDA     ESTKL,X
;  225:         BNE     :+
        BNE     :+
;  226:         LDA     ESTKH,X
        LDA     ESTKH,X
;  227:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  228: :       LDA     ESTKL+1,X
:       LDA     ESTKL+1,X
;  229:         STA     DSTL
        STA     DSTL
;  230:         LDA     ESTKH+1,X
        LDA     ESTKH+1,X
;  231:         STA     DSTH
        STA     DSTH
;  232:         LDA     ESTKL+2,X
        LDA     ESTKL+2,X
;  233:         STA     SRCL
        STA     SRCL
;  234:         LDA     ESTKH+2,X
        LDA     ESTKH+2,X
;  235:         STA     SRCH
        STA     SRCH
;  236:         CMP     DSTH
        CMP     DSTH
;  237:         BCC     REVCPY
        BCC     REVCPY
;  238:         BNE     FORCPY
        BNE     FORCPY
;  239:         LDA     SRCL
        LDA     SRCL
;  240:         CMP     DSTL
        CMP     DSTL
;  241:         BCS     FORCPY
        BCS     FORCPY
;  242: REVCPY:             ; REVERSE DIRECTION COPY
REVCPY:             ; REVERSE DIRECTION COPY
;  243: ;       CLC
;  244:         LDA     ESTKL,X
        LDA     ESTKL,X
;  245:         ADC     DSTL
        ADC     DSTL
;  246:         STA     DSTL
        STA     DSTL
;  247:         LDA     ESTKH,X
        LDA     ESTKH,X
;  248:         ADC     DSTH
        ADC     DSTH
;  249:         STA     DSTH
        STA     DSTH
;  250:         CLC
        CLC
;  251:         LDA     ESTKL,X
        LDA     ESTKL,X
;  252:         ADC     SRCL
        ADC     SRCL
;  253:         STA     SRCL
        STA     SRCL
;  254:         LDA     ESTKH,X
        LDA     ESTKH,X
;  255:         ADC     SRCH
        ADC     SRCH
;  256:         STA     SRCH
        STA     SRCH
;  257:         INC     ESTKH,X
        INC     ESTKH,X
;  258: REVCPYLP:
REVCPYLP:
;  259:         LDA     DSTL
        LDA     DSTL
;  260:         BNE     :+
        BNE     :+
;  261:         DEC     DSTH
        DEC     DSTH
;  262: :       DEC     DSTL
:       DEC     DSTL
;  263:         LDA     SRCL
        LDA     SRCL
;  264:         BNE     :+
        BNE     :+
;  265:         DEC     SRCH
        DEC     SRCH
;  266: :       DEC     SRCL
:       DEC     SRCL
;  267:         LDA     (SRC),Y
        LDA     (SRC),Y
;  268:         STA     (DST),Y
        STA     (DST),Y
;  269:         DEC     ESTKL,X
        DEC     ESTKL,X
;  270:         BNE     REVCPYLP
        BNE     REVCPYLP
;  271:         DEC     ESTKH,X
        DEC     ESTKH,X
;  272:         BNE     REVCPYLP
        BNE     REVCPYLP
;  273:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  274: FORCPY: INC     ESTKH,X
FORCPY: INC     ESTKH,X
;  275: FORCPYLP:
FORCPYLP:
;  276:         LDA     (SRC),Y
        LDA     (SRC),Y
;  277:         STA     (DST),Y
        STA     (DST),Y
;  278:         INC     DSTL
        INC     DSTL
;  279:         BNE     :+
        BNE     :+
;  280:         INC     DSTH
        INC     DSTH
;  281: :       INC     SRCL
:       INC     SRCL
;  282:         BNE     :+
        BNE     :+
;  283:         INC     SRCH
        INC     SRCH
;  284: :       DEC     ESTKL,X
:       DEC     ESTKL,X
;  285:         BNE     FORCPYLP
        BNE     FORCPYLP
;  286:         DEC     ESTKH,X
        DEC     ESTKH,X
;  287:         BNE     FORCPYLP
        BNE     FORCPYLP
;  288:         BEQ     MEMEXIT
        BEQ     MEMEXIT
;  289: end
	RTS
;  290: ;
;  291: ; CHAR OUT
;  292: ; COUT(CHAR)
;  293: ;
;  294: asm cout
C0011:					; cout()
;  295:         LDA     ESTKL,X
        LDA     ESTKL,X
;  296:         INX
        INX
;  297:         ORA     #$80
        ORA     #$80
;  298:         BIT     ROMIN
        BIT     ROMIN
;  299:         JSR     $FDED
        JSR     $FDED
;  300:         BIT     LCBNK2
        BIT     LCBNK2
;  301: end
	RTS
;  302: ;
;  303: ; CHAR IN
;  304: ; RDKEY()
;  305: ;
;  306: asm cin
C0013:					; cin()
;  307:         BIT     ROMIN
        BIT     ROMIN
;  308:         STX     ESP
        STX     ESP
;  309:         JSR     $FD0C
        JSR     $FD0C
;  310:         LDX     ESP
        LDX     ESP
;  311:         BIT     LCBNK2
        BIT     LCBNK2
;  312:         DEX
        DEX
;  313:         AND     #$7F
        AND     #$7F
;  314:         STA     ESTKL,X
        STA     ESTKL,X
;  315:         LDY     #$00
        LDY     #$00
;  316:         STY     ESTKH,X
        STY     ESTKH,X
;  317: end
	RTS
;  318: ;
;  319: ; PRINT STRING
;  320: ; PRSTR(STR)
;  321: ;
;  322: asm prstr
C0015:					; prstr()
;  323:         LDY     #$00
        LDY     #$00
;  324:         LDA     ESTKL,X
        LDA     ESTKL,X
;  325:         STA     SRCL
        STA     SRCL
;  326:         LDA     ESTKH,X
        LDA     ESTKH,X
;  327:         STA     SRCH
        STA     SRCH
;  328:         BIT     ROMIN
        BIT     ROMIN
;  329:         LDA     (SRC),Y
        LDA     (SRC),Y
;  330:         STA     ESTKL,X
        STA     ESTKL,X
;  331:         BEQ     :+
        BEQ     :+
;  332: _PRS1:  INY
_PRS1:  INY
;  333:         LDA     (SRC),Y
        LDA     (SRC),Y
;  334:         ORA     #$80
        ORA     #$80
;  335:         JSR     $FDED
        JSR     $FDED
;  336:         TYA
        TYA
;  337:         CMP     ESTKL,X
        CMP     ESTKL,X
;  338:         BNE     _PRS1
        BNE     _PRS1
;  339: :       INX
:       INX
;  340:         BIT     LCBNK2
        BIT     LCBNK2
;  341: end
	RTS
;  342: ;
;  343: ; READ STRING
;  344: ; STR = RDSTR(PROMPTCHAR)
;  345: ;
;  346: asm rdstr
C0017:					; rdstr()
;  347:         LDA     ESTKL,X
        LDA     ESTKL,X
;  348:         STA     $33
        STA     $33
;  349:         STX     ESP
        STX     ESP
;  350:         BIT     ROMIN
        BIT     ROMIN
;  351:         JSR     $FD6A
        JSR     $FD6A
;  352:         BIT     LCBNK2
        BIT     LCBNK2
;  353:         STX     $01FF
        STX     $01FF
;  354: :       LDA     $01FF,X
:       LDA     $01FF,X
;  355:         AND     #$7F
        AND     #$7F
;  356:         STA     $01FF,X
        STA     $01FF,X
;  357:         DEX
        DEX
;  358:         BPL     :-
        BPL     :-
;  359:         LDX     ESP
        LDX     ESP
;  360:         LDA     #$FF
        LDA     #$FF
;  361:         STA     ESTKL,X
        STA     ESTKL,X
;  362:         LDA     #$01
        LDA     #$01
;  363:         STA     ESTKH,X
        STA     ESTKH,X
;  364: end
	RTS
;  365: ;
;  366: ; EXIT
;  367: ;
;  368: asm exit
C0019:					; exit()
;  369:         JSR $BF00
        JSR $BF00
;  370:         DB  $65
        DB  $65
;  371:         DW  EXITTBL
        DW  EXITTBL
;  372: EXITTBL:
EXITTBL:
;  373:         DB  4
        DB  4
;  374:         DB  0
        DB  0
;  375: end
	RTS
;  376: ;
;  377: ; ProDOS routines
;  378: ;
;  379: def getpfx_11(path)
C0021:					; getpfx_11()
					; path = 2
;  380:     byte params[3]
					; params = 4
;  381: 
;  382:     ^path    = 0
	JSR	_INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$70			; SB
;  383:     params.0 = 1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  384:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  385:     perr     = syscall($C7, @params)
	DB	$2A,$C7			; CB	199
	DB	$28,$04			; LLA	4
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  386:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  387: end
;  388: def setpfx_11(path)
C0023:					; setpfx_11()
					; path = 2
;  389:     byte params[3]
					; params = 4
;  390: 
;  391:     params.0 = 1
	JSR	_INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  392:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  393:     perr     = syscall($C6, @params)
	DB	$2A,$C6			; CB	198
	DB	$28,$04			; LLA	4
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  394:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  395: end
;  396: def open_21(path, buff)
C0025:					; open_21()
					; path = 2
					; buff = 4
;  397:     byte params[6]
					; params = 6
;  398: 
;  399:     params.0 = 3
	JSR	_INTERP
	DB	$58,$0C,$02		; ENTER	12,2
	DB	$28,$06			; LLA	6
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  400:     params:1 = path
	DB	$28,$07			; LLA	7
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  401:     params:3 = buff
	DB	$28,$09			; LLA	9
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  402:     params.5 = 0
	DB	$28,$0B			; LLA	11
	DB	$00			; ZERO
	DB	$70			; SB
;  403:     perr     = syscall($C8, @params)
	DB	$2A,$C8			; CB	200
	DB	$28,$06			; LLA	6
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  404:     return params.5
	DB	$28,$0B			; LLA	11
	DB	$60			; LB
	DB	$5A			; LEAVE
;  405: end
;  406: def close_11(refnum)
C0027:					; close_11()
					; refnum = 2
;  407:     byte params[2]
					; params = 4
;  408: 
;  409:     params.0 = 1
	JSR	_INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  410:     params.1 = refnum
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  411:     perr     = syscall($CC, @params)
	DB	$2A,$CC			; CB	204
	DB	$28,$04			; LLA	4
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  412:     return perr
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$5A			; LEAVE
;  413: end
;  414: def read_31(refnum, buff, len)
C0029:					; read_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  415:     byte params[8]
					; params = 8
;  416: 
;  417:     params.0 = 4
	JSR	_INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  418:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  419:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  420:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  421:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  422:     perr     = syscall($CA, @params)
	DB	$2A,$CA			; CB	202
	DB	$28,$08			; LLA	8
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  423:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  424: end
;  425: def write_31(refnum, buff, len)
C0031:					; write_31()
					; refnum = 2
					; buff = 4
					; len = 6
;  426:     byte params[8]
					; params = 8
;  427: 
;  428:     params.0 = 4
	JSR	_INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  429:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  430:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  431:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  432:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  433:     perr     = syscall($CB, @params)
	DB	$2A,$CB			; CB	203
	DB	$28,$08			; LLA	8
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  434:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  435: end
;  436: def create_41(path, access, type, aux)
C0033:					; create_41()
					; path = 2
					; access = 4
					; type = 6
					; aux = 8
;  437:     byte params[12]
					; params = 10
;  438: 
;  439:     params.0  = 7
	JSR	_INTERP
	DB	$58,$16,$04		; ENTER	22,4
	DB	$28,$0A			; LLA	10
	DB	$2A,$07			; CB	7
	DB	$70			; SB
;  440:     params:1  = path
	DB	$28,$0B			; LLA	11
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  441:     params.3  = access
	DB	$28,$0D			; LLA	13
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  442:     params.4  = type
	DB	$28,$0E			; LLA	14
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  443:     params:5  = aux
	DB	$28,$0F			; LLA	15
	DB	$66,$08			; LLW	8
	DB	$72			; SW
;  444:     params.7  = $1
	DB	$28,$11			; LLA	17
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  445:     params:8  = 0
	DB	$28,$12			; LLA	18
	DB	$00			; ZERO
	DB	$72			; SW
;  446:     params:10 = 0
	DB	$28,$14			; LLA	20
	DB	$00			; ZERO
	DB	$72			; SW
;  447:     perr      = syscall($C0, @params)
	DB	$2A,$C0			; CB	192
	DB	$28,$0A			; LLA	10
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  448:     return perr
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$5A			; LEAVE
;  449: end
;  450: def destroy_11(path)
C0035:					; destroy_11()
					; path = 2
;  451:     byte params[12]
					; params = 4
;  452: 
;  453:     params.0 = 1
	JSR	_INTERP
	DB	$58,$10,$01		; ENTER	16,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  454:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  455:     perr     = syscall($C1, @params)
	DB	$2A,$C1			; CB	193
	DB	$28,$04			; LLA	4
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  456:     return perr
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$5A			; LEAVE
;  457: end
;  458: def newline_31(refnum, emask, nlchar)
C0037:					; newline_31()
					; refnum = 2
					; emask = 4
					; nlchar = 6
;  459:     byte params[4]
					; params = 8
;  460: 
;  461:     params.0 = 3
	JSR	_INTERP
	DB	$58,$0C,$03		; ENTER	12,3
	DB	$28,$08			; LLA	8
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  462:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  463:     params.2 = emask
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  464:     params.3 = nlchar
	DB	$28,$0B			; LLA	11
	DB	$66,$06			; LLW	6
	DB	$70			; SB
;  465:     perr     = syscall($C9, @params)
	DB	$2A,$C9			; CB	201
	DB	$28,$08			; LLA	8
	DB	$54,<C0005,>C0005	; CALL	C0005
	DB	$78,<D0091,>D0091	; SAB	D0091
;  466:     return perr
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$5A			; LEAVE
;  467: end
;  468: 
;  469: ;=====================================
;  470: ;
;  471: ;            Editor
;  472: ;
;  473: ;=====================================
;  474: 
;  475: def crout
C0039:					; crout()
;  476:     cout($0D)
	JSR	_INTERP
	DB	$2A,$0D			; CB	13
	DB	$54,<C0011,>C0011	; CALL	C0011
;  477: end
	DB	$5C			; RET
;  478: def bell
C0041:					; bell()
;  479:     drop romcall(0, 0, 0, 0, $FBDD)
	JSR	_INTERP
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DD,$FB		; CW	64477
	DB	$54,<C0003,>C0003	; CALL	C0003
	DB	$30			; DROP
;  480: end
	DB	$5C			; RET
;  481: ;
;  482: ; Memory management routines
;  483: ;
;  484: defopt strcpy_20(srcstr, dststr)
C0043:					; strcpy_20()
					; srcstr = 2
					; dststr = 4
;  485:     byte strlen
					; strlen = 6
;  486: 
;  487:     strlen = ^srcstr
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
;  488:     while (srcstr).[strlen] == $8D or (srcstr).[strlen] == $A0
	INX
C0045:
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
	JMP	C0046
:
;  489:         strlen = strlen - 1
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
;  490:     loop
	INX
	JMP	C0045
C0046:
;  491:     ^dststr = strlen
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
;  492:     memcpy(srcstr + 1, dststr + 1, strlen)
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
	JSR	C0009
;  493: end
	JMP	LEAVE
;  494: defopt heapaddr_21(ofst, mask)
C0047:					; heapaddr_21()
					; ofst = 2
					; mask = 4
;  495:     word addr
					; addr = 6
;  496: 
;  497:     addr = (ofst << 7) + strheap
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
	LDA	#$48
	STA	ESTKH,X
	JSR	ADD
	LDY	#$06
	LDA	ESTKL,X
	STA	(FRMP),Y
	INY
	LDA	ESTKH,X
	STA	(FRMP),Y
;  498:     while !(mask & 1)
	INX
C0049:
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
	JMP	C0050
:
;  499:         addr = addr + 16
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
;  500:         mask = mask >> 1
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
;  501:     loop
	INX
	JMP	C0049
C0050:
;  502:     return addr
	DEX
	LDY	#$06
	LDA	(FRMP),Y
	STA	ESTKL,X
	INY
	LDA	(FRMP),Y
	STA	ESTKH,X
	JMP	LEAVE
;  503: end
;  504: defopt sizemask_11(size)
C0051:					; sizemask_11()
					; size = 2
;  505:     if size <= 16
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
	JMP	C0053
:
;  506:         return $01
	DEX
	LDA	#$01
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  507:     elsif size <= 32
	JMP	C0054
C0053:
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
	JMP	C0055
:
;  508:         return $03
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  509:     elsif size <= 48
	JMP	C0054
C0055:
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
	JMP	C0056
:
;  510:         return $07
	DEX
	LDA	#$07
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  511:     elsif size <= 64
	JMP	C0054
C0056:
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
	JMP	C0057
:
;  512:         return $0F
	DEX
	LDA	#$0F
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  513:     elsif size <= 80
	JMP	C0054
C0057:
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
	JMP	C0058
:
;  514:         return $1F
	DEX
	LDA	#$1F
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	JMP	LEAVE
;  515:     fin
C0058:
C0054:
;  516:     return 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
;  517: end
;  518: defopt heapalloc_11(size)
C0059:					; heapalloc_11()
					; size = 2
;  519:     byte szmask, i
					; szmask = 4
					; i = 5
;  520:     word mapmask
					; mapmask = 6
;  521: 
;  522:     szmask = sizemask_11(size)
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
	JSR	C0051
	LDY	#$04
	LDA	ESTKL,X
	STA	(FRMP),Y
;  523:     for i = strheapmsz - 1 downto 0
	LDA	#$E0
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
C0062:
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
	JMP	C0061
:
	LDA	ESTKL,X
	BNE	:+
	DEC	ESTKH,X
:	DEC	ESTKL,X
;  524:         if strheapmap.[i] <> $FF
	DEX
	STY	ESTKL,X
	LDA	#$1F
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
	JMP	C0063
:
;  525:             mapmask = szmask
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
;  526:             repeat
	INX
C0066:
;  527:                 if strheapmap.[i] & mapmask
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$1F
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
	JMP	C0067
:
;  528:                     mapmask = mapmask << 1
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
;  529:                 else
	INX
	JMP	C0068
C0067:
;  530:                     strheapmap.[i] = strheapmap.[i] ? mapmask
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$1F
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
	LDA	#$1F
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
;  531:                     return heapaddr_21(i, mapmask)
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
	JSR	C0047
	JMP	LEAVE
;  532:                 fin
C0068:
;  533:             until mapmask & $100
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
	JMP	C0066
:
C0065:
;  534:         fin
C0063:
C0064:
;  535:     next
	JMP	C0062
C0061:
;  536:     bell()
	INX
	JSR	C0041
;  537:     prstr(@outofmem)
	DEX
	LDA	#<D0092
	STA	ESTKL,X
	LDA	#>D0092
	STA	ESTKH,X
	JSR	C0015
;  538:     return 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	JMP	LEAVE
;  539: end
;  540: def freestr_10(strptr)
C0069:					; freestr_10()
					; strptr = 2
;  541:     byte mask, ofst
					; mask = 4
					; ofst = 5
;  542: 
;  543:     if strptr and strptr <> @nullstr
	JSR	_INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$42			; ISNE
	DB	$24			; LAND
	DB	$4C,<C0071,>C0071	; SKPFLS	C0071
;  544:         mask = sizemask_11(^strptr + 1)
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0051,>C0051	; CALL	C0051
	DB	$74,$04			; SLB	4
;  545:         ofst = (strptr - strheap) >> 4
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$48		; CW	18432
	DB	$04			; SUB
	DB	$2A,$04			; CB	4
	DB	$1C			; SHR
	DB	$74,$05			; SLB	5
;  546:         mask = mask << (ofst & $07)
	DB	$64,$04			; LLB	4
	DB	$64,$05			; LLB	5
	DB	$2A,$07			; CB	7
	DB	$14			; BAND
	DB	$1A			; SHL
	DB	$74,$04			; SLB	4
;  547:         ofst = ofst >> 3
	DB	$64,$05			; LLB	5
	DB	$2A,$03			; CB	3
	DB	$1C			; SHR
	DB	$74,$05			; SLB	5
;  548:         strheapmap.[ofst] = strheapmap.[ofst] & #mask
	DB	$2C,$00,$1F		; CW	7936
	DB	$64,$05			; LLB	5
	DB	$02			; IDXB
	DB	$2C,$00,$1F		; CW	7936
	DB	$64,$05			; LLB	5
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$64,$04			; LLB	4
	DB	$12			; COMP
	DB	$14			; BAND
	DB	$70			; SB
;  549:     fin
C0071:
C0072:
;  550: end
	DB	$5A			; LEAVE
;  551: def newstr_11(strptr)
C0073:					; newstr_11()
					; strptr = 2
;  552:     byte strlen
					; strlen = 4
;  553:     word newptr
					; newptr = 5
;  554: 
;  555:     strlen = ^strptr
	JSR	_INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$74,$04			; SLB	4
;  556:     while (strptr).[strlen] == $8D or (strptr).[strlen] == $A0
C0075:
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
	DB	$4C,<C0076,>C0076	; SKPFLS	C0076
;  557:         strlen = strlen - 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$04			; SLB	4
;  558:     loop
	DB	$50,<C0075,>C0075	; SKIP	C0075
C0076:
;  559:     if strlen == 0
	DB	$64,$04			; LLB	4
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0077,>C0077	; SKPFLS	C0077
;  560:         return @nullstr
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$5A			; LEAVE
;  561:     fin
C0077:
C0078:
;  562:     newptr = heapalloc_11(strlen + 1)
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0059,>C0059	; CALL	C0059
	DB	$76,$05			; SLW	5
;  563:     if newptr
	DB	$66,$05			; LLW	5
	DB	$4C,<C0079,>C0079	; SKPFLS	C0079
;  564:         memcpy(strptr, newptr, strlen + 1)
	DB	$66,$02			; LLW	2
	DB	$66,$05			; LLW	5
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0009,>C0009	; CALL	C0009
;  565:         ^newptr = strlen
	DB	$66,$05			; LLW	5
	DB	$64,$04			; LLB	4
	DB	$70			; SB
;  566:         return newptr
	DB	$66,$05			; LLW	5
	DB	$5A			; LEAVE
;  567:     fin
C0079:
C0080:
;  568:     return @nullstr
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$5A			; LEAVE
;  569: end
;  570: def inittxtbuf
C0081:					; inittxtbuf()
;  571:     word i
					; i = 2
;  572: 
;  573:     memset(0, strheapmap, strheapmsz)
	JSR	_INTERP
	DB	$58,$04,$00		; ENTER	4,0
	DB	$00			; ZERO
	DB	$2C,$00,$1F		; CW	7936
	DB	$2A,$E0			; CB	224
	DB	$54,<C0007,>C0007	; CALL	C0007
;  574:     memset(@nullstr, strlinbuf, maxfill * 2)
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$2C,$00,$10		; CW	4096
	DB	$2C,$F4,$05		; CW	1524
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0007,>C0007	; CALL	C0007
;  575:     numlines   = 0
	DB	$00			; ZERO
	DB	$7A,<D0209,>D0209	; SAW	D0209
;  576:     cursrow    = 0
	DB	$00			; ZERO
	DB	$7A,<D0203,>D0203	; SAW	D0203
;  577:     curscol    = 0
	DB	$00			; ZERO
	DB	$78,<D0200,>D0200	; SAB	D0200
;  578:     cursx      = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
;  579:     cursy      = 0
	DB	$00			; ZERO
	DB	$78,<D0198,>D0198	; SAB	D0198
;  580:     scrnleft   = 0
	DB	$00			; ZERO
	DB	$78,<D0199,>D0199	; SAB	D0199
;  581:     scrntop    = 0
	DB	$00			; ZERO
	DB	$7A,<D0205,>D0205	; SAW	D0205
;  582:     cutbuf     = 0
	DB	$00			; ZERO
	DB	$7A,<D0211,>D0211	; SAW	D0211
;  583: end
	DB	$5A			; LEAVE
;  584: ;
;  585: ; Case conversion/printing routines
;  586: ;
;  587: def caseconv_11(chr)
C0083:					; caseconv_11()
					; chr = 2
;  588:     if flags & uppercase
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0085,>C0085	; SKPFLS	C0085
;  589:         if chr & $E0 == $E0
	DB	$66,$02			; LLW	2
	DB	$2A,$E0			; CB	224
	DB	$14			; BAND
	DB	$2A,$E0			; CB	224
	DB	$40			; ISEQ
	DB	$4C,<C0087,>C0087	; SKPFLS	C0087
;  590:             chr = chr - $E0
	DB	$66,$02			; LLW	2
	DB	$2A,$E0			; CB	224
	DB	$04			; SUB
	DB	$76,$02			; SLW	2
;  591:         fin
C0087:
C0088:
;  592:     fin
C0085:
C0086:
;  593:     return chr
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  594: end
;  595: def strupper_10(strptr)
C0089:					; strupper_10()
					; strptr = 2
;  596:     byte i, chr
					; i = 4
					; chr = 5
;  597: 
;  598:     for i = ^strptr downto 1
	JSR	_INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
C0092:
	DB	$6C,$04			; DLB	4
	DB	$2A,$01			; CB	1
	DB	$38,<C0091,>C0091	; SKPLT	C0091
	DB	$0E			; DECR
;  599:         chr = (strptr).[i]
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$74,$05			; SLB	5
;  600:         if chr & $E0 == $E0
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$14			; BAND
	DB	$2A,$E0			; CB	224
	DB	$40			; ISEQ
	DB	$4C,<C0093,>C0093	; SKPFLS	C0093
;  601:             (strptr).[i] = chr - $E0
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$04			; SUB
	DB	$70			; SB
;  602:         fin
C0093:
C0094:
;  603:     next
	DB	$50,<C0092,>C0092	; SKIP	C0092
C0091:
	DB	$30			; DROP
;  604: end
	DB	$5A			; LEAVE
;  605: def strlower_10(strptr)
C0095:					; strlower_10()
					; strptr = 2
;  606:     byte i, chr
					; i = 4
					; chr = 5
;  607: 
;  608:     for i = ^strptr downto 1
	JSR	_INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
C0098:
	DB	$6C,$04			; DLB	4
	DB	$2A,$01			; CB	1
	DB	$38,<C0097,>C0097	; SKPLT	C0097
	DB	$0E			; DECR
;  609:         chr = (strptr).[i]
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$74,$05			; SLB	5
;  610:         if chr & $E0 == $00
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$14			; BAND
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0099,>C0099	; SKPFLS	C0099
;  611:             (strptr).[i] = chr + $E0
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$64,$05			; LLB	5
	DB	$2A,$E0			; CB	224
	DB	$02			; ADD
	DB	$70			; SB
;  612:         fin
C0099:
C0100:
;  613:     next
	DB	$50,<C0098,>C0098	; SKIP	C0098
C0097:
	DB	$30			; DROP
;  614: end
	DB	$5A			; LEAVE
;  615: def txtupper
C0101:					; txtupper()
;  616:     word i, strptr
					; i = 2
					; strptr = 4
;  617: 
;  618:     flags = flags ? uppercase
	JSR	_INTERP
	DB	$58,$06,$00		; ENTER	6,0
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$08			; CB	8
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
;  619:     for i = numlines - 1 downto 0
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
C0104:
	DB	$6E,$02			; DLW	2
	DB	$00			; ZERO
	DB	$38,<C0103,>C0103	; SKPLT	C0103
	DB	$0E			; DECR
;  620:         strupper_10(strlinbuf:[i])
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0089,>C0089	; CALL	C0089
;  621:     next
	DB	$50,<C0104,>C0104	; SKIP	C0104
C0103:
	DB	$30			; DROP
;  622: end
	DB	$5A			; LEAVE
;  623: def txtlower
C0105:					; txtlower()
;  624:     word i, strptr
					; i = 2
					; strptr = 4
;  625: 
;  626:     flags = flags & #uppercase
	JSR	_INTERP
	DB	$58,$06,$00		; ENTER	6,0
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2C,$F7,$FF		; CW	-9
	DB	$14			; BAND
	DB	$78,<D0195,>D0195	; SAB	D0195
;  627:     for i = numlines - 1 downto 0
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
C0108:
	DB	$6E,$02			; DLW	2
	DB	$00			; ZERO
	DB	$38,<C0107,>C0107	; SKPLT	C0107
	DB	$0E			; DECR
;  628:         strlower_10(strlinbuf:[i])
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0095,>C0095	; CALL	C0095
;  629:     next
	DB	$50,<C0108,>C0108	; SKIP	C0108
C0107:
	DB	$30			; DROP
;  630: end
	DB	$5A			; LEAVE
;  631: def prbyte_10(h)
C0109:					; prbyte_10()
					; h = 2
;  632:     cout('$')
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0011,>C0011	; CALL	C0011
;  633:     drop romcall(h, 0, 0, 0, $FDDA)
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DA,$FD		; CW	64986
	DB	$54,<C0003,>C0003	; CALL	C0003
	DB	$30			; DROP
;  634: end
	DB	$5A			; LEAVE
;  635: def prword_10(h)
C0111:					; prword_10()
					; h = 2
;  636:     cout('$')
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$2A,$24			; CB	36
	DB	$54,<C0011,>C0011	; CALL	C0011
;  637:     drop romcall(h >> 8, h, 0, 0, $F941)
	DB	$66,$02			; LLW	2
	DB	$2A,$08			; CB	8
	DB	$1C			; SHR
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$41,$F9		; CW	63809
	DB	$54,<C0003,>C0003	; CALL	C0003
	DB	$30			; DROP
;  638: end
	DB	$5A			; LEAVE
;  639: def print_10(i)
C0113:					; print_10()
					; i = 2
;  640:     byte numstr[7]
					; numstr = 4
;  641:     byte place, sign
					; place = 11
					; sign = 12
;  642: 
;  643:     place = 6
	JSR	_INTERP
	DB	$58,$0D,$01		; ENTER	13,1
	DB	$2A,$06			; CB	6
	DB	$74,$0B			; SLB	11
;  644:     if i < 0
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$46			; ISLT
	DB	$4C,<C0115,>C0115	; SKPFLS	C0115
;  645:         sign = 1
	DB	$2A,$01			; CB	1
	DB	$74,$0C			; SLB	12
;  646:         i    = -i
	DB	$66,$02			; LLW	2
	DB	$10			; NEG
	DB	$76,$02			; SLW	2
;  647:     else
	DB	$50,<C0116,>C0116	; SKIP	C0116
C0115:
;  648:         sign = 0
	DB	$00			; ZERO
	DB	$74,$0C			; SLB	12
;  649:     fin
C0116:
;  650:     while i >= 10
C0117:
	DB	$66,$02			; LLW	2
	DB	$2A,$0A			; CB	10
	DB	$48			; ISGE
	DB	$4C,<C0118,>C0118	; SKPFLS	C0118
;  651:         i =, numstr[place] = i % 10 + '0'
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
;  652:         place              = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  653:     loop
	DB	$50,<C0117,>C0117	; SKIP	C0117
C0118:
;  654:     numstr[place] = i + '0'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$2A,$30			; CB	48
	DB	$02			; ADD
	DB	$70			; SB
;  655:     place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  656:     if sign
	DB	$64,$0C			; LLB	12
	DB	$4C,<C0119,>C0119	; SKPFLS	C0119
;  657:         numstr[place] = '-'
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$2D			; CB	45
	DB	$70			; SB
;  658:         place         = place - 1
	DB	$64,$0B			; LLB	11
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$0B			; SLB	11
;  659:     fin
C0119:
C0120:
;  660:     numstr[place] = 6 - place
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$2A,$06			; CB	6
	DB	$64,$0B			; LLB	11
	DB	$04			; SUB
	DB	$70			; SB
;  661:     prstr(@numstr[place])
	DB	$28,$04			; LLA	4
	DB	$64,$0B			; LLB	11
	DB	$02			; IDXB
	DB	$54,<C0015,>C0015	; CALL	C0015
;  662: end
	DB	$5A			; LEAVE
;  663: def nametostr_30(namestr, len, strptr)
C0121:					; nametostr_30()
					; namestr = 2
					; len = 4
					; strptr = 6
;  664:     ^strptr = len
	JSR	_INTERP
	DB	$58,$08,$03		; ENTER	8,3
	DB	$66,$06			; LLW	6
	DB	$66,$04			; LLW	4
	DB	$70			; SB
;  665:     memcpy(namestr, strptr + 1, len)
	DB	$66,$02			; LLW	2
	DB	$66,$06			; LLW	6
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$04			; LLW	4
	DB	$54,<C0009,>C0009	; CALL	C0009
;  666: end
	DB	$5A			; LEAVE
;  667: ;def toupper_11(c)
;  668: ;   if c >= 'a'
;  669: ;       if c <= 'z'
;  670: ;           return c - $20
;  671: ;       fin
;  672: ;   fin
;  673: ;   return c
;  674: ;end
;  675: asm toupper_11
C0123:					; toupper_11()
;  676:         LDA     ESTKL,X
        LDA     ESTKL,X
;  677:         AND     #$7F
        AND     #$7F
;  678:         CMP     #'a'
        CMP     #'a'
;  679:         BCC     :+
        BCC     :+
;  680:         CMP     #'z'+1
        CMP     #'z'+1
;  681:         BCS     :+
        BCS     :+
;  682:         SEC
        SEC
;  683:         SBC     #$20
        SBC     #$20
;  684: :       STA     ESTKL,X
:       STA     ESTKL,X
;  685: end
	RTS
;  686: asm clrhibit_10(strptr)
C0125:					; clrhibit_10()
					; strptr = 2
;  687:         LDY     #$02        ; strptr
	LDY	#4
	LDA	#1
	JSR	ENTER
        LDY     #$02        ; strptr
;  688:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  689:         STA     SRCL
        STA     SRCL
;  690:         INY
        INY
;  691:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  692:         STA     SRCH
        STA     SRCH
;  693:         LDY     #$00
        LDY     #$00
;  694:         LDA     (SRC),Y
        LDA     (SRC),Y
;  695:         BEQ     :+
        BEQ     :+
;  696:         TAY
        TAY
;  697: CLHILP: LDA     (SRC),Y
CLHILP: LDA     (SRC),Y
;  698:         AND     #$7F
        AND     #$7F
;  699:         STA     (SRC),Y
        STA     (SRC),Y
;  700:         DEY
        DEY
;  701:         BNE     CLHILP
        BNE     CLHILP
;  702: :
:
;  703: end
	JMP	LEAVE
;  704: asm sethibit_10(strptr)
C0127:					; sethibit_10()
					; strptr = 2
;  705:         LDY     #$02        ; strptr
	LDY	#4
	LDA	#1
	JSR	ENTER
        LDY     #$02        ; strptr
;  706:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  707:         STA     SRCL
        STA     SRCL
;  708:         INY
        INY
;  709:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  710:         STA     SRCH
        STA     SRCH
;  711:         LDY     #$00
        LDY     #$00
;  712:         LDA     (SRC),Y
        LDA     (SRC),Y
;  713:         BEQ     :+
        BEQ     :+
;  714:         TAY
        TAY
;  715: STHILP: LDA     (SRC),Y
STHILP: LDA     (SRC),Y
;  716:         ORA     #$80
        ORA     #$80
;  717:         STA     (SRC),Y
        STA     (SRC),Y
;  718:         DEY
        DEY
;  719:         BNE     STHILP
        BNE     STHILP
;  720: :
:
;  721: end
	JMP	LEAVE
;  722: asm cpyln_20(srcstr, dststr)
C0129:					; cpyln_20()
					; srcstr = 2
					; dststr = 4
;  723:         LDY     #$02        ; srcstr
	LDY	#6
	LDA	#2
	JSR	ENTER
        LDY     #$02        ; srcstr
;  724:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  725:         STA     SRCL
        STA     SRCL
;  726:         INY
        INY
;  727:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  728:         STA     SRCH
        STA     SRCH
;  729:         INY                 ; dststr
        INY                 ; dststr
;  730:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  731:         STA     DSTL
        STA     DSTL
;  732:         INY
        INY
;  733:         LDA     (FRMP),Y
        LDA     (FRMP),Y
;  734:         STA     DSTH
        STA     DSTH
;  735:         LDY     #$00
        LDY     #$00
;  736:         LDA     (SRC),Y
        LDA     (SRC),Y
;  737:         TAY
        TAY
;  738:         LDA     #$00
        LDA     #$00
;  739:         INY
        INY
;  740:         STA     (DST),Y
        STA     (DST),Y
;  741:         DEY
        DEY
;  742:         BEQ     :++
        BEQ     :++
;  743: CPLNLP: LDA     (SRC),Y
CPLNLP: LDA     (SRC),Y
;  744:         CMP     #$20
        CMP     #$20
;  745:         BCS     :+
        BCS     :+
;  746:         ADC     #$60
        ADC     #$60
;  747: :       AND     #$7F
:       AND     #$7F
;  748:         STA     (DST),Y
        STA     (DST),Y
;  749:         DEY
        DEY
;  750:         BNE     CPLNLP
        BNE     CPLNLP
;  751:         LDA     (SRC),Y
        LDA     (SRC),Y
;  752: :       STA     (DST),Y
:       STA     (DST),Y
;  753: end
	JMP	LEAVE
;  754: ;
;  755: ; File routines
;  756: ;
;  757: def readtxt_10(filename)
C0131:					; readtxt_10()
					; filename = 2
;  758:     byte txtbuf[81], refnum, i, j
					; txtbuf = 4
					; refnum = 85
					; i = 86
					; j = 87
;  759: 
;  760:     refnum = open_21(filename, iobuffer)
	JSR	_INTERP
	DB	$58,$58,$01		; ENTER	88,1
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0025,>C0025	; CALL	C0025
	DB	$74,$55			; SLB	85
;  761:     if refnum
	DB	$64,$55			; LLB	85
	DB	$4C,<C0133,>C0133	; SKPFLS	C0133
;  762:         drop newline_31(refnum, $7F, $0D)
	DB	$64,$55			; LLB	85
	DB	$2A,$7F			; CB	127
	DB	$2A,$0D			; CB	13
	DB	$54,<C0037,>C0037	; CALL	C0037
	DB	$30			; DROP
;  763:         repeat
C0136:
;  764:             txtbuf = read_31(refnum, @txtbuf + 1, maxlnlen)
	DB	$64,$55			; LLB	85
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$2A,$4F			; CB	79
	DB	$54,<C0029,>C0029	; CALL	C0029
	DB	$74,$04			; SLB	4
;  765:             if txtbuf
	DB	$64,$04			; LLB	4
	DB	$4C,<C0137,>C0137	; SKPFLS	C0137
;  766:                 sethibit_10(@txtbuf)
	DB	$28,$04			; LLA	4
	DB	$54,<C0127,>C0127	; CALL	C0127
;  767:                 if flags & uppercase
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0139,>C0139	; SKPFLS	C0139
;  768:                     strupper_10(@txtbuf)
	DB	$28,$04			; LLA	4
	DB	$54,<C0089,>C0089	; CALL	C0089
;  769:                 fin
C0139:
C0140:
;  770:                 strlinbuf:[numlines] = newstr_11(@txtbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$1E			; IDXW
	DB	$28,$04			; LLA	4
	DB	$54,<C0073,>C0073	; CALL	C0073
	DB	$72			; SW
;  771:                 numlines = numlines + 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0209,>D0209	; SAW	D0209
;  772:             fin
C0137:
C0138:
;  773:             if !(numlines & $0F)
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0141,>C0141	; SKPFLS	C0141
;  774:                 cout('.')
	DB	$2A,$2E			; CB	46
	DB	$54,<C0011,>C0011	; CALL	C0011
;  775:             fin
C0141:
C0142:
;  776:         until txtbuf == 0 or numlines == maxlines
	DB	$64,$04			; LLB	4
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2C,$DC,$05		; CW	1500
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0136,>C0136	; SKPFLS	C0136
C0135:
;  777:         drop close_11(refnum)
	DB	$64,$55			; LLB	85
	DB	$54,<C0027,>C0027	; CALL	C0027
	DB	$30			; DROP
;  778:     fin
C0133:
C0134:
;  779:     if numlines == 0
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0143,>C0143	; SKPFLS	C0143
;  780:         numlines = 1
	DB	$2A,$01			; CB	1
	DB	$7A,<D0209,>D0209	; SAW	D0209
;  781:     fin
C0143:
C0144:
;  782: end
	DB	$5A			; LEAVE
;  783: def writetxt_10(filename)
C0145:					; writetxt_10()
					; filename = 2
;  784:     byte txtbuf[81], refnum
					; txtbuf = 4
					; refnum = 85
;  785:     byte j, chr
					; j = 86
					; chr = 87
;  786:     word i, strptr
					; i = 88
					; strptr = 90
;  787: 
;  788:     drop destroy_11(filename)
	JSR	_INTERP
	DB	$58,$5C,$01		; ENTER	92,1
	DB	$66,$02			; LLW	2
	DB	$54,<C0035,>C0035	; CALL	C0035
	DB	$30			; DROP
;  789:     drop create_41(filename, $C3, $04, $00) ; full access, TXT file
	DB	$66,$02			; LLW	2
	DB	$2A,$C3			; CB	195
	DB	$2A,$04			; CB	4
	DB	$00			; ZERO
	DB	$54,<C0033,>C0033	; CALL	C0033
	DB	$30			; DROP
;  790:     refnum = open_21(filename, iobuffer)
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0025,>C0025	; CALL	C0025
	DB	$74,$55			; SLB	85
;  791:     if refnum == 0
	DB	$64,$55			; LLB	85
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0147,>C0147	; SKPFLS	C0147
;  792:         return
	DB	$5A			; LEAVE
;  793:     fin
C0147:
C0148:
;  794:     for i = 0 to numlines - 1
	DB	$00			; ZERO
C0150:
	DB	$6E,$58			; DLW	88
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$3A,<C0149,>C0149	; SKPGT	C0149
	DB	$0C			; INCR
;  795:         cpyln_20(strlinbuf:[i], @txtbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$58			; LLW	88
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$04			; LLA	4
	DB	$54,<C0129,>C0129	; CALL	C0129
;  796:         txtbuf = txtbuf + 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$04			; SLB	4
;  797:         txtbuf[txtbuf] = $0D
	DB	$28,$04			; LLA	4
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$2A,$0D			; CB	13
	DB	$70			; SB
;  798:         drop write_31(refnum, @txtbuf + 1, txtbuf)
	DB	$64,$55			; LLB	85
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$64,$04			; LLB	4
	DB	$54,<C0031,>C0031	; CALL	C0031
	DB	$30			; DROP
;  799:         if !(i & $0F)
	DB	$66,$58			; LLW	88
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0151,>C0151	; SKPFLS	C0151
;  800:             cout('.')
	DB	$2A,$2E			; CB	46
	DB	$54,<C0011,>C0011	; CALL	C0011
;  801:         fin
C0151:
C0152:
;  802:     next
	DB	$50,<C0150,>C0150	; SKIP	C0150
C0149:
	DB	$30			; DROP
;  803:     drop close_11(refnum)
	DB	$64,$55			; LLB	85
	DB	$54,<C0027,>C0027	; CALL	C0027
	DB	$30			; DROP
;  804: end
	DB	$5A			; LEAVE
;  805: ;
;  806: ; Screen routines
;  807: ;
;  808: def clrscrn
C0153:					; clrscrn()
;  809:     drop romcall(0, 0, 0, 0, $FC58)
	JSR	_INTERP
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$58,$FC		; CW	64600
	DB	$54,<C0003,>C0003	; CALL	C0003
	DB	$30			; DROP
;  810: end
	DB	$5C			; RET
;  811: def drawrow_30(row, ofst, strptr)
C0155:					; drawrow_30()
					; row = 2
					; ofst = 4
					; strptr = 6
;  812:     byte numchars
					; numchars = 8
;  813:     word scrnptr
					; scrnptr = 9
;  814: 
;  815:     scrnptr = txtscrn[row]
	JSR	_INTERP
	DB	$58,$0B,$03		; ENTER	11,3
	DB	$26,<D0000,>D0000	; LA	D0000
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$76,$09			; SLW	9
;  816:     if ^strptr <= ofst
	DB	$66,$06			; LLW	6
	DB	$60			; LB
	DB	$66,$04			; LLW	4
	DB	$4A			; ISLE
	DB	$4C,<C0157,>C0157	; SKPFLS	C0157
;  817:         numchars = 0
	DB	$00			; ZERO
	DB	$74,$08			; SLB	8
;  818:     else
	DB	$50,<C0158,>C0158	; SKIP	C0158
C0157:
;  819:         numchars = ^strptr - ofst
	DB	$66,$06			; LLW	6
	DB	$60			; LB
	DB	$66,$04			; LLW	4
	DB	$04			; SUB
	DB	$74,$08			; SLB	8
;  820:     fin
C0158:
;  821:     if numchars >= 40
	DB	$64,$08			; LLB	8
	DB	$2A,$28			; CB	40
	DB	$48			; ISGE
	DB	$4C,<C0159,>C0159	; SKPFLS	C0159
;  822:         numchars = 40
	DB	$2A,$28			; CB	40
	DB	$74,$08			; SLB	8
;  823:     else
	DB	$50,<C0160,>C0160	; SKIP	C0160
C0159:
;  824:         memset($A0A0, scrnptr + numchars, 40 - numchars)
	DB	$2C,$A0,$A0		; CW	41120
	DB	$66,$09			; LLW	9
	DB	$64,$08			; LLB	8
	DB	$02			; ADD
	DB	$2A,$28			; CB	40
	DB	$64,$08			; LLB	8
	DB	$04			; SUB
	DB	$54,<C0007,>C0007	; CALL	C0007
;  825:     fin
C0160:
;  826:     memcpy(strptr + ofst + 1, scrnptr, numchars)
	DB	$66,$06			; LLW	6
	DB	$66,$04			; LLW	4
	DB	$02			; ADD
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$09			; LLW	9
	DB	$64,$08			; LLB	8
	DB	$54,<C0009,>C0009	; CALL	C0009
;  827: end
	DB	$5A			; LEAVE
;  828: defopt drawscrn_20(toprow, ofst)
C0161:					; drawscrn_20()
					; toprow = 2
					; ofst = 4
;  829:     byte row, numchars
					; row = 6
					; numchars = 7
;  830:     word strptr, scrnptr
					; strptr = 8
					; scrnptr = 10
;  831: 
;  832:     for row = 0 to 23
	LDY	#12
	LDA	#2
	JSR	ENTER
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
C0164:
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
	JMP	C0163
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
;  833:         strptr  = strlinbuf:[toprow + row]
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
;  834:         scrnptr = txtscrn[row]
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
;  835:         if ^strptr <= ofst
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
	JMP	C0165
:
;  836:             numchars = 0
	DEX
	LDY	#$00
	STY	ESTKL,X
	STY	ESTKH,X
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
;  837:         else
	INX
	JMP	C0166
C0165:
;  838:             numchars = ^strptr - ofst
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
;  839:         fin
	INX
C0166:
;  840:         if numchars >= 40
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
	JMP	C0167
:
;  841:             numchars = 40
	DEX
	LDA	#$28
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	LDY	#$07
	LDA	ESTKL,X
	STA	(FRMP),Y
;  842:         else
	INX
	JMP	C0168
C0167:
;  843:             memset($A0A0, scrnptr + numchars, 40 - numchars)
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
	JSR	C0007
;  844:         fin
C0168:
;  845:         memcpy(strptr + ofst + 1, scrnptr, numchars)
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
	JSR	C0009
;  846:     next
	JMP	C0164
C0163:
;  847: end
	INX
	JMP	LEAVE
;  848: def cursoff
C0169:					; cursoff()
;  849:     if flags & showcurs
	JSR	_INTERP
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0171,>C0171	; SKPFLS	C0171
;  850:         ^cursptr = underchr
	DB	$6A,<D0207,>D0207	; LAW	D0207
	DB	$68,<D0201,>D0201	; LAB	D0201
	DB	$70			; SB
;  851:         flags = flags & #showcurs
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2C,$FB,$FF		; CW	-5
	DB	$14			; BAND
	DB	$78,<D0195,>D0195	; SAB	D0195
;  852:     fin
C0171:
C0172:
;  853: end
	DB	$5C			; RET
;  854: def curson
C0173:					; curson()
;  855:     if !(flags & showcurs)
	JSR	_INTERP
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0175,>C0175	; SKPFLS	C0175
;  856:         cursptr  = txtscrn[cursy] + cursx
	DB	$26,<D0000,>D0000	; LA	D0000
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$02			; ADD
	DB	$7A,<D0207,>D0207	; SAW	D0207
;  857:         underchr = ^cursptr
	DB	$6A,<D0207,>D0207	; LAW	D0207
	DB	$60			; LB
	DB	$78,<D0201,>D0201	; SAB	D0201
;  858:         ^cursptr = curschr
	DB	$6A,<D0207,>D0207	; LAW	D0207
	DB	$68,<D0202,>D0202	; LAB	D0202
	DB	$70			; SB
;  859:         flags = flags ? showcurs
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$04			; CB	4
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
;  860:     fin
C0175:
C0176:
;  861: end
	DB	$5C			; RET
;  862: def cursflash()
C0177:					; cursflash()
;  863:     if flags & showcurs
	JSR	_INTERP
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$04			; CB	4
	DB	$14			; BAND
	DB	$4C,<C0179,>C0179	; SKPFLS	C0179
;  864:         if flash == 0
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0181,>C0181	; SKPFLS	C0181
;  865:             ^cursptr = curschr
	DB	$6A,<D0207,>D0207	; LAW	D0207
	DB	$68,<D0202,>D0202	; LAB	D0202
	DB	$70			; SB
;  866:         elsif flash == 128
	DB	$50,<C0182,>C0182	; SKIP	C0182
C0181:
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$2A,$80			; CB	128
	DB	$40			; ISEQ
	DB	$4C,<C0183,>C0183	; SKPFLS	C0183
;  867:             ^cursptr = underchr
	DB	$6A,<D0207,>D0207	; LAW	D0207
	DB	$68,<D0201,>D0201	; LAB	D0201
	DB	$70			; SB
;  868:         fin
C0183:
C0182:
;  869:         flash = flash + 1
	DB	$68,<D0196,>D0196	; LAB	D0196
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0196,>D0196	; SAB	D0196
;  870:     fin
C0179:
C0180:
;  871: end
	DB	$5C			; RET
;  872: def redraw
C0184:					; redraw()
;  873:     cursoff()
	JSR	_INTERP
	DB	$54,<C0169,>C0169	; CALL	C0169
;  874:     drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
;  875:     curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
;  876: end
	DB	$5C			; RET
;  877: def curshome
C0186:					; curshome()
;  878:     cursoff()
	JSR	_INTERP
	DB	$54,<C0169,>C0169	; CALL	C0169
;  879:     cursrow  = 0
	DB	$00			; ZERO
	DB	$7A,<D0203,>D0203	; SAW	D0203
;  880:     curscol  = 0
	DB	$00			; ZERO
	DB	$78,<D0200,>D0200	; SAB	D0200
;  881:     cursx    = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
;  882:     cursy    = 0
	DB	$00			; ZERO
	DB	$78,<D0198,>D0198	; SAB	D0198
;  883:     scrnleft = 0
	DB	$00			; ZERO
	DB	$78,<D0199,>D0199	; SAB	D0199
;  884:     scrntop  = 0
	DB	$00			; ZERO
	DB	$7A,<D0205,>D0205	; SAW	D0205
;  885:     drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
;  886:     curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
;  887: end
	DB	$5C			; RET
;  888: def cursend
C0188:					; cursend()
;  889:     cursoff()
	JSR	_INTERP
	DB	$54,<C0169,>C0169	; CALL	C0169
;  890:     if numlines > 23
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$17			; CB	23
	DB	$44			; ISGT
	DB	$4C,<C0190,>C0190	; SKPFLS	C0190
;  891:         cursrow  = numlines - 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0203,>D0203	; SAW	D0203
;  892:         cursy    = 23
	DB	$2A,$17			; CB	23
	DB	$78,<D0198,>D0198	; SAB	D0198
;  893:         scrntop  = cursrow - 23
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$17			; CB	23
	DB	$04			; SUB
	DB	$7A,<D0205,>D0205	; SAW	D0205
;  894:     else
	DB	$50,<C0191,>C0191	; SKIP	C0191
C0190:
;  895:         cursrow  = numlines - 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0203,>D0203	; SAW	D0203
;  896:         cursy    = numlines - 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0198,>D0198	; SAB	D0198
;  897:         scrntop  = 0
	DB	$00			; ZERO
	DB	$7A,<D0205,>D0205	; SAW	D0205
;  898:     fin
C0191:
;  899:     curscol  = 0
	DB	$00			; ZERO
	DB	$78,<D0200,>D0200	; SAB	D0200
;  900:     cursx    = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
;  901:     scrnleft = 0
	DB	$00			; ZERO
	DB	$78,<D0199,>D0199	; SAB	D0199
;  902:     drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
;  903:     curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
;  904: end
	DB	$5C			; RET
;  905: def cursup
C0192:					; cursup()
;  906:     if cursrow > 0
	JSR	_INTERP
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0194,>C0194	; SKPFLS	C0194
;  907:         cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
;  908:         cursrow = cursrow - 1
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0203,>D0203	; SAW	D0203
;  909:         if cursy > 0
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0196,>C0196	; SKPFLS	C0196
;  910:             cursy = cursy - 1
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0198,>D0198	; SAB	D0198
;  911:         else
	DB	$50,<C0197,>C0197	; SKIP	C0197
C0196:
;  912:             scrntop = cursrow
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$7A,<D0205,>D0205	; SAW	D0205
;  913:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
;  914:         fin
C0197:
;  915:         curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
;  916:     fin
C0194:
C0195:
;  917: end
	DB	$5C			; RET
;  918: def pgup
C0198:					; pgup()
;  919:     byte i
					; i = 2
;  920: 
;  921:     for i = pgjmp downto 0
	JSR	_INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$10			; CB	16
C0201:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0200,>C0200	; SKPLT	C0200
	DB	$0E			; DECR
;  922:         cursup()
	DB	$54,<C0192,>C0192	; CALL	C0192
;  923:     next
	DB	$50,<C0201,>C0201	; SKIP	C0201
C0200:
	DB	$30			; DROP
;  924: end
	DB	$5A			; LEAVE
;  925: def cursdown
C0202:					; cursdown()
;  926:     if cursrow < numlines - 1
	JSR	_INTERP
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$46			; ISLT
	DB	$4C,<C0204,>C0204	; SKPFLS	C0204
;  927:         cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
;  928:         cursrow = cursrow + 1
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0203,>D0203	; SAW	D0203
;  929:         if cursy < 23
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$2A,$17			; CB	23
	DB	$46			; ISLT
	DB	$4C,<C0206,>C0206	; SKPFLS	C0206
;  930:             cursy = cursy + 1
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0198,>D0198	; SAB	D0198
;  931:         else
	DB	$50,<C0207,>C0207	; SKIP	C0207
C0206:
;  932:             scrntop = cursrow - 23
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$17			; CB	23
	DB	$04			; SUB
	DB	$7A,<D0205,>D0205	; SAW	D0205
;  933:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
;  934:         fin
C0207:
;  935:         curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
;  936:     fin
C0204:
C0205:
;  937: end
	DB	$5C			; RET
;  938: def pgdown
C0208:					; pgdown()
;  939:     byte i
					; i = 2
;  940: 
;  941:     for i = pgjmp downto 0
	JSR	_INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$10			; CB	16
C0211:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0210,>C0210	; SKPLT	C0210
	DB	$0E			; DECR
;  942:         cursdown()
	DB	$54,<C0202,>C0202	; CALL	C0202
;  943:     next
	DB	$50,<C0211,>C0211	; SKIP	C0211
C0210:
	DB	$30			; DROP
;  944: end
	DB	$5A			; LEAVE
;  945: def cursleft
C0212:					; cursleft()
;  946:     if curscol > 0
	JSR	_INTERP
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0214,>C0214	; SKPFLS	C0214
;  947:         cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
;  948:         curscol = curscol - 1
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0200,>D0200	; SAB	D0200
;  949:         if cursx > 0
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0216,>C0216	; SKPFLS	C0216
;  950:             cursx = cursx - 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0197,>D0197	; SAB	D0197
;  951:         else
	DB	$50,<C0217,>C0217	; SKIP	C0217
C0216:
;  952:             scrnleft = curscol
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$78,<D0199,>D0199	; SAB	D0199
;  953:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
;  954:         fin
C0217:
;  955:         curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
;  956:     fin
C0214:
C0215:
;  957: end
	DB	$5C			; RET
;  958: def pgleft
C0218:					; pgleft()
;  959:     byte i
					; i = 2
;  960: 
;  961:     for i = 7 downto 0
	JSR	_INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$07			; CB	7
C0221:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0220,>C0220	; SKPLT	C0220
	DB	$0E			; DECR
;  962:         cursleft()
	DB	$54,<C0212,>C0212	; CALL	C0212
;  963:     next
	DB	$50,<C0221,>C0221	; SKIP	C0221
C0220:
	DB	$30			; DROP
;  964: end
	DB	$5A			; LEAVE
;  965: def cursright
C0222:					; cursright()
;  966:     if curscol < 80
	JSR	_INTERP
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$50			; CB	80
	DB	$46			; ISLT
	DB	$4C,<C0224,>C0224	; SKPFLS	C0224
;  967:         cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
;  968:         curscol = curscol + 1
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0200,>D0200	; SAB	D0200
;  969:         if cursx < 39
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$27			; CB	39
	DB	$46			; ISLT
	DB	$4C,<C0226,>C0226	; SKPFLS	C0226
;  970:             cursx = cursx + 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0197,>D0197	; SAB	D0197
;  971:         else
	DB	$50,<C0227,>C0227	; SKIP	C0227
C0226:
;  972:             scrnleft = curscol - 39
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$27			; CB	39
	DB	$04			; SUB
	DB	$78,<D0199,>D0199	; SAB	D0199
;  973:             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
;  974:         fin
C0227:
;  975:         curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
;  976:     fin
C0224:
C0225:
;  977: end
	DB	$5C			; RET
;  978: def pgright
C0228:					; pgright()
;  979:     byte i
					; i = 2
;  980: 
;  981:     for i = 7 downto 0
	JSR	_INTERP
	DB	$58,$03,$00		; ENTER	3,0
	DB	$2A,$07			; CB	7
C0231:
	DB	$6C,$02			; DLB	2
	DB	$00			; ZERO
	DB	$38,<C0230,>C0230	; SKPLT	C0230
	DB	$0E			; DECR
;  982:         cursright()
	DB	$54,<C0222,>C0222	; CALL	C0222
;  983:     next
	DB	$50,<C0231,>C0231	; SKIP	C0231
C0230:
	DB	$30			; DROP
;  984: end
	DB	$5A			; LEAVE
;  985: ;
;  986: ; Keyboard routines
;  987: ;
;  988: def keyin2e_01
C0232:					; keyin2e_01()
;  989:     repeat
	JSR	_INTERP
C0235:
;  990:         cursflash()
	DB	$54,<C0177,>C0177	; CALL	C0177
;  991:     until ^keyboard >= 128
	DB	$2C,$00,$C0		; CW	49152
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$48			; ISGE
	DB	$4C,<C0235,>C0235	; SKPFLS	C0235
C0234:
;  992:     return ^keystrobe
	DB	$2C,$10,$C0		; CW	49168
	DB	$60			; LB
	DB	$5C			; RET
;  993: end
;  994: def keyin2_01
C0236:					; keyin2_01()
;  995:     byte key
					; key = 2
;  996: 
;  997:     repeat
	JSR	_INTERP
	DB	$58,$03,$00		; ENTER	3,0
C0239:
;  998:         cursflash()
	DB	$54,<C0177,>C0177	; CALL	C0177
;  999:         key = ^keyboard
	DB	$2C,$00,$C0		; CW	49152
	DB	$60			; LB
	DB	$74,$02			; SLB	2
; 1000:         if key == keyctrll
	DB	$64,$02			; LLB	2
	DB	$2A,$8C			; CB	140
	DB	$40			; ISEQ
	DB	$4C,<C0240,>C0240	; SKPFLS	C0240
; 1001:             drop ^keystrobe
	DB	$2C,$10,$C0		; CW	49168
	DB	$60			; LB
	DB	$30			; DROP
; 1002:             flags = flags ^ shiftlock
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$80			; CB	128
	DB	$18			; XOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1003:             key   = 0
	DB	$00			; ZERO
	DB	$74,$02			; SLB	2
; 1004:         fin
C0240:
C0241:
; 1005:     until key >= 128
	DB	$64,$02			; LLB	2
	DB	$2A,$80			; CB	128
	DB	$48			; ISGE
	DB	$4C,<C0239,>C0239	; SKPFLS	C0239
C0238:
; 1006:     drop ^keystrobe
	DB	$2C,$10,$C0		; CW	49168
	DB	$60			; LB
	DB	$30			; DROP
; 1007:     if key == keyctrln
	DB	$64,$02			; LLB	2
	DB	$2A,$8E			; CB	142
	DB	$40			; ISEQ
	DB	$4C,<C0242,>C0242	; SKPFLS	C0242
; 1008:         key = $DB ; [
	DB	$2A,$DB			; CB	219
	DB	$74,$02			; SLB	2
; 1009:     elsif key == keyctrlp
	DB	$50,<C0243,>C0243	; SKIP	C0243
C0242:
	DB	$64,$02			; LLB	2
	DB	$2A,$90			; CB	144
	DB	$40			; ISEQ
	DB	$4C,<C0244,>C0244	; SKPFLS	C0244
; 1010:         key = $DF ; _
	DB	$2A,$DF			; CB	223
	DB	$74,$02			; SLB	2
; 1011:     elsif key == keyctrlb
	DB	$50,<C0243,>C0243	; SKIP	C0243
C0244:
	DB	$64,$02			; LLB	2
	DB	$2A,$82			; CB	130
	DB	$40			; ISEQ
	DB	$4C,<C0245,>C0245	; SKPFLS	C0245
; 1012:         key = $DC ; \
	DB	$2A,$DC			; CB	220
	DB	$74,$02			; SLB	2
; 1013:     elsif key == keyarrowleft
	DB	$50,<C0243,>C0243	; SKIP	C0243
C0245:
	DB	$64,$02			; LLB	2
	DB	$2A,$88			; CB	136
	DB	$40			; ISEQ
	DB	$4C,<C0246,>C0246	; SKPFLS	C0246
; 1014:         if ^pushbttn3 < 128
	DB	$2C,$63,$C0		; CW	49251
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$46			; ISLT
	DB	$4C,<C0247,>C0247	; SKPFLS	C0247
; 1015:             key = $FF
	DB	$2A,$FF			; CB	255
	DB	$74,$02			; SLB	2
; 1016:         fin
C0247:
C0248:
; 1017:     elsif key >= $C0 and flags < shiftlock
	DB	$50,<C0243,>C0243	; SKIP	C0243
C0246:
	DB	$64,$02			; LLB	2
	DB	$2A,$C0			; CB	192
	DB	$48			; ISGE
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$80			; CB	128
	DB	$46			; ISLT
	DB	$24			; LAND
	DB	$4C,<C0249,>C0249	; SKPFLS	C0249
; 1018:         if ^pushbttn3 < 128
	DB	$2C,$63,$C0		; CW	49251
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$46			; ISLT
	DB	$4C,<C0250,>C0250	; SKPFLS	C0250
; 1019:             if key == $C0
	DB	$64,$02			; LLB	2
	DB	$2A,$C0			; CB	192
	DB	$40			; ISEQ
	DB	$4C,<C0252,>C0252	; SKPFLS	C0252
; 1020:                 key = $D0 ; P
	DB	$2A,$D0			; CB	208
	DB	$74,$02			; SLB	2
; 1021:             elsif key == $DD
	DB	$50,<C0253,>C0253	; SKIP	C0253
C0252:
	DB	$64,$02			; LLB	2
	DB	$2A,$DD			; CB	221
	DB	$40			; ISEQ
	DB	$4C,<C0254,>C0254	; SKPFLS	C0254
; 1022:                 key = $CD ; M
	DB	$2A,$CD			; CB	205
	DB	$74,$02			; SLB	2
; 1023:             elsif key == $DE
	DB	$50,<C0253,>C0253	; SKIP	C0253
C0254:
	DB	$64,$02			; LLB	2
	DB	$2A,$DE			; CB	222
	DB	$40			; ISEQ
	DB	$4C,<C0255,>C0255	; SKPFLS	C0255
; 1024:                 key = $CE ; N
	DB	$2A,$CE			; CB	206
	DB	$74,$02			; SLB	2
; 1025:             fin
C0255:
C0253:
; 1026:         else
	DB	$50,<C0251,>C0251	; SKIP	C0251
C0250:
; 1027:            key = key ? $E0
	DB	$64,$02			; LLB	2
	DB	$2A,$E0			; CB	224
	DB	$16			; IOR
	DB	$74,$02			; SLB	2
; 1028:         fin
C0251:
; 1029:     fin
C0249:
C0243:
; 1030:     return key
	DB	$64,$02			; LLB	2
	DB	$5A			; LEAVE
; 1031: end
; 1032: ;
; 1033: ; Printer routines
; 1034: ;
; 1035: def printtxt_10(slot)
C0256:					; printtxt_10()
					; slot = 2
; 1036:     byte txtbuf[80]
					; txtbuf = 4
; 1037:     word i, scrncsw
					; i = 84
					; scrncsw = 86
; 1038: 
; 1039:     scrncsw = *(csw)
	JSR	_INTERP
	DB	$58,$58,$01		; ENTER	88,1
	DB	$2A,$36			; CB	54
	DB	$62			; LW
	DB	$76,$56			; SLW	86
; 1040:     *(csw)  = $C000 ? (slot << 8)
	DB	$2A,$36			; CB	54
	DB	$2C,$00,$C0		; CW	49152
	DB	$66,$02			; LLW	2
	DB	$2A,$08			; CB	8
	DB	$1A			; SHL
	DB	$16			; IOR
	DB	$72			; SW
; 1041:     for i = 0 to numlines - 1
	DB	$00			; ZERO
C0259:
	DB	$6E,$54			; DLW	84
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$3A,<C0258,>C0258	; SKPGT	C0258
	DB	$0C			; INCR
; 1042:         cpyln_20(strlinbuf:[i], @txtbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$54			; LLW	84
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$04			; LLA	4
	DB	$54,<C0129,>C0129	; CALL	C0129
; 1043:         prstr(@txtbuf)
	DB	$28,$04			; LLA	4
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1044:         crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1045:     next
	DB	$50,<C0259,>C0259	; SKIP	C0259
C0258:
	DB	$30			; DROP
; 1046:     *(csw) = scrncsw
	DB	$2A,$36			; CB	54
	DB	$66,$56			; LLW	86
	DB	$72			; SW
; 1047: end
	DB	$5A			; LEAVE
; 1048: def openline_11(row)
C0260:					; openline_11()
					; row = 2
; 1049:     if numlines < maxlines
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2C,$DC,$05		; CW	1500
	DB	$46			; ISLT
	DB	$4C,<C0262,>C0262	; SKPFLS	C0262
; 1050:         memcpy(@strlinbuf:[row], @strlinbuf:[row + 1], (numlines - row) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$66,$02			; LLW	2
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1051:         strlinbuf:[row] = @nullstr
	DB	$2C,$00,$10		; CW	4096
	DB	$66,$02			; LLW	2
	DB	$1E			; IDXW
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$72			; SW
; 1052:         numlines = numlines + 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0209,>D0209	; SAW	D0209
; 1053:         flags = flags ? changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1054:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1055:     fin
C0262:
C0263:
; 1056:     bell()
	DB	$54,<C0041,>C0041	; CALL	C0041
; 1057:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1058: end
; 1059: def cutline
C0264:					; cutline()
; 1060:     freestr_10(cutbuf)
	JSR	_INTERP
	DB	$6A,<D0211,>D0211	; LAW	D0211
	DB	$54,<C0069,>C0069	; CALL	C0069
; 1061:     cutbuf = strlinbuf:[cursrow]
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$7A,<D0211,>D0211	; SAW	D0211
; 1062:     memcpy(@strlinbuf:[cursrow + 1], @strlinbuf:[cursrow], (numlines - cursrow) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1063:     if numlines > 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$44			; ISGT
	DB	$4C,<C0266,>C0266	; SKPFLS	C0266
; 1064:         numlines = numlines - 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0209,>D0209	; SAW	D0209
; 1065:     fin
C0266:
C0267:
; 1066:     flags = flags ? changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1067:     if cursrow == numlines
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$40			; ISEQ
	DB	$4C,<C0268,>C0268	; SKPFLS	C0268
; 1068:         cursup()
	DB	$54,<C0192,>C0192	; CALL	C0192
; 1069:     fin
C0268:
C0269:
; 1070:     redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1071: end
	DB	$5C			; RET
; 1072: def pasteline
C0270:					; pasteline()
; 1073:     if cutbuf and numlines < maxlines
	JSR	_INTERP
	DB	$6A,<D0211,>D0211	; LAW	D0211
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2C,$DC,$05		; CW	1500
	DB	$46			; ISLT
	DB	$24			; LAND
	DB	$4C,<C0272,>C0272	; SKPFLS	C0272
; 1074:         memcpy(@strlinbuf:[cursrow], @strlinbuf:[cursrow + 1], (numlines - cursrow) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1075:         strlinbuf:[cursrow] = newstr_11(cutbuf)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$6A,<D0211,>D0211	; LAW	D0211
	DB	$54,<C0073,>C0073	; CALL	C0073
	DB	$72			; SW
; 1076:         numlines = numlines + 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$7A,<D0209,>D0209	; SAW	D0209
; 1077:         flags = flags ? changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1078:         redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1079:     else
	DB	$50,<C0273,>C0273	; SKIP	C0273
C0272:
; 1080:         bell()
	DB	$54,<C0041,>C0041	; CALL	C0041
; 1081:     fin
C0273:
; 1082: end
	DB	$5C			; RET
; 1083: def joinline
C0274:					; joinline()
; 1084:     byte joinstr[80], joinlen
					; joinstr = 2
					; joinlen = 82
; 1085: 
; 1086:     if cursrow < numlines - 1
	JSR	_INTERP
	DB	$58,$53,$00		; ENTER	83,0
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$46			; ISLT
	DB	$4C,<C0276,>C0276	; SKPFLS	C0276
; 1087:         strcpy_20(strlinbuf:[cursrow], @joinstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$02			; LLA	2
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1088:         joinlen = joinstr + ^(strlinbuf:[cursrow + 1])
	DB	$64,$02			; LLB	2
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$60			; LB
	DB	$02			; ADD
	DB	$74,$52			; SLB	82
; 1089:         if joinlen < 80
	DB	$64,$52			; LLB	82
	DB	$2A,$50			; CB	80
	DB	$46			; ISLT
	DB	$4C,<C0278,>C0278	; SKPFLS	C0278
; 1090:             memcpy(strlinbuf:[cursrow + 1] + 1, @joinstr + joinstr + 1, ^(strlinbuf:[cursrow + 1]))
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
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
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$60			; LB
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1091:             joinstr = joinlen
	DB	$64,$52			; LLB	82
	DB	$74,$02			; SLB	2
; 1092:             freestr_10(strlinbuf:[cursrow])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0069,>C0069	; CALL	C0069
; 1093:             strlinbuf:[cursrow] = newstr_11(@joinstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$28,$02			; LLA	2
	DB	$54,<C0073,>C0073	; CALL	C0073
	DB	$72			; SW
; 1094:             freestr_10(strlinbuf:[cursrow + 1])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0069,>C0069	; CALL	C0069
; 1095:             numlines = numlines - 1
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$7A,<D0209,>D0209	; SAW	D0209
; 1096:             memcpy(@strlinbuf:[cursrow + 2], @strlinbuf:[cursrow + 1], (numlines - cursrow) * 2)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$6A,<D0209,>D0209	; LAW	D0209
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$04			; SUB
	DB	$2A,$02			; CB	2
	DB	$06			; MUL
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1097:             flags = flags ? changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1098:             redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1099:         else
	DB	$50,<C0279,>C0279	; SKIP	C0279
C0278:
; 1100:             bell()
	DB	$54,<C0041,>C0041	; CALL	C0041
; 1101:         fin
C0279:
; 1102:     fin
C0276:
C0277:
; 1103: end
	DB	$5A			; LEAVE
; 1104: def splitline
C0280:					; splitline()
; 1105:     byte splitstr[80], splitlen
					; splitstr = 2
					; splitlen = 82
; 1106: 
; 1107:     if openline_11(cursrow + 1)
	JSR	_INTERP
	DB	$58,$53,$00		; ENTER	83,0
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0260,>C0260	; CALL	C0260
	DB	$4C,<C0282,>C0282	; SKPFLS	C0282
; 1108:         if curscol
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$4C,<C0284,>C0284	; SKPFLS	C0284
; 1109:             splitlen = ^(strlinbuf:[cursrow])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$60			; LB
	DB	$74,$52			; SLB	82
; 1110:             if curscol < splitlen - 1
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$64,$52			; LLB	82
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$46			; ISLT
	DB	$4C,<C0286,>C0286	; SKPFLS	C0286
; 1111:                 memcpy(strlinbuf:[cursrow] + curscol + 1, @splitstr + 1, splitlen - curscol)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$02			; ADD
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$28,$02			; LLA	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$64,$52			; LLB	82
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$04			; SUB
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1112:                 splitstr = splitlen - curscol
	DB	$64,$52			; LLB	82
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$04			; SUB
	DB	$74,$02			; SLB	2
; 1113:                 strlinbuf:[cursrow + 1] = newstr_11(@splitstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$28,$02			; LLA	2
	DB	$54,<C0073,>C0073	; CALL	C0073
	DB	$72			; SW
; 1114:                 memcpy(strlinbuf:[cursrow] + 1, @splitstr + 1, curscol)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$28,$02			; LLA	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1115:                 splitstr = curscol
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$74,$02			; SLB	2
; 1116:                 freestr_10(strlinbuf:[cursrow])
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$54,<C0069,>C0069	; CALL	C0069
; 1117:                 strlinbuf:[cursrow] = newstr_11(@splitstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$28,$02			; LLA	2
	DB	$54,<C0073,>C0073	; CALL	C0073
	DB	$72			; SW
; 1118:             fin
C0286:
C0287:
; 1119:         else
	DB	$50,<C0285,>C0285	; SKIP	C0285
C0284:
; 1120:             strlinbuf:[cursrow + 1] = strlinbuf:[cursrow]
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$1E			; IDXW
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$72			; SW
; 1121:             strlinbuf:[cursrow]     = @nullstr
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$72			; SW
; 1122:         fin
C0285:
; 1123:         curscol  = 0
	DB	$00			; ZERO
	DB	$78,<D0200,>D0200	; SAB	D0200
; 1124:         cursx    = 0
	DB	$00			; ZERO
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1125:         scrnleft = 0
	DB	$00			; ZERO
	DB	$78,<D0199,>D0199	; SAB	D0199
; 1126:         redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1127:         cursdown()
	DB	$54,<C0202,>C0202	; CALL	C0202
; 1128:     fin
C0282:
C0283:
; 1129: end
	DB	$5A			; LEAVE
; 1130: def editkey_11(key)
C0288:					; editkey_11()
					; key = 2
; 1131:     if key >= keyspace
	JSR	_INTERP
	DB	$58,$04,$01		; ENTER	4,1
	DB	$66,$02			; LLW	2
	DB	$2A,$A0			; CB	160
	DB	$48			; ISGE
	DB	$4C,<C0290,>C0290	; SKPFLS	C0290
; 1132:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1133:     elsif key == keydelete
	DB	$50,<C0291,>C0291	; SKIP	C0291
C0290:
	DB	$66,$02			; LLW	2
	DB	$2A,$FF			; CB	255
	DB	$40			; ISEQ
	DB	$4C,<C0292,>C0292	; SKPFLS	C0292
; 1134:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1135:     elsif key == keyctrld
	DB	$50,<C0291,>C0291	; SKIP	C0291
C0292:
	DB	$66,$02			; LLW	2
	DB	$2A,$84			; CB	132
	DB	$40			; ISEQ
	DB	$4C,<C0293,>C0293	; SKPFLS	C0293
; 1136:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1137:     elsif key == keyctrlr
	DB	$50,<C0291,>C0291	; SKIP	C0291
C0293:
	DB	$66,$02			; LLW	2
	DB	$2A,$92			; CB	146
	DB	$40			; ISEQ
	DB	$4C,<C0294,>C0294	; SKPFLS	C0294
; 1138:         return 1
	DB	$2A,$01			; CB	1
	DB	$5A			; LEAVE
; 1139:     fin
C0294:
C0291:
; 1140:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1141: end
; 1142: def editline_11(key)
C0295:					; editline_11()
					; key = 2
; 1143:     byte editstr[80]
					; editstr = 4
; 1144:     word undoline
					; undoline = 84
; 1145: 
; 1146:     if (editkey_11(key))
	JSR	_INTERP
	DB	$58,$56,$01		; ENTER	86,1
	DB	$66,$02			; LLW	2
	DB	$54,<C0288,>C0288	; CALL	C0288
	DB	$4C,<C0297,>C0297	; SKPFLS	C0297
; 1147:         flags = flags ? changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1148:         memset($A0A0, @editstr, 80)
	DB	$2C,$A0,$A0		; CW	41120
	DB	$28,$04			; LLA	4
	DB	$2A,$50			; CB	80
	DB	$54,<C0007,>C0007	; CALL	C0007
; 1149:         strcpy_20(strlinbuf:[cursrow], @editstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$28,$04			; LLA	4
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1150:         undoline = strlinbuf:[cursrow]
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$62			; LW
	DB	$76,$54			; SLW	84
; 1151:         strlinbuf:[cursrow] = @editstr
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$28,$04			; LLA	4
	DB	$72			; SW
; 1152:         repeat
C0300:
; 1153:             if key >= keyspace
	DB	$66,$02			; LLW	2
	DB	$2A,$A0			; CB	160
	DB	$48			; ISGE
	DB	$4C,<C0301,>C0301	; SKPFLS	C0301
; 1154:                 if key == keydelete
	DB	$66,$02			; LLW	2
	DB	$2A,$FF			; CB	255
	DB	$40			; ISEQ
	DB	$4C,<C0303,>C0303	; SKPFLS	C0303
; 1155:                     if curscol > 0
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0305,>C0305	; SKPFLS	C0305
; 1156:                         if curscol <= editstr
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$64,$04			; LLB	4
	DB	$4A			; ISLE
	DB	$4C,<C0307,>C0307	; SKPFLS	C0307
; 1157:                             memcpy(@editstr[curscol + 1], @editstr[curscol], editstr - curscol)
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$02			; IDXB
	DB	$64,$04			; LLB	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$04			; SUB
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1158:                             editstr = editstr - 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$04			; SLB	4
; 1159:                         fin
C0307:
C0308:
; 1160:                         curscol = curscol - 1
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0200,>D0200	; SAB	D0200
; 1161:                         cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
; 1162:                         if cursx > 0
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$00			; ZERO
	DB	$44			; ISGT
	DB	$4C,<C0309,>C0309	; SKPFLS	C0309
; 1163:                             cursx = cursx - 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1164:                             drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$28,$04			; LLA	4
	DB	$54,<C0155,>C0155	; CALL	C0155
; 1165:                         else
	DB	$50,<C0310,>C0310	; SKIP	C0310
C0309:
; 1166:                             scrnleft = scrnleft - 1
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0199,>D0199	; SAB	D0199
; 1167:                             drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
; 1168:                         fin
C0310:
; 1169:                         curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1170:                     fin
C0305:
C0306:
; 1171:                 elsif curscol < maxlnlen
	DB	$50,<C0304,>C0304	; SKIP	C0304
C0303:
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$4F			; CB	79
	DB	$46			; ISLT
	DB	$4C,<C0311,>C0311	; SKPFLS	C0311
; 1172:                     curscol = curscol + 1
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0200,>D0200	; SAB	D0200
; 1173:                     cursx   = cursx   + 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1174:                     if flags & insmode
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0312,>C0312	; SKPFLS	C0312
; 1175:                         if editstr < maxlnlen or editstr.maxlnlen == $A0
	DB	$64,$04			; LLB	4
	DB	$2A,$4F			; CB	79
	DB	$46			; ISLT
	DB	$28,$53			; LLA	83
	DB	$60			; LB
	DB	$2A,$A0			; CB	160
	DB	$40			; ISEQ
	DB	$22			; LOR
	DB	$4C,<C0314,>C0314	; SKPFLS	C0314
; 1176:                             editstr = editstr + 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$04			; SLB	4
; 1177:                             if curscol >= editstr
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$64,$04			; LLB	4
	DB	$48			; ISGE
	DB	$4C,<C0316,>C0316	; SKPFLS	C0316
; 1178:                                 editstr = curscol
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$74,$04			; SLB	4
; 1179:                             else
	DB	$50,<C0317,>C0317	; SKIP	C0317
C0316:
; 1180:                                 memcpy(@editstr[curscol], @editstr[curscol + 1], editstr - curscol)
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$02			; IDXB
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$64,$04			; LLB	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$04			; SUB
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1181:                             fin
C0317:
; 1182:                         else
	DB	$50,<C0315,>C0315	; SKIP	C0315
C0314:
; 1183:                             curscol = curscol - 1
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0200,>D0200	; SAB	D0200
; 1184:                             cursx   = cursx   - 1
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1185:                             key     = editstr[curscol]
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$76,$02			; SLW	2
; 1186:                             bell()
	DB	$54,<C0041,>C0041	; CALL	C0041
; 1187:                         fin
C0315:
; 1188:                     else
	DB	$50,<C0313,>C0313	; SKIP	C0313
C0312:
; 1189:                         if curscol > editstr
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$64,$04			; LLB	4
	DB	$44			; ISGT
	DB	$4C,<C0318,>C0318	; SKPFLS	C0318
; 1190:                             editstr = curscol
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$74,$04			; SLB	4
; 1191:                         fin
C0318:
C0319:
; 1192:                     fin
C0313:
; 1193:                     editstr[curscol] = caseconv_11(key)
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$02			; IDXB
	DB	$66,$02			; LLW	2
	DB	$54,<C0083,>C0083	; CALL	C0083
	DB	$70			; SB
; 1194:                     cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
; 1195:                     if cursx <= 39
	DB	$68,<D0197,>D0197	; LAB	D0197
	DB	$2A,$27			; CB	39
	DB	$4A			; ISLE
	DB	$4C,<C0320,>C0320	; SKPFLS	C0320
; 1196:                         drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$28,$04			; LLA	4
	DB	$54,<C0155,>C0155	; CALL	C0155
; 1197:                     else
	DB	$50,<C0321,>C0321	; SKIP	C0321
C0320:
; 1198:                         scrnleft = scrnleft + 1
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$78,<D0199,>D0199	; SAB	D0199
; 1199:                         cursx    = 39
	DB	$2A,$27			; CB	39
	DB	$78,<D0197,>D0197	; SAB	D0197
; 1200:                         drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
; 1201:                     fin
C0321:
; 1202:                     curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1203:                 else
	DB	$50,<C0304,>C0304	; SKIP	C0304
C0311:
; 1204:                     bell()
	DB	$54,<C0041,>C0041	; CALL	C0041
; 1205:                 fin
C0304:
; 1206:             elsif key == keyctrld
	DB	$50,<C0302,>C0302	; SKIP	C0302
C0301:
	DB	$66,$02			; LLW	2
	DB	$2A,$84			; CB	132
	DB	$40			; ISEQ
	DB	$4C,<C0322,>C0322	; SKPFLS	C0322
; 1207:                 if curscol < editstr
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$64,$04			; LLB	4
	DB	$46			; ISLT
	DB	$4C,<C0323,>C0323	; SKPFLS	C0323
; 1208:                     memcpy(@editstr[curscol + 2], @editstr[curscol + 1], editstr - curscol)
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$28,$04			; LLA	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$02			; IDXB
	DB	$64,$04			; LLB	4
	DB	$68,<D0200,>D0200	; LAB	D0200
	DB	$04			; SUB
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1209:                     editstr = editstr - 1
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$74,$04			; SLB	4
; 1210:                     cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
; 1211:                     drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$28,$04			; LLA	4
	DB	$54,<C0155,>C0155	; CALL	C0155
; 1212:                     curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1213:                 fin
C0323:
C0324:
; 1214:             elsif key == keyctrlr
	DB	$50,<C0302,>C0302	; SKIP	C0302
C0322:
	DB	$66,$02			; LLW	2
	DB	$2A,$92			; CB	146
	DB	$40			; ISEQ
	DB	$4C,<C0325,>C0325	; SKPFLS	C0325
; 1215:                 strcpy_20(undoline, @editstr)
	DB	$66,$54			; LLW	84
	DB	$28,$04			; LLA	4
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1216:                 cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
; 1217:                 drawrow_30(cursy, scrnleft, @editstr)
	DB	$68,<D0198,>D0198	; LAB	D0198
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$28,$04			; LLA	4
	DB	$54,<C0155,>C0155	; CALL	C0155
; 1218:                 curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1219:             fin
C0325:
C0302:
; 1220:             key = keyin_01()
	DB	$26,<D0213,>D0213	; LA	D0213
	DB	$62			; LW
	DB	$34			; PUSH
	DB	$36			; PULL
	DB	$56			; ICAL
	DB	$76,$02			; SLW	2
; 1221:         until !editkey_11(key)
	DB	$66,$02			; LLW	2
	DB	$54,<C0288,>C0288	; CALL	C0288
	DB	$20			; NOT
	DB	$4C,<C0300,>C0300	; SKPFLS	C0300
C0299:
; 1222:         if editstr
	DB	$64,$04			; LLB	4
	DB	$4C,<C0326,>C0326	; SKPFLS	C0326
; 1223:             strlinbuf:[cursrow] = newstr_11(@editstr)
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$28,$04			; LLA	4
	DB	$54,<C0073,>C0073	; CALL	C0073
	DB	$72			; SW
; 1224:         else
	DB	$50,<C0327,>C0327	; SKIP	C0327
C0326:
; 1225:             strlinbuf:[cursrow] = @nullstr
	DB	$2C,$00,$10		; CW	4096
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$1E			; IDXW
	DB	$26,<D0048,>D0048	; LA	D0048
	DB	$72			; SW
; 1226:         fin
C0327:
; 1227:         freestr_10(undoline)
	DB	$66,$54			; LLW	84
	DB	$54,<C0069,>C0069	; CALL	C0069
; 1228:     fin
C0297:
C0298:
; 1229:     return key
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
; 1230: end
; 1231: def editmode
C0328:					; editmode()
; 1232:     repeat
	JSR	_INTERP
C0331:
; 1233:         when editline_11(keyin_01())
	DB	$26,<D0213,>D0213	; LA	D0213
	DB	$62			; LW
	DB	$34			; PUSH
	DB	$36			; PULL
	DB	$56			; ICAL
	DB	$54,<C0295,>C0295	; CALL	C0295
; 1234:             is keyarrowup
	DB	$2A,$8B			; CB	139
	DB	$3E,<C0333,>C0333	; SKPNE	C0333
; 1235:                 cursup()
	DB	$54,<C0192,>C0192	; CALL	C0192
; 1236:             is keyarrowdown
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0333:
	DB	$2A,$8A			; CB	138
	DB	$3E,<C0334,>C0334	; SKPNE	C0334
; 1237:                 cursdown()
	DB	$54,<C0202,>C0202	; CALL	C0202
; 1238:             is keyarrowleft
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0334:
	DB	$2A,$88			; CB	136
	DB	$3E,<C0335,>C0335	; SKPNE	C0335
; 1239:                 cursleft()
	DB	$54,<C0212,>C0212	; CALL	C0212
; 1240:             is keyarrowright
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0335:
	DB	$2A,$95			; CB	149
	DB	$3E,<C0336,>C0336	; SKPNE	C0336
; 1241:                 cursright()
	DB	$54,<C0222,>C0222	; CALL	C0222
; 1242:             is keyctrlw
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0336:
	DB	$2A,$97			; CB	151
	DB	$3E,<C0337,>C0337	; SKPNE	C0337
; 1243:                 pgup()
	DB	$54,<C0198,>C0198	; CALL	C0198
; 1244:             is keyctrlz
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0337:
	DB	$2A,$9A			; CB	154
	DB	$3E,<C0338,>C0338	; SKPNE	C0338
; 1245:                 pgdown()
	DB	$54,<C0208,>C0208	; CALL	C0208
; 1246:             is keyctrla
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0338:
	DB	$2A,$81			; CB	129
	DB	$3E,<C0339,>C0339	; SKPNE	C0339
; 1247:                 pgleft()
	DB	$54,<C0218,>C0218	; CALL	C0218
; 1248:             is keyctrls
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0339:
	DB	$2A,$93			; CB	147
	DB	$3E,<C0340,>C0340	; SKPNE	C0340
; 1249:                 pgright()
	DB	$54,<C0228,>C0228	; CALL	C0228
; 1250:             is keyctrlq
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0340:
	DB	$2A,$91			; CB	145
	DB	$3E,<C0341,>C0341	; SKPNE	C0341
; 1251:                 curshome()
	DB	$54,<C0186,>C0186	; CALL	C0186
; 1252:             is keyctrle
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0341:
	DB	$2A,$85			; CB	133
	DB	$3E,<C0342,>C0342	; SKPNE	C0342
; 1253:                 cursend()
	DB	$54,<C0188,>C0188	; CALL	C0188
; 1254:             is keyctrlx
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0342:
	DB	$2A,$98			; CB	152
	DB	$3E,<C0343,>C0343	; SKPNE	C0343
; 1255:                 cutline()
	DB	$54,<C0264,>C0264	; CALL	C0264
; 1256:             is keyctrlv
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0343:
	DB	$2A,$96			; CB	150
	DB	$3E,<C0344,>C0344	; SKPNE	C0344
; 1257:                 pasteline()
	DB	$54,<C0270,>C0270	; CALL	C0270
; 1258:             is keyctrlo
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0344:
	DB	$2A,$8F			; CB	143
	DB	$3E,<C0345,>C0345	; SKPNE	C0345
; 1259:                 drop openline_11(cursrow)
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$54,<C0260,>C0260	; CALL	C0260
	DB	$30			; DROP
; 1260:                 redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1261:             is keyenter
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0345:
	DB	$2A,$8D			; CB	141
	DB	$3E,<C0346,>C0346	; SKPNE	C0346
; 1262:                 if flags & insmode
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0347,>C0347	; SKPFLS	C0347
; 1263:                     splitline()
	DB	$54,<C0280,>C0280	; CALL	C0280
; 1264:                 else
	DB	$50,<C0348,>C0348	; SKIP	C0348
C0347:
; 1265:                     drop openline_11(cursrow + 1)
	DB	$6A,<D0203,>D0203	; LAW	D0203
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0260,>C0260	; CALL	C0260
	DB	$30			; DROP
; 1266:                     cursdown()
	DB	$54,<C0202,>C0202	; CALL	C0202
; 1267:                     redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1268:                 fin
C0348:
; 1269:             is keyctrlt
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0346:
	DB	$2A,$94			; CB	148
	DB	$3E,<C0349,>C0349	; SKPNE	C0349
; 1270:                 joinline()
	DB	$54,<C0274,>C0274	; CALL	C0274
; 1271:             is keyctrli
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0349:
	DB	$2A,$89			; CB	137
	DB	$3E,<C0350,>C0350	; SKPNE	C0350
; 1272:                 if flags & insmode
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$02			; CB	2
	DB	$14			; BAND
	DB	$4C,<C0351,>C0351	; SKPFLS	C0351
; 1273:                     flags = flags & #insmode
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2C,$FD,$FF		; CW	-3
	DB	$14			; BAND
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1274:                     curschr = ' '
	DB	$2A,$20			; CB	32
	DB	$78,<D0202,>D0202	; SAB	D0202
; 1275:                 else
	DB	$50,<C0352,>C0352	; SKIP	C0352
C0351:
; 1276:                     flags = flags ? insmode
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$02			; CB	2
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1277:                     curschr = '+'
	DB	$2A,$2B			; CB	43
	DB	$78,<D0202,>D0202	; SAB	D0202
; 1278:                 fin
C0352:
; 1279:             is keyctrlc
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0350:
	DB	$2A,$83			; CB	131
	DB	$3E,<C0353,>C0353	; SKPNE	C0353
; 1280:                 if flags & uppercase
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$08			; CB	8
	DB	$14			; BAND
	DB	$4C,<C0354,>C0354	; SKPFLS	C0354
; 1281:                     txtlower()
	DB	$54,<C0105,>C0105	; CALL	C0105
; 1282:                 else
	DB	$50,<C0355,>C0355	; SKIP	C0355
C0354:
; 1283:                     txtupper()
	DB	$54,<C0101,>C0101	; CALL	C0101
; 1284:                 fin
C0355:
; 1285:                 redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1286:             is keyescape
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0353:
	DB	$2A,$9B			; CB	155
	DB	$3E,<C0356,>C0356	; SKPNE	C0356
; 1287:                 cursoff()
	DB	$54,<C0169,>C0169	; CALL	C0169
; 1288:                 cmdmode()
	DB	$54,<C0000,>C0000	; CALL	C0000
; 1289:                 redraw()
	DB	$54,<C0184,>C0184	; CALL	C0184
; 1290:         wend
	DB	$50,<C0332,>C0332	; SKIP	C0332
C0356:
C0332:
	DB	$30			; DROP
; 1291:     until 0
	DB	$00			; ZERO
	DB	$4C,<C0331,>C0331	; SKPFLS	C0331
C0330:
; 1292: end
	DB	$5C			; RET
; 1293: ;
; 1294: ; Command mode
; 1295: ;
; 1296: def prfiles_11(optpath)
C0358:					; prfiles_11()
					; optpath = 2
; 1297:     byte path[64]
					; path = 4
; 1298:     byte refnum
					; refnum = 68
; 1299:     byte firstblk
					; firstblk = 69
; 1300:     byte entrylen, entriesblk
					; entrylen = 70
					; entriesblk = 71
; 1301:     byte i, type, len
					; i = 72
					; type = 73
					; len = 74
; 1302:     word entry, filecnt
					; entry = 75
					; filecnt = 77
; 1303: 
; 1304:     if ^optpath
	JSR	_INTERP
	DB	$58,$4F,$01		; ENTER	79,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0360,>C0360	; SKPFLS	C0360
; 1305:         strcpy_20(optpath, @path)
	DB	$66,$02			; LLW	2
	DB	$28,$04			; LLA	4
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1306:     else
	DB	$50,<C0361,>C0361	; SKIP	C0361
C0360:
; 1307:         drop getpfx_11(@path)
	DB	$28,$04			; LLA	4
	DB	$54,<C0021,>C0021	; CALL	C0021
	DB	$30			; DROP
; 1308:         prstr(@path)
	DB	$28,$04			; LLA	4
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1309:         crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1310:     fin
C0361:
; 1311:     refnum = open_21(@path, iobuffer);
	DB	$28,$04			; LLA	4
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0025,>C0025	; CALL	C0025
	DB	$74,$44			; SLB	68
; 1312:     if perr
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$4C,<C0362,>C0362	; SKPFLS	C0362
; 1313:         return perr
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$5A			; LEAVE
; 1314:     fin
C0362:
C0363:
; 1315:     firstblk = 1
	DB	$2A,$01			; CB	1
	DB	$74,$45			; SLB	69
; 1316:     repeat
C0365:
; 1317:         if read_31(refnum, databuff, 512) == 512
	DB	$64,$44			; LLB	68
	DB	$2C,$00,$0C		; CW	3072
	DB	$2C,$00,$02		; CW	512
	DB	$54,<C0029,>C0029	; CALL	C0029
	DB	$2C,$00,$02		; CW	512
	DB	$40			; ISEQ
	DB	$4C,<C0366,>C0366	; SKPFLS	C0366
; 1318:             entry = databuff + 4
	DB	$2C,$00,$0C		; CW	3072
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
; 1319:             if firstblk
	DB	$64,$45			; LLB	69
	DB	$4C,<C0368,>C0368	; SKPFLS	C0368
; 1320:                 entrylen   = databuff.$23
	DB	$2C,$23,$0C		; CW	3107
	DB	$60			; LB
	DB	$74,$46			; SLB	70
; 1321:                 entriesblk = databuff.$24
	DB	$2C,$24,$0C		; CW	3108
	DB	$60			; LB
	DB	$74,$47			; SLB	71
; 1322:                 filecnt    = databuff:$25
	DB	$2C,$25,$0C		; CW	3109
	DB	$62			; LW
	DB	$76,$4D			; SLW	77
; 1323:                 entry      = entry + entrylen
	DB	$66,$4B			; LLW	75
	DB	$64,$46			; LLB	70
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
; 1324:             fin
C0368:
C0369:
; 1325:             for i = firstblk to entriesblk
	DB	$64,$45			; LLB	69
C0371:
	DB	$6C,$48			; DLB	72
	DB	$64,$47			; LLB	71
	DB	$3A,<C0370,>C0370	; SKPGT	C0370
	DB	$0C			; INCR
; 1326:                 type = ^entry
	DB	$66,$4B			; LLW	75
	DB	$60			; LB
	DB	$74,$49			; SLB	73
; 1327:                 if type <> 0
	DB	$64,$49			; LLB	73
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0372,>C0372	; SKPFLS	C0372
; 1328:                     len = type & $0F
	DB	$64,$49			; LLB	73
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$74,$4A			; SLB	74
; 1329:                     ^entry = len
	DB	$66,$4B			; LLW	75
	DB	$64,$4A			; LLB	74
	DB	$70			; SB
; 1330:                     prstr(entry)
	DB	$66,$4B			; LLW	75
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1331:                     if type & $F0 == $D0 ; Is it a directory?
	DB	$64,$49			; LLB	73
	DB	$2A,$F0			; CB	240
	DB	$14			; BAND
	DB	$2A,$D0			; CB	208
	DB	$40			; ISEQ
	DB	$4C,<C0374,>C0374	; SKPFLS	C0374
; 1332:                         cout('/')
	DB	$2A,$2F			; CB	47
	DB	$54,<C0011,>C0011	; CALL	C0011
; 1333:                         len = len + 1
	DB	$64,$4A			; LLB	74
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$4A			; SLB	74
; 1334:                     fin
C0374:
C0375:
; 1335:                     for len = 20 - len downto 1
	DB	$2A,$14			; CB	20
	DB	$64,$4A			; LLB	74
	DB	$04			; SUB
C0377:
	DB	$6C,$4A			; DLB	74
	DB	$2A,$01			; CB	1
	DB	$38,<C0376,>C0376	; SKPLT	C0376
	DB	$0E			; DECR
; 1336:                         cout(' ')
	DB	$2A,$20			; CB	32
	DB	$54,<C0011,>C0011	; CALL	C0011
; 1337:                     next
	DB	$50,<C0377,>C0377	; SKIP	C0377
C0376:
	DB	$30			; DROP
; 1338:                     filecnt = filecnt - 1
	DB	$66,$4D			; LLW	77
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$4D			; SLW	77
; 1339:                 fin
C0372:
C0373:
; 1340:                 entry = entry + entrylen
	DB	$66,$4B			; LLW	75
	DB	$64,$46			; LLB	70
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
; 1341:             next
	DB	$50,<C0371,>C0371	; SKIP	C0371
C0370:
	DB	$30			; DROP
; 1342:             firstblk = 0
	DB	$00			; ZERO
	DB	$74,$45			; SLB	69
; 1343:         else
	DB	$50,<C0367,>C0367	; SKIP	C0367
C0366:
; 1344:             filecnt = 0
	DB	$00			; ZERO
	DB	$76,$4D			; SLW	77
; 1345:         fin
C0367:
; 1346:     until filecnt == 0
	DB	$66,$4D			; LLW	77
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0365,>C0365	; SKPFLS	C0365
C0364:
; 1347:     drop close_11(refnum)
	DB	$64,$44			; LLB	68
	DB	$54,<C0027,>C0027	; CALL	C0027
	DB	$30			; DROP
; 1348:     crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1349:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
; 1350: end
; 1351: def striplead_20(strptr, chr)
C0378:					; striplead_20()
					; strptr = 2
					; chr = 4
; 1352:     while ^strptr and ^(strptr + 1) == chr
	JSR	_INTERP
	DB	$58,$06,$02		; ENTER	6,2
C0380:
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$66,$04			; LLW	4
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$4C,<C0381,>C0381	; SKPFLS	C0381
; 1353:         memcpy(strptr + 2, strptr + 1, ^strptr)
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1354:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
; 1355:     loop
	DB	$50,<C0380,>C0380	; SKIP	C0380
C0381:
; 1356: end
	DB	$5A			; LEAVE
; 1357: def parsecmd_11(strptr)
C0382:					; parsecmd_11()
					; strptr = 2
; 1358:     byte cmd
					; cmd = 4
; 1359: 
; 1360:     cmd = 0
	JSR	_INTERP
	DB	$58,$05,$01		; ENTER	5,1
	DB	$00			; ZERO
	DB	$74,$04			; SLB	4
; 1361:     striplead_20(strptr, ' ')
	DB	$66,$02			; LLW	2
	DB	$2A,$20			; CB	32
	DB	$54,<C0378,>C0378	; CALL	C0378
; 1362:     if ^strptr
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0384,>C0384	; SKPFLS	C0384
; 1363:         cmd = ^(strptr + 1)
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$04			; SLB	4
; 1364:         memcpy(strptr + 2, strptr + 1, ^strptr)
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$54,<C0009,>C0009	; CALL	C0009
; 1365:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
; 1366:     fin
C0384:
C0385:
; 1367:     if ^strptr
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0386,>C0386	; SKPFLS	C0386
; 1368:         striplead_20(strptr, ' ')
	DB	$66,$02			; LLW	2
	DB	$2A,$20			; CB	32
	DB	$54,<C0378,>C0378	; CALL	C0378
; 1369:     fin
C0386:
C0387:
; 1370:     return cmd
	DB	$64,$04			; LLB	4
	DB	$5A			; LEAVE
; 1371: end
; 1372: def chkchng_01
C0388:					; chkchng_01()
; 1373:     if flags & changed
	JSR	_INTERP
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0390,>C0390	; SKPFLS	C0390
; 1374:         prstr(@losechng)
	DB	$26,<D0107,>D0107	; LA	D0107
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1375:         if toupper_11(keyin_01()) == 'N'
	DB	$26,<D0213,>D0213	; LA	D0213
	DB	$62			; LW
	DB	$34			; PUSH
	DB	$36			; PULL
	DB	$56			; ICAL
	DB	$54,<C0123,>C0123	; CALL	C0123
	DB	$2A,$4E			; CB	78
	DB	$40			; ISEQ
	DB	$4C,<C0392,>C0392	; SKPFLS	C0392
; 1376:             crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1377:             return 0
	DB	$00			; ZERO
	DB	$5C			; RET
; 1378:         fin
C0392:
C0393:
; 1379:         crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1380:     fin
C0390:
C0391:
; 1381:     return 1
	DB	$2A,$01			; CB	1
	DB	$5C			; RET
; 1382: end
; 1383: def quit
C0394:					; quit()
; 1384:     if chkchng_01()
	JSR	_INTERP
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$4C,<C0396,>C0396	; SKPFLS	C0396
; 1385:         exit
	DB	$54,<C0019,>C0019	; CALL	C0019
; 1386:     fin
C0396:
C0397:
; 1387: end
	DB	$5C			; RET
; 1388: def cmdmode
C0000:					; cmdmode()
; 1389:     byte slot
					; slot = 2
; 1390:     word cmdptr
					; cmdptr = 3
; 1391: 
; 1392:     clrscrn();
	JSR	_INTERP
	DB	$58,$05,$00		; ENTER	5,0
	DB	$54,<C0153,>C0153	; CALL	C0153
; 1393:     prstr(@version)
	DB	$26,<D0049,>D0049	; LA	D0049
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1394:     crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1395:     while 1
C0399:
	DB	$2A,$01			; CB	1
	DB	$4C,<C0400,>C0400	; SKPFLS	C0400
; 1396:         prstr(@txtfile)
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1397:         cmdptr = rdstr($BA)
	DB	$2A,$BA			; CB	186
	DB	$54,<C0017,>C0017	; CALL	C0017
	DB	$76,$03			; SLW	3
; 1398:         when toupper_11(parsecmd_11(cmdptr))
	DB	$66,$03			; LLW	3
	DB	$54,<C0382,>C0382	; CALL	C0382
	DB	$54,<C0123,>C0123	; CALL	C0123
; 1399:             is 'A'
	DB	$2A,$41			; CB	65
	DB	$3E,<C0402,>C0402	; SKPNE	C0402
; 1400:                 readtxt_10(cmdptr)
	DB	$66,$03			; LLW	3
	DB	$54,<C0131,>C0131	; CALL	C0131
; 1401:                 flags = flags ? changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1402:             is 'R'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0402:
	DB	$2A,$52			; CB	82
	DB	$3E,<C0403,>C0403	; SKPNE	C0403
; 1403:                 if chkchng_01()
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$4C,<C0404,>C0404	; SKPFLS	C0404
; 1404:                     inittxtbuf()
	DB	$54,<C0081,>C0081	; CALL	C0081
; 1405:                     strcpy_20(cmdptr, @txtfile)
	DB	$66,$03			; LLW	3
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1406:                     readtxt_10(@txtfile)
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0131,>C0131	; CALL	C0131
; 1407:                     flags = flags & #changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2C,$FE,$FF		; CW	-2
	DB	$14			; BAND
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1408:                 fin
C0404:
C0405:
; 1409:             is 'W'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0403:
	DB	$2A,$57			; CB	87
	DB	$3E,<C0406,>C0406	; SKPNE	C0406
; 1410:                 if ^cmdptr
	DB	$66,$03			; LLW	3
	DB	$60			; LB
	DB	$4C,<C0407,>C0407	; SKPFLS	C0407
; 1411:                     strcpy_20(cmdptr, @txtfile)
	DB	$66,$03			; LLW	3
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1412:                 fin
C0407:
C0408:
; 1413:                 writetxt_10(@txtfile)
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0145,>C0145	; CALL	C0145
; 1414:                 if flags & changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$01			; CB	1
	DB	$14			; BAND
	DB	$4C,<C0409,>C0409	; SKPFLS	C0409
; 1415:                 fin
C0409:
C0410:
; 1416:                 flags = flags & #changed
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2C,$FE,$FF		; CW	-2
	DB	$14			; BAND
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1417:             is 'Q'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0406:
	DB	$2A,$51			; CB	81
	DB	$3E,<C0411,>C0411	; SKPNE	C0411
; 1418:                 quit()
	DB	$54,<C0394,>C0394	; CALL	C0394
; 1419:             is 'C'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0411:
	DB	$2A,$43			; CB	67
	DB	$3E,<C0412,>C0412	; SKPNE	C0412
; 1420:                 drop prfiles_11(cmdptr)
	DB	$66,$03			; LLW	3
	DB	$54,<C0358,>C0358	; CALL	C0358
	DB	$30			; DROP
; 1421:             is 'P'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0412:
	DB	$2A,$50			; CB	80
	DB	$3E,<C0413,>C0413	; SKPNE	C0413
; 1422:                 drop setpfx_11(cmdptr)
	DB	$66,$03			; LLW	3
	DB	$54,<C0023,>C0023	; CALL	C0023
	DB	$30			; DROP
; 1423:             is 'H'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0413:
	DB	$2A,$48			; CB	72
	DB	$3E,<C0414,>C0414	; SKPNE	C0414
; 1424:                 if ^cmdptr
	DB	$66,$03			; LLW	3
	DB	$60			; LB
	DB	$4C,<C0415,>C0415	; SKPFLS	C0415
; 1425:                     slot = cmdptr.1 - '0'
	DB	$28,$04			; LLA	4
	DB	$60			; LB
	DB	$2A,$30			; CB	48
	DB	$04			; SUB
	DB	$74,$02			; SLB	2
; 1426:                 else
	DB	$50,<C0416,>C0416	; SKIP	C0416
C0415:
; 1427:                     slot = 1
	DB	$2A,$01			; CB	1
	DB	$74,$02			; SLB	2
; 1428:                 fin
C0416:
; 1429:                 printtxt_10(slot)
	DB	$64,$02			; LLB	2
	DB	$54,<C0256,>C0256	; CALL	C0256
; 1430:             is 'E'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0414:
	DB	$2A,$45			; CB	69
	DB	$3E,<C0417,>C0417	; SKPNE	C0417
; 1431:                 return
	DB	$30			; DROP
	DB	$5A			; LEAVE
; 1432:             is 0
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0417:
	DB	$00			; ZERO
	DB	$3E,<C0418,>C0418	; SKPNE	C0418
; 1433:                 return
	DB	$30			; DROP
	DB	$5A			; LEAVE
; 1434:             is 'N'
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0418:
	DB	$2A,$4E			; CB	78
	DB	$3E,<C0419,>C0419	; SKPNE	C0419
; 1435:                 if chkchng_01()
	DB	$54,<C0388,>C0388	; CALL	C0388
	DB	$4C,<C0420,>C0420	; SKPFLS	C0420
; 1436:                     inittxtbuf()
	DB	$54,<C0081,>C0081	; CALL	C0081
; 1437:                     numlines = 1
	DB	$2A,$01			; CB	1
	DB	$7A,<D0209,>D0209	; SAW	D0209
; 1438:                     strcpy_20(@untitled, @txtfile)
	DB	$26,<D0135,>D0135	; LA	D0135
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1439:                 fin
C0420:
C0421:
; 1440:             otherwise
	DB	$50,<C0401,>C0401	; SKIP	C0401
C0419:
; 1441:                 bell()
	DB	$54,<C0041,>C0041	; CALL	C0041
; 1442:                 cout('?')
	DB	$2A,$3F			; CB	63
	DB	$54,<C0011,>C0011	; CALL	C0011
; 1443:                 crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1444:         wend
C0401:
	DB	$30			; DROP
; 1445:         if perr
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$4C,<C0423,>C0423	; SKPFLS	C0423
; 1446:             prstr(@errorstr)
	DB	$26,<D0079,>D0079	; LA	D0079
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1447:             drop romcall(perr, 0, 0, 0, $FDDA)
	DB	$68,<D0091,>D0091	; LAB	D0091
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$DA,$FD		; CW	64986
	DB	$54,<C0003,>C0003	; CALL	C0003
	DB	$30			; DROP
; 1448:         else
	DB	$50,<C0424,>C0424	; SKIP	C0424
C0423:
; 1449:             prstr(@okstr)
	DB	$26,<D0088,>D0088	; LA	D0088
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1450:         fin
C0424:
; 1451:         crout()
	DB	$54,<C0039,>C0039	; CALL	C0039
; 1452:     loop
	DB	$50,<C0399,>C0399	; SKIP	C0399
C0400:
; 1453: end
	DB	$5A			; LEAVE
; 1454: ;
; 1455: ; Init editor
; 1456: ;
; 1457: if !(^machid & $80)
START:	; JSR	INTERP
	DB	$2C,$98,$BF		; CW	49048
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$14			; BAND
	DB	$20			; NOT
	DB	$4C,<C0425,>C0425	; SKPFLS	C0425
; 1458:     flags = uppercase ? shiftlock
	DB	$2A,$08			; CB	8
	DB	$2A,$80			; CB	128
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1459:     keyin_01 = @keyin2_01
	DB	$26,<C0236,>C0236	; LA	C0236
	DB	$7A,<D0213,>D0213	; SAW	D0213
; 1460: else
	DB	$50,<C0426,>C0426	; SKIP	C0426
C0425:
; 1461:     keyin_01 = @keyin2e_01
	DB	$26,<C0232,>C0232	; LA	C0232
	DB	$7A,<D0213,>D0213	; SAW	D0213
; 1462: fin
C0426:
; 1463: inittxtbuf()
	DB	$54,<C0081,>C0081	; CALL	C0081
; 1464: if ^argbuff
	DB	$2C,$06,$20		; CW	8198
	DB	$60			; LB
	DB	$4C,<C0427,>C0427	; SKPFLS	C0427
; 1465:     strcpy_20(argbuff, @txtfile)
	DB	$2C,$06,$20		; CW	8198
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0043,>C0043	; CALL	C0043
; 1466:     prstr(@txtfile)
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0015,>C0015	; CALL	C0015
; 1467:     readtxt_10(@txtfile)
	DB	$26,<D0144,>D0144	; LA	D0144
	DB	$54,<C0131,>C0131	; CALL	C0131
; 1468: else
	DB	$50,<C0428,>C0428	; SKIP	C0428
C0427:
; 1469:     numlines = 1
	DB	$2A,$01			; CB	1
	DB	$7A,<D0209,>D0209	; SAW	D0209
; 1470: fin
C0428:
; 1471: curschr  = '+'
	DB	$2A,$2B			; CB	43
	DB	$78,<D0202,>D0202	; SAB	D0202
; 1472: flags = flags ? insmode
	DB	$68,<D0195,>D0195	; LAB	D0195
	DB	$2A,$02			; CB	2
	DB	$16			; IOR
	DB	$78,<D0195,>D0195	; SAB	D0195
; 1473: drawscrn_20(scrntop, scrnleft)
	DB	$6A,<D0205,>D0205	; LAW	D0205
	DB	$68,<D0199,>D0199	; LAB	D0199
	DB	$54,<C0161,>C0161	; CALL	C0161
; 1474: curson()
	DB	$54,<C0173,>C0173	; CALL	C0173
; 1475: editmode()
	DB	$54,<C0328,>C0328	; CALL	C0328
; 1476: done
	DB	$5C			; RET
