;    1: const iobuffer    = $0800
					; iobuffer = 2048
;    2: const databuff    = $0C00
					; databuff = 3072
;    3: const autorun     = $01FF
					; autorun = 511
;    4: byte version[]    = "PLASMA ][ VM VERSION 0.8"
D0000:					; version
	DB	$18
	DB	$50,$4C,$41,$53,$4D,$41,$20,$5D
	DB	$5B,$20,$56,$4D,$20,$56,$45,$52
	DB	$53,$49,$4F,$4E,$20,$30,$2E,$38
;    5: byte errorstr[]   = "ERROR: $"
D0025:					; errorstr
	DB	$08
	DB	$45,$52,$52,$4F,$52,$3A,$20,$24
;    6: byte okstr[]      = "OK"
D0034:					; okstr
	DB	$02
	DB	$4F,$4B
;    7: byte prefix[32]   = ""
D0037:					; prefix
	DB	$00
	DS	$1F
;    8: byte perr
D0068:	DS	1			; perr
;    9: word cmdptr
D0069:	DS	2			; cmdptr
;   10: 
;   11: ;
;   12: ; Utility functions
;   13: ;
;   14: ; CALL PRODOS
;   15: ; SYSCALL(CMD, PARAMS)
;   16: ;
;   17: asm prodos
C0000:					; prodos()
;   18: 		LDA		ESTKL,X
		LDA		ESTKL,X
;   19: 		LDY		ESTKH,X
		LDY		ESTKH,X
;   20: 		STA		PARAMS
		STA		PARAMS
;   21: 		STY		PARAMS+1
		STY		PARAMS+1
;   22: 		INX
		INX
;   23: 		LDA		ESTKL,X
		LDA		ESTKL,X
;   24: 		STA		CMD
		STA		CMD
;   25: 		STX		ESP
		STX		ESP
;   26: 		JSR		$BF00
		JSR		$BF00
;   27: CMD:	DB		00
CMD:	DB		00
;   28: PARAMS:	DW		0000
PARAMS:	DW		0000
;   29: 		BIT		LCBNK2
		BIT		LCBNK2
;   30: 		LDX		ESP
		LDX		ESP
;   31: 		STA		ESTKL,X
		STA		ESTKL,X
;   32: 		LDY		#$00
		LDY		#$00
;   33: 		STY		ESTKH,X
		STY		ESTKH,X
;   34: end
	RTS
;   35: ;
;   36: ; CALL LOADED SYSTEM PROGRAM
;   37: ;
;   38: asm exec
C0002:					; exec()
;   39: 		LDX		#$FF
		LDX		#$FF
;   40: 		TXS
		TXS
;   41: 		BIT		ROMIN
		BIT		ROMIN
;   42: 		JMP		$2000
		JMP		$2000
;   43: end
	RTS
;   44: ;
;   45: ; SET MEMORY TO 0
;   46: ; MEMCLR(ADDR, SIZE)
;   47: ;
;   48: asm memclr
C0004:					; memclr()
;   49: 		LDY		#$00
		LDY		#$00
;   50: 		LDA 	ESTKL+1,X
		LDA 	ESTKL+1,X
;   51: 		STA 	DSTL
		STA 	DSTL
;   52: 		LDA 	ESTKH+1,X
		LDA 	ESTKH+1,X
;   53: 		STA 	DSTH
		STA 	DSTH
;   54: 		INC		ESTKL,X
		INC		ESTKL,X
;   55:     	INC 	ESTKH,X
    	INC 	ESTKH,X
;   56: 		TYA
		TYA
;   57: SETMLP: DEC 	ESTKL,X
SETMLP: DEC 	ESTKL,X
;   58: 		BNE		:+
		BNE		:+
;   59: 		DEC		ESTKH,X
		DEC		ESTKH,X
;   60: 		BEQ		:++
		BEQ		:++
;   61: :		STA		(DST),Y
:		STA		(DST),Y
;   62: 		INY
		INY
;   63: 		BNE		SETMLP
		BNE		SETMLP
;   64: 		INC		DSTH
		INC		DSTH
;   65: 		BNE		SETMLP
		BNE		SETMLP
;   66: :		INX
:		INX
;   67: 		INX
		INX
;   68: end
	RTS
;   69: ;
;   70: ; COPY MEMORY
;   71: ; MEMCPY(SRCADDR, DSTADDR, SIZE)
;   72: ;
;   73: asm memcpy
C0006:					; memcpy()
;   74: 		LDY		#$00
		LDY		#$00
;   75: 		LDA		ESTKL,X
		LDA		ESTKL,X
;   76: 		BNE 	:+
		BNE 	:+
;   77: 		LDA		ESTKH,X
		LDA		ESTKH,X
;   78: 		BEQ		MEMEXIT
		BEQ		MEMEXIT
;   79: :		LDA		ESTKL+1,X
:		LDA		ESTKL+1,X
;   80:     	STA 	DSTL
    	STA 	DSTL
;   81: 		LDA		ESTKH+1,X
		LDA		ESTKH+1,X
;   82:     	STA 	DSTH
    	STA 	DSTH
;   83: 		LDA		ESTKL+2,X
		LDA		ESTKL+2,X
;   84:     	STA 	SRCL
    	STA 	SRCL
;   85: 		LDA		ESTKH+2,X
		LDA		ESTKH+2,X
;   86:     	STA 	SRCH
    	STA 	SRCH
;   87: 		CMP		DSTH
		CMP		DSTH
;   88: 		BCC		REVCPY
		BCC		REVCPY
;   89: 		BNE		FORCPY
		BNE		FORCPY
;   90:     	LDA 	SRCL
    	LDA 	SRCL
;   91: 		CMP		DSTL
		CMP		DSTL
;   92: 		BCS		FORCPY
		BCS		FORCPY
;   93: REVCPY:				; REVERSE DIRECTION COPY
REVCPY:				; REVERSE DIRECTION COPY
;   94: ;		CLC
;   95: 		LDA 	ESTKL,X
		LDA 	ESTKL,X
;   96: 		ADC		DSTL
		ADC		DSTL
;   97: 		STA		DSTL
		STA		DSTL
;   98: 		LDA		ESTKH,X
		LDA		ESTKH,X
;   99: 		ADC		DSTH
		ADC		DSTH
;  100: 		STA		DSTH
		STA		DSTH
;  101: 		CLC
		CLC
;  102: 		LDA 	ESTKL,X
		LDA 	ESTKL,X
;  103: 		ADC		SRCL
		ADC		SRCL
;  104: 		STA		SRCL
		STA		SRCL
;  105: 		LDA		ESTKH,X
		LDA		ESTKH,X
;  106: 		ADC		SRCH
		ADC		SRCH
;  107: 		STA		SRCH
		STA		SRCH
;  108:     	INC 	ESTKH,X
    	INC 	ESTKH,X
;  109: REVCPYLP:
REVCPYLP:
;  110: 		LDA		DSTL
		LDA		DSTL
;  111: 		BNE		:+
		BNE		:+
;  112: 		DEC		DSTH
		DEC		DSTH
;  113: :		DEC		DSTL
:		DEC		DSTL
;  114: 		LDA		SRCL
		LDA		SRCL
;  115: 		BNE		:+
		BNE		:+
;  116: 		DEC		SRCH
		DEC		SRCH
;  117: :		DEC		SRCL
:		DEC		SRCL
;  118: 		LDA		(SRC),Y
		LDA		(SRC),Y
;  119: 		STA		(DST),Y
		STA		(DST),Y
;  120: 		DEC 	ESTKL,X
		DEC 	ESTKL,X
;  121: 		BNE		REVCPYLP
		BNE		REVCPYLP
;  122: 		DEC		ESTKH,X
		DEC		ESTKH,X
;  123: 		BNE		REVCPYLP
		BNE		REVCPYLP
;  124: 		BEQ		MEMEXIT
		BEQ		MEMEXIT
;  125: FORCPY: INC 	ESTKH,X
FORCPY: INC 	ESTKH,X
;  126: FORCPYLP:
FORCPYLP:
;  127: 		LDA		(SRC),Y
		LDA		(SRC),Y
;  128: 		STA		(DST),Y
		STA		(DST),Y
;  129: 		INC		DSTL
		INC		DSTL
;  130: 		BNE		:+
		BNE		:+
;  131: 		INC		DSTH
		INC		DSTH
;  132: :		INC		SRCL
:		INC		SRCL
;  133: 		BNE		:+
		BNE		:+
;  134: 		INC		SRCH
		INC		SRCH
;  135: :		DEC 	ESTKL,X
:		DEC 	ESTKL,X
;  136: 		BNE		FORCPYLP
		BNE		FORCPYLP
;  137: 		DEC		ESTKH,X
		DEC		ESTKH,X
;  138: 		BNE		FORCPYLP
		BNE		FORCPYLP
;  139: MEMEXIT: INX
MEMEXIT: INX
;  140: 		INX
		INX
;  141: 		INX
		INX
;  142: end
	RTS
;  143: ;
;  144: ; CHAR OUT
;  145: ; COUT(CHAR)
;  146: ;
;  147: asm cout
C0008:					; cout()
;  148: 		LDA		ESTKL,X
		LDA		ESTKL,X
;  149:     	INX
    	INX
;  150: 		ORA 	#$80
		ORA 	#$80
;  151: 		BIT		ROMIN
		BIT		ROMIN
;  152: 		JSR		$FDED
		JSR		$FDED
;  153: 		BIT		LCBNK2
		BIT		LCBNK2
;  154: end
	RTS
;  155: ;
;  156: ; CHAR IN
;  157: ; RDKEY()
;  158: ;
;  159: asm cin
C0010:					; cin()
;  160: 		BIT		ROMIN
		BIT		ROMIN
;  161: 		STX		ESP
		STX		ESP
;  162: 		JSR     $FD0C
		JSR     $FD0C
;  163: 		LDX		ESP
		LDX		ESP
;  164: 		BIT		LCBNK2
		BIT		LCBNK2
;  165:     	DEX
    	DEX
;  166:         STA     ESTKL,X
        STA     ESTKL,X
;  167: 		LDY		#$00
		LDY		#$00
;  168:         STY     ESTKH,X
        STY     ESTKH,X
;  169: end
	RTS
;  170: ;
;  171: ; PRINT STRING
;  172: ; PRSTR(STR)
;  173: ;
;  174: asm prstr
C0012:					; prstr()
;  175: 		LDY		#$00
		LDY		#$00
;  176: 		LDA     ESTKL,X
		LDA     ESTKL,X
;  177:         STA     SRCL
        STA     SRCL
;  178:         LDA     ESTKH,X
        LDA     ESTKH,X
;  179:         STA     SRCH
        STA     SRCH
;  180: 		BIT		ROMIN
		BIT		ROMIN
;  181:         LDA     (SRC),Y
        LDA     (SRC),Y
;  182:         STA     ESTKL,X
        STA     ESTKL,X
;  183:         BEQ     :+
        BEQ     :+
;  184: _PRS1:	INY
_PRS1:	INY
;  185: 		LDA		(SRC),Y
		LDA		(SRC),Y
;  186:         ORA		#$80
        ORA		#$80
;  187: 		JSR		$FDED
		JSR		$FDED
;  188:         TYA
        TYA
;  189:         CMP		ESTKL,X
        CMP		ESTKL,X
;  190: 		BNE		_PRS1
		BNE		_PRS1
;  191: :		INX
:		INX
;  192: 		BIT		LCBNK2
		BIT		LCBNK2
;  193: end
	RTS
;  194: ;
;  195: ; PRINT BYTE
;  196: ;
;  197: asm prbyte
C0014:					; prbyte()
;  198: 		LDA		ESTKL,X
		LDA		ESTKL,X
;  199:     	INX
    	INX
;  200:         STX     ESP
        STX     ESP
;  201: 		BIT		ROMIN
		BIT		ROMIN
;  202: 		JSR		$FDDA
		JSR		$FDDA
;  203: 		BIT		LCBNK2
		BIT		LCBNK2
;  204:         LDX     ESP
        LDX     ESP
;  205: end
	RTS
;  206: ;
;  207: ; READ STRING
;  208: ; STR = RDSTR(PROMPTCHAR)
;  209: ;
;  210: asm rdstr
C0016:					; rdstr()
;  211: 		LDA     ESTKL,X
		LDA     ESTKL,X
;  212:         STA     $33
        STA     $33
;  213:         STX     ESP
        STX     ESP
;  214: 		BIT		ROMIN
		BIT		ROMIN
;  215:         JSR     $FD6A
        JSR     $FD6A
;  216: 		BIT		LCBNK2
		BIT		LCBNK2
;  217:         STX     $01FF
        STX     $01FF
;  218: :		LDA		$01FF,X
:		LDA		$01FF,X
;  219: 		AND		#$7F
		AND		#$7F
;  220: 		STA		$01FF,X
		STA		$01FF,X
;  221: 		DEX
		DEX
;  222: 		BPL		:-
		BPL		:-
;  223: 		LDX     ESP
		LDX     ESP
;  224:         LDA     #$FF
        LDA     #$FF
;  225:         STA     ESTKL,X
        STA     ESTKL,X
;  226:         LDA     #$01
        LDA     #$01
;  227:         STA     ESTKH,X
        STA     ESTKH,X
;  228: end
	RTS
;  229: asm toupper
C0018:					; toupper()
;  230:         LDA     ESTKL,X
        LDA     ESTKL,X
;  231:         CMP     #'a'
        CMP     #'a'
;  232:         BCC     :+
        BCC     :+
;  233:         CMP     #'z'+1
        CMP     #'z'+1
;  234:         BCS     :+
        BCS     :+
;  235:         SEC
        SEC
;  236:         SBC     #$20
        SBC     #$20
;  237:         STA     ESTKL,X
        STA     ESTKL,X
;  238: :
:
;  239: end
	RTS
;  240: ;
;  241: ; EXIT
;  242: ;
;  243: asm reboot
C0020:					; reboot()
;  244: 		BIT		ROMIN
		BIT		ROMIN
;  245: 		LDA		#$00
		LDA		#$00
;  246: 		STA		$3F4		; INVALIDATE POWER-UP BYTE
		STA		$3F4		; INVALIDATE POWER-UP BYTE
;  247: 		JMP		($FFFC)		; RESET
		JMP		($FFFC)		; RESET
;  248: end
	RTS
;  249: def crout
C0022:					; crout()
;  250:     cout($0D)
	JSR	INTERP
	DB	$2A,$0D			; CB	13
	DB	$54,<C0008,>C0008	; CALL	C0008
;  251: end
	DB	$5C			; RET
;  252: ;
;  253: ; ProDOS routines
;  254: ;
;  255: def getpfx(path)
C0024:					; getpfx()
					; path = 2
;  256:     byte params[3]
					; params = 4
;  257: 
;  258:     ^path    = 0
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$00			; ZERO
	DB	$70			; SB
;  259:     params.0 = 1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  260:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  261:     perr     = prodos($C7, @params)
	DB	$2A,$C7			; CB	199
	DB	$28,$04			; LLA	4
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$78,<D0068,>D0068	; SAB	D0068
;  262:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  263: end
;  264: def setpfx(path)
C0026:					; setpfx()
					; path = 2
;  265:     byte params[3]
					; params = 4
;  266: 
;  267:     params.0 = 1
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  268:     params:1 = path
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  269:     perr     = prodos($C6, @params)
	DB	$2A,$C6			; CB	198
	DB	$28,$04			; LLA	4
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$78,<D0068,>D0068	; SAB	D0068
;  270:     return path
	DB	$66,$02			; LLW	2
	DB	$5A			; LEAVE
;  271: end
;  272: def online
C0028:					; online()
;  273:     byte params[4]
					; params = 2
;  274: 
;  275:     params.0 = 2
	JSR	INTERP
	DB	$58,$06,$00		; ENTER	6,0
	DB	$28,$02			; LLA	2
	DB	$2A,$02			; CB	2
	DB	$70			; SB
;  276:     params.1 = 0
	DB	$28,$03			; LLA	3
	DB	$00			; ZERO
	DB	$70			; SB
;  277:     params:2 = $2000
	DB	$28,$04			; LLA	4
	DB	$2C,$00,$20		; CW	8192
	DB	$72			; SW
;  278:     perr     = prodos($C5, @params)
	DB	$2A,$C5			; CB	197
	DB	$28,$02			; LLA	2
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$78,<D0068,>D0068	; SAB	D0068
;  279:     return $2000
	DB	$2C,$00,$20		; CW	8192
	DB	$5A			; LEAVE
;  280: end
;  281: def open(path, buff)
C0030:					; open()
					; path = 2
					; buff = 4
;  282:     byte params[6]
					; params = 6
;  283: 
;  284:     params.0 = 3
	JSR	INTERP
	DB	$58,$0C,$02		; ENTER	12,2
	DB	$28,$06			; LLA	6
	DB	$2A,$03			; CB	3
	DB	$70			; SB
;  285:     params:1 = path
	DB	$28,$07			; LLA	7
	DB	$66,$02			; LLW	2
	DB	$72			; SW
;  286:     params:3 = buff
	DB	$28,$09			; LLA	9
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  287:     params.5 = 0
	DB	$28,$0B			; LLA	11
	DB	$00			; ZERO
	DB	$70			; SB
;  288:     perr     = prodos($C8, @params)
	DB	$2A,$C8			; CB	200
	DB	$28,$06			; LLA	6
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$78,<D0068,>D0068	; SAB	D0068
;  289:     return params.5
	DB	$28,$0B			; LLA	11
	DB	$60			; LB
	DB	$5A			; LEAVE
;  290: end
;  291: def close(refnum)
C0032:					; close()
					; refnum = 2
;  292:     byte params[2]
					; params = 4
;  293: 
;  294:     params.0 = 1
	JSR	INTERP
	DB	$58,$06,$01		; ENTER	6,1
	DB	$28,$04			; LLA	4
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  295:     params.1 = refnum
	DB	$28,$05			; LLA	5
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  296:     perr     = prodos($CC, @params)
	DB	$2A,$CC			; CB	204
	DB	$28,$04			; LLA	4
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$78,<D0068,>D0068	; SAB	D0068
;  297:     return perr
	DB	$68,<D0068,>D0068	; LAB	D0068
	DB	$5A			; LEAVE
;  298: end
;  299: def read(refnum, buff, len)
C0034:					; read()
					; refnum = 2
					; buff = 4
					; len = 6
;  300:     byte params[8]
					; params = 8
;  301: 
;  302:     params.0 = 4
	JSR	INTERP
	DB	$58,$10,$03		; ENTER	16,3
	DB	$28,$08			; LLA	8
	DB	$2A,$04			; CB	4
	DB	$70			; SB
;  303:     params.1 = refnum
	DB	$28,$09			; LLA	9
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  304:     params:2 = buff
	DB	$28,$0A			; LLA	10
	DB	$66,$04			; LLW	4
	DB	$72			; SW
;  305:     params:4 = len
	DB	$28,$0C			; LLA	12
	DB	$66,$06			; LLW	6
	DB	$72			; SW
;  306:     params:6 = 0
	DB	$28,$0E			; LLA	14
	DB	$00			; ZERO
	DB	$72			; SW
;  307:     perr     = prodos($CA, @params)
	DB	$2A,$CA			; CB	202
	DB	$28,$08			; LLA	8
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$78,<D0068,>D0068	; SAB	D0068
;  308:     return params:6
	DB	$28,$0E			; LLA	14
	DB	$62			; LW
	DB	$5A			; LEAVE
;  309: end
;  310: ;
;  311: ; Command mode
;  312: ;
;  313: def volumes
C0036:					; volumes()
;  314: 	word strbuf
					; strbuf = 2
;  315: 	byte i
					; i = 4
;  316: 
;  317: 	strbuf = online()
	JSR	INTERP
	DB	$58,$05,$00		; ENTER	5,0
	DB	$54,<C0028,>C0028	; CALL	C0028
	DB	$76,$02			; SLW	2
;  318: 	for i = 0 to 15
	DB	$00			; ZERO
C0039:
	DB	$6C,$04			; DLB	4
	DB	$2A,$0F			; CB	15
	DB	$3A,<C0038,>C0038	; SKPGT	C0038
	DB	$0C			; INCR
;  319: 		^strbuf = ^strbuf & $0F
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$70			; SB
;  320: 		if ^strbuf
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0040,>C0040	; SKPFLS	C0040
;  321: 			cout('/')
	DB	$2A,$2F			; CB	47
	DB	$54,<C0008,>C0008	; CALL	C0008
;  322: 			prstr(strbuf)
	DB	$66,$02			; LLW	2
	DB	$54,<C0012,>C0012	; CALL	C0012
;  323: 			crout()
	DB	$54,<C0022,>C0022	; CALL	C0022
;  324: 		fin
C0040:
C0041:
;  325: 		strbuf = strbuf + 16
	DB	$66,$02			; LLW	2
	DB	$2A,$10			; CB	16
	DB	$02			; ADD
	DB	$76,$02			; SLW	2
;  326: 	next
	DB	$50,<C0039,>C0039	; SKIP	C0039
C0038:
	DB	$30			; DROP
;  327: end
	DB	$5A			; LEAVE
;  328: def catalog(optpath)
C0042:					; catalog()
					; optpath = 2
;  329:     byte path[64]
					; path = 4
;  330:     byte refnum
					; refnum = 68
;  331:     byte firstblk
					; firstblk = 69
;  332:     byte entrylen, entriesblk
					; entrylen = 70
					; entriesblk = 71
;  333:     byte i, type, len
					; i = 72
					; type = 73
					; len = 74
;  334:     word entry, filecnt
					; entry = 75
					; filecnt = 77
;  335: 
;  336:     if ^optpath
	JSR	INTERP
	DB	$58,$4F,$01		; ENTER	79,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0044,>C0044	; SKPFLS	C0044
;  337:         memcpy(optpath, @path, ^optpath + 1)
	DB	$66,$02			; LLW	2
	DB	$28,$04			; LLA	4
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0006,>C0006	; CALL	C0006
;  338:     else
	DB	$50,<C0045,>C0045	; SKIP	C0045
C0044:
;  339:         drop getpfx(@path)
	DB	$28,$04			; LLA	4
	DB	$54,<C0024,>C0024	; CALL	C0024
	DB	$30			; DROP
;  340:         prstr(@path)
	DB	$28,$04			; LLA	4
	DB	$54,<C0012,>C0012	; CALL	C0012
;  341:         crout()
	DB	$54,<C0022,>C0022	; CALL	C0022
;  342:     fin
C0045:
;  343:     refnum = open(@path, iobuffer);
	DB	$28,$04			; LLA	4
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0030,>C0030	; CALL	C0030
	DB	$74,$44			; SLB	68
;  344:     if perr
	DB	$68,<D0068,>D0068	; LAB	D0068
	DB	$4C,<C0046,>C0046	; SKPFLS	C0046
;  345:         return perr
	DB	$68,<D0068,>D0068	; LAB	D0068
	DB	$5A			; LEAVE
;  346:     fin
C0046:
C0047:
;  347:     firstblk = 1
	DB	$2A,$01			; CB	1
	DB	$74,$45			; SLB	69
;  348:     repeat
C0049:
;  349:         if read(refnum, databuff, 512) == 512
	DB	$64,$44			; LLB	68
	DB	$2C,$00,$0C		; CW	3072
	DB	$2C,$00,$02		; CW	512
	DB	$54,<C0034,>C0034	; CALL	C0034
	DB	$2C,$00,$02		; CW	512
	DB	$40			; ISEQ
	DB	$4C,<C0050,>C0050	; SKPFLS	C0050
;  350:             entry = databuff + 4
	DB	$2C,$00,$0C		; CW	3072
	DB	$2A,$04			; CB	4
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
;  351:             if firstblk
	DB	$64,$45			; LLB	69
	DB	$4C,<C0052,>C0052	; SKPFLS	C0052
;  352:                 entrylen   = databuff.$23
	DB	$2C,$23,$0C		; CW	3107
	DB	$60			; LB
	DB	$74,$46			; SLB	70
;  353:                 entriesblk = databuff.$24
	DB	$2C,$24,$0C		; CW	3108
	DB	$60			; LB
	DB	$74,$47			; SLB	71
;  354:                 filecnt    = databuff:$25
	DB	$2C,$25,$0C		; CW	3109
	DB	$62			; LW
	DB	$76,$4D			; SLW	77
;  355:                 entry      = entry + entrylen
	DB	$66,$4B			; LLW	75
	DB	$64,$46			; LLB	70
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
;  356:             fin
C0052:
C0053:
;  357:             for i = firstblk to entriesblk
	DB	$64,$45			; LLB	69
C0055:
	DB	$6C,$48			; DLB	72
	DB	$64,$47			; LLB	71
	DB	$3A,<C0054,>C0054	; SKPGT	C0054
	DB	$0C			; INCR
;  358:                 type = ^entry
	DB	$66,$4B			; LLW	75
	DB	$60			; LB
	DB	$74,$49			; SLB	73
;  359:                 if type <> 0
	DB	$64,$49			; LLB	73
	DB	$00			; ZERO
	DB	$42			; ISNE
	DB	$4C,<C0056,>C0056	; SKPFLS	C0056
;  360:                     len = type & $0F
	DB	$64,$49			; LLB	73
	DB	$2A,$0F			; CB	15
	DB	$14			; BAND
	DB	$74,$4A			; SLB	74
;  361:                     ^entry = len
	DB	$66,$4B			; LLW	75
	DB	$64,$4A			; LLB	74
	DB	$70			; SB
;  362:                     prstr(entry)
	DB	$66,$4B			; LLW	75
	DB	$54,<C0012,>C0012	; CALL	C0012
;  363:                     if type & $F0 == $D0 ; Is it a directory?
	DB	$64,$49			; LLB	73
	DB	$2A,$F0			; CB	240
	DB	$14			; BAND
	DB	$2A,$D0			; CB	208
	DB	$40			; ISEQ
	DB	$4C,<C0058,>C0058	; SKPFLS	C0058
;  364:                         cout('/')
	DB	$2A,$2F			; CB	47
	DB	$54,<C0008,>C0008	; CALL	C0008
;  365:                         len = len + 1
	DB	$64,$4A			; LLB	74
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$4A			; SLB	74
;  366: 					elsif (entry).$10 == $FF
	DB	$50,<C0059,>C0059	; SKIP	C0059
C0058:
	DB	$66,$4B			; LLW	75
	DB	$2A,$10			; CB	16
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$FF			; CB	255
	DB	$40			; ISEQ
	DB	$4C,<C0060,>C0060	; SKPFLS	C0060
;  367: 						cout('*')
	DB	$2A,$2A			; CB	42
	DB	$54,<C0008,>C0008	; CALL	C0008
;  368: 						len = len + 1
	DB	$64,$4A			; LLB	74
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$74,$4A			; SLB	74
;  369:                     fin
C0060:
C0059:
;  370:                     for len = 19 - len downto 0
	DB	$2A,$13			; CB	19
	DB	$64,$4A			; LLB	74
	DB	$04			; SUB
C0062:
	DB	$6C,$4A			; DLB	74
	DB	$00			; ZERO
	DB	$38,<C0061,>C0061	; SKPLT	C0061
	DB	$0E			; DECR
;  371:                         cout(' ')
	DB	$2A,$20			; CB	32
	DB	$54,<C0008,>C0008	; CALL	C0008
;  372:                     next
	DB	$50,<C0062,>C0062	; SKIP	C0062
C0061:
	DB	$30			; DROP
;  373:                     filecnt = filecnt - 1
	DB	$66,$4D			; LLW	77
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$76,$4D			; SLW	77
;  374:                 fin
C0056:
C0057:
;  375:                 entry = entry + entrylen
	DB	$66,$4B			; LLW	75
	DB	$64,$46			; LLB	70
	DB	$02			; ADD
	DB	$76,$4B			; SLW	75
;  376:             next
	DB	$50,<C0055,>C0055	; SKIP	C0055
C0054:
	DB	$30			; DROP
;  377:             firstblk = 0
	DB	$00			; ZERO
	DB	$74,$45			; SLB	69
;  378:         else
	DB	$50,<C0051,>C0051	; SKIP	C0051
C0050:
;  379:             filecnt = 0
	DB	$00			; ZERO
	DB	$76,$4D			; SLW	77
;  380:         fin
C0051:
;  381:     until filecnt == 0
	DB	$66,$4D			; LLW	77
	DB	$00			; ZERO
	DB	$40			; ISEQ
	DB	$4C,<C0049,>C0049	; SKPFLS	C0049
C0048:
;  382:     drop close(refnum)
	DB	$64,$44			; LLB	68
	DB	$54,<C0032,>C0032	; CALL	C0032
	DB	$30			; DROP
;  383:     crout()
	DB	$54,<C0022,>C0022	; CALL	C0022
;  384:     return 0
	DB	$00			; ZERO
	DB	$5A			; LEAVE
;  385: end
;  386: def stripchars(strptr)
C0063:					; stripchars()
					; strptr = 2
;  387:     while ^strptr and ^(strptr + 1) <> ' '
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
C0065:
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$20			; CB	32
	DB	$42			; ISNE
	DB	$24			; LAND
	DB	$4C,<C0066,>C0066	; SKPFLS	C0066
;  388:         memcpy(strptr + 2, strptr + 1, ^strptr)
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$54,<C0006,>C0006	; CALL	C0006
;  389:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
;  390:     loop
	DB	$50,<C0065,>C0065	; SKIP	C0065
C0066:
;  391: 	return ^strptr
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$5A			; LEAVE
;  392: end
;  393: def stripspaces(strptr)
C0067:					; stripspaces()
					; strptr = 2
;  394: 	while ^strptr and ^(strptr + ^strptr) <= ' '
	JSR	INTERP
	DB	$58,$04,$01		; ENTER	4,1
C0069:
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$20			; CB	32
	DB	$4A			; ISLE
	DB	$24			; LAND
	DB	$4C,<C0070,>C0070	; SKPFLS	C0070
;  395:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
;  396:     loop
	DB	$50,<C0069,>C0069	; SKIP	C0069
C0070:
;  397:     while ^strptr and ^(strptr + 1) <= ' '
C0071:
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$2A,$20			; CB	32
	DB	$4A			; ISLE
	DB	$24			; LAND
	DB	$4C,<C0072,>C0072	; SKPFLS	C0072
;  398:         memcpy(strptr + 2, strptr + 1, ^strptr)
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$54,<C0006,>C0006	; CALL	C0006
;  399:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
;  400:     loop
	DB	$50,<C0071,>C0071	; SKIP	C0071
C0072:
;  401: end
	DB	$5A			; LEAVE
;  402: def striptrail(strptr)
C0073:					; striptrail()
					; strptr = 2
;  403: 	byte i
					; i = 4
;  404: 
;  405: 	for i = 1 to ^strptr
	JSR	INTERP
	DB	$58,$05,$01		; ENTER	5,1
	DB	$2A,$01			; CB	1
C0076:
	DB	$6C,$04			; DLB	4
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$3A,<C0075,>C0075	; SKPGT	C0075
	DB	$0C			; INCR
;  406: 		if (strptr)[i] == ' '
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$02			; IDXB
	DB	$60			; LB
	DB	$2A,$20			; CB	32
	DB	$40			; ISEQ
	DB	$4C,<C0077,>C0077	; SKPFLS	C0077
;  407: 			^strptr = i - 1
	DB	$66,$02			; LLW	2
	DB	$64,$04			; LLB	4
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
;  408: 			return
	DB	$30			; DROP
	DB	$5A			; LEAVE
;  409: 		fin
C0077:
C0078:
;  410: 	next
	DB	$50,<C0076,>C0076	; SKIP	C0076
C0075:
	DB	$30			; DROP
;  411: end
	DB	$5A			; LEAVE
;  412: def parsecmd(strptr)
C0079:					; parsecmd()
					; strptr = 2
;  413:     byte cmd
					; cmd = 4
;  414: 
;  415:     cmd = 0
	JSR	INTERP
	DB	$58,$05,$01		; ENTER	5,1
	DB	$00			; ZERO
	DB	$74,$04			; SLB	4
;  416:     stripspaces(strptr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0067,>C0067	; CALL	C0067
;  417:     if ^strptr
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0081,>C0081	; SKPFLS	C0081
;  418:         cmd = ^(strptr + 1)
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$60			; LB
	DB	$74,$04			; SLB	4
;  419:         memcpy(strptr + 2, strptr + 1, ^strptr)
	DB	$66,$02			; LLW	2
	DB	$2A,$02			; CB	2
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$54,<C0006,>C0006	; CALL	C0006
;  420:         ^strptr = ^strptr - 1
	DB	$66,$02			; LLW	2
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$04			; SUB
	DB	$70			; SB
;  421:     fin
C0081:
C0082:
;  422:     stripspaces(strptr)
	DB	$66,$02			; LLW	2
	DB	$54,<C0067,>C0067	; CALL	C0067
;  423:     return cmd
	DB	$64,$04			; LLB	4
	DB	$5A			; LEAVE
;  424: end
;  425: def resetmemfiles
C0083:					; resetmemfiles()
;  426: 	;
;  427: 	; Close all files
;  428: 	;
;  429: 	^$BFD8 = 0
	JSR	INTERP
	DB	$2C,$D8,$BF		; CW	49112
	DB	$00			; ZERO
	DB	$70			; SB
;  430: 	drop close(0)
	DB	$00			; ZERO
	DB	$54,<C0032,>C0032	; CALL	C0032
	DB	$30			; DROP
;  431: 	;
;  432: 	; Set memory bitmap
;  433: 	;
;  434: 	memclr($BF58, 24)
	DB	$2C,$58,$BF		; CW	48984
	DB	$2A,$18			; CB	24
	DB	$54,<C0004,>C0004	; CALL	C0004
;  435: 	^$BF58 = $CF
	DB	$2C,$58,$BF		; CW	48984
	DB	$2A,$CF			; CB	207
	DB	$70			; SB
;  436: 	^$BF6F = $01
	DB	$2C,$6F,$BF		; CW	49007
	DB	$2A,$01			; CB	1
	DB	$70			; SB
;  437: end
	DB	$5C			; RET
;  438: def execsys(sysfile)
C0085:					; execsys()
					; sysfile = 2
;  439: 	byte refnum
					; refnum = 4
;  440: 	word len
					; len = 5
;  441: 
;  442: 	if ^sysfile
	JSR	INTERP
	DB	$58,$07,$01		; ENTER	7,1
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4C,<C0087,>C0087	; SKPFLS	C0087
;  443: 		memcpy(sysfile, $280, ^sysfile + 1)
	DB	$66,$02			; LLW	2
	DB	$2C,$80,$02		; CW	640
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0006,>C0006	; CALL	C0006
;  444: 		striptrail(sysfile)
	DB	$66,$02			; LLW	2
	DB	$54,<C0073,>C0073	; CALL	C0073
;  445: 		refnum = open(sysfile, iobuffer)
	DB	$66,$02			; LLW	2
	DB	$2C,$00,$08		; CW	2048
	DB	$54,<C0030,>C0030	; CALL	C0030
	DB	$74,$04			; SLB	4
;  446: 		if refnum
	DB	$64,$04			; LLB	4
	DB	$4C,<C0089,>C0089	; SKPFLS	C0089
;  447: 			len = read(refnum, $2000, $FFFF)
	DB	$64,$04			; LLB	4
	DB	$2C,$00,$20		; CW	8192
	DB	$2C,$FF,$FF		; CW	65535
	DB	$54,<C0034,>C0034	; CALL	C0034
	DB	$76,$05			; SLW	5
;  448: 			resetmemfiles()
	DB	$54,<C0083,>C0083	; CALL	C0083
;  449: 			if len
	DB	$66,$05			; LLW	5
	DB	$4C,<C0091,>C0091	; SKPFLS	C0091
;  450: 				memcpy($280, sysfile, ^$280 + 1)
	DB	$2C,$80,$02		; CW	640
	DB	$66,$02			; LLW	2
	DB	$2C,$80,$02		; CW	640
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0006,>C0006	; CALL	C0006
;  451: 				if stripchars(sysfile) and ^$2000 == $4C and *$2003 == $EEEE
	DB	$66,$02			; LLW	2
	DB	$54,<C0063,>C0063	; CALL	C0063
	DB	$2C,$00,$20		; CW	8192
	DB	$60			; LB
	DB	$2A,$4C			; CB	76
	DB	$40			; ISEQ
	DB	$2C,$03,$20		; CW	8195
	DB	$62			; LW
	DB	$2C,$EE,$EE		; CW	61166
	DB	$40			; ISEQ
	DB	$24			; LAND
	DB	$24			; LAND
	DB	$4C,<C0093,>C0093	; SKPFLS	C0093
;  452: 					stripspaces(sysfile)
	DB	$66,$02			; LLW	2
	DB	$54,<C0067,>C0067	; CALL	C0067
;  453: 					if ^$2006 <= ^sysfile
	DB	$2C,$06,$20		; CW	8198
	DB	$60			; LB
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$4A			; ISLE
	DB	$4C,<C0095,>C0095	; SKPFLS	C0095
;  454: 						memcpy(sysfile, $2006, ^sysfile + 1)
	DB	$66,$02			; LLW	2
	DB	$2C,$06,$20		; CW	8198
	DB	$66,$02			; LLW	2
	DB	$60			; LB
	DB	$2A,$01			; CB	1
	DB	$02			; ADD
	DB	$54,<C0006,>C0006	; CALL	C0006
;  455: 					fin
C0095:
C0096:
;  456: 				fin
C0093:
C0094:
;  457: 				striptrail($280)
	DB	$2C,$80,$02		; CW	640
	DB	$54,<C0073,>C0073	; CALL	C0073
;  458: 				exec()
	DB	$54,<C0002,>C0002	; CALL	C0002
;  459: 			fin
C0091:
C0092:
;  460: 		fin
C0089:
C0090:
;  461: 	fin
C0087:
C0088:
;  462: end
	DB	$5A			; LEAVE
;  463: 
;  464: resetmemfiles()
START:	; JSR	INTERP
	DB	$54,<C0083,>C0083	; CALL	C0083
;  465: execsys(autorun)
	DB	$2C,$FF,$01		; CW	511
	DB	$54,<C0085,>C0085	; CALL	C0085
;  466: prstr(@version)
	DB	$26,<D0000,>D0000	; LA	D0000
	DB	$54,<C0012,>C0012	; CALL	C0012
;  467: crout();
	DB	$54,<C0022,>C0022	; CALL	C0022
;  468: while 1
C0097:
	DB	$2A,$01			; CB	1
	DB	$4C,<C0098,>C0098	; SKPFLS	C0098
;  469: 	prstr(getpfx(@prefix))
	DB	$26,<D0037,>D0037	; LA	D0037
	DB	$54,<C0024,>C0024	; CALL	C0024
	DB	$54,<C0012,>C0012	; CALL	C0012
;  470: 	cmdptr = rdstr($BA)
	DB	$2A,$BA			; CB	186
	DB	$54,<C0016,>C0016	; CALL	C0016
	DB	$7A,<D0069,>D0069	; SAW	D0069
;  471: 	when toupper(parsecmd(cmdptr))
	DB	$6A,<D0069,>D0069	; LAW	D0069
	DB	$54,<C0079,>C0079	; CALL	C0079
	DB	$54,<C0018,>C0018	; CALL	C0018
;  472: 		is 'Q'
	DB	$2A,$51			; CB	81
	DB	$3E,<C0100,>C0100	; SKPNE	C0100
;  473: 			reboot()
	DB	$54,<C0020,>C0020	; CALL	C0020
;  474: 		is 'C'
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0100:
	DB	$2A,$43			; CB	67
	DB	$3E,<C0101,>C0101	; SKPNE	C0101
;  475: 			drop catalog(cmdptr)
	DB	$6A,<D0069,>D0069	; LAW	D0069
	DB	$54,<C0042,>C0042	; CALL	C0042
	DB	$30			; DROP
;  476: 		is 'P'
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0101:
	DB	$2A,$50			; CB	80
	DB	$3E,<C0102,>C0102	; SKPNE	C0102
;  477: 			drop setpfx(cmdptr)
	DB	$6A,<D0069,>D0069	; LAW	D0069
	DB	$54,<C0026,>C0026	; CALL	C0026
	DB	$30			; DROP
;  478: 		is 'V'
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0102:
	DB	$2A,$56			; CB	86
	DB	$3E,<C0103,>C0103	; SKPNE	C0103
;  479: 			volumes();
	DB	$54,<C0036,>C0036	; CALL	C0036
;  480: 		is '-'
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0103:
	DB	$2A,$2D			; CB	45
	DB	$3E,<C0104,>C0104	; SKPNE	C0104
;  481: 			execsys(cmdptr)
	DB	$6A,<D0069,>D0069	; LAW	D0069
	DB	$54,<C0085,>C0085	; CALL	C0085
;  482: 			perr = $46
	DB	$2A,$46			; CB	70
	DB	$78,<D0068,>D0068	; SAB	D0068
;  483: 	wend
	DB	$50,<C0099,>C0099	; SKIP	C0099
C0104:
C0099:
	DB	$30			; DROP
;  484: 	if perr
	DB	$68,<D0068,>D0068	; LAB	D0068
	DB	$4C,<C0106,>C0106	; SKPFLS	C0106
;  485: 		prstr(@errorstr)
	DB	$26,<D0025,>D0025	; LA	D0025
	DB	$54,<C0012,>C0012	; CALL	C0012
;  486: 		prbyte(perr)
	DB	$68,<D0068,>D0068	; LAB	D0068
	DB	$54,<C0014,>C0014	; CALL	C0014
;  487: 	else
	DB	$50,<C0107,>C0107	; SKIP	C0107
C0106:
;  488: 		prstr(@okstr)
	DB	$26,<D0034,>D0034	; LA	D0034
	DB	$54,<C0012,>C0012	; CALL	C0012
;  489: 	fin
C0107:
;  490: 	crout()
	DB	$54,<C0022,>C0022	; CALL	C0022
;  491: loop
	DB	$50,<C0097,>C0097	; SKIP	C0097
C0098:
;  492: done
	DB	$5C			; RET
