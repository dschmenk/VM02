	.INCLUDE	"plstub.s"
;    1: const speaker=$C030
					; speaker = 49200
;    2: const showgraphics=$C050
					; showgraphics = 49232
;    3: const showtext=$C051
					; showtext = 49233
;    4: const showfull=$C052
					; showfull = 49234
;    5: const showmix=$C053
					; showmix = 49235
;    6: const TRUE=$FFFF
					; TRUE = 65535
;    7: const FALSE=$0000
					; FALSE = 0
;    8: const showpage1=$C054
					; showpage1 = 49236
;    9: const showpage2=$C055
					; showpage2 = 49237
;   10: const showlores=$C056
					; showlores = 49238
;   11: const showhires=$C057
					; showhires = 49239
;   12: const keyboard=$C000
					; keyboard = 49152
;   13: const keystrobe=$C010
					; keystrobe = 49168
;   14: const hgr1=$2000
					; hgr1 = 8192
;   15: const hgr2=$4000
					; hgr2 = 16384
;   16: const page1=0
					; page1 = 0
;   17: const page2=1
					; page2 = 1
;   18: byte exitmsg[] = "PRESS ANY KEY TO EXIT."
D0000:					; exitmsg
	DB	$16
	DB	$50,$52,$45,$53,$53,$20,$41,$4E
	DB	$59,$20,$4B,$45,$59,$20,$54,$4F
	DB	$20,$45,$58,$49,$54,$2E
;   19: byte goodbye[] = "THAT'S ALL FOLKS!"
D0023:					; goodbye
	DB	$11
	DB	$54,$48,$41,$54,$27,$53,$20,$41
	DB	$4C,$4C,$20,$46,$4F,$4C,$4B,$53
	DB	$21
;   20: byte rebootmsg[] = "PRESS ANY KEY TO REBOOT..."
D0041:					; rebootmsg
	DB	$1A
	DB	$50,$52,$45,$53,$53,$20,$41,$4E
	DB	$59,$20,$4B,$45,$59,$20,$54,$4F
	DB	$20,$52,$45,$42,$4F,$4F,$54,$2E
	DB	$2E,$2E
;   21: byte i, j, k, w, fmi, fmk, color
D0068:	DS	1			; i
D0069:	DS	1			; j
D0070:	DS	1			; k
D0071:	DS	1			; w
D0072:	DS	1			; fmi
D0073:	DS	1			; fmk
D0074:	DS	1			; color
;   22: ;
;   23: ; CALL 6502 ROUTINE
;   24: ; ROMCALL(AREG, XREG, YREG, STATUS, ADDR)
;   25: ;
;   26: asm romcall
C0000:					; romcall()
;   27: TMP	EQU	$06
TMP	EQU	$06
;   28: 
;   29: 	PHP
	PHP
;   30: 	LDA	ESTKL,X
	LDA	ESTKL,X
;   31: 	STA	TMP
	STA	TMP
;   32: 	LDA	ESTKH,X
	LDA	ESTKH,X
;   33: 	STA	TMP+1
	STA	TMP+1
;   34: 	INX
	INX
;   35: 	LDA	ESTKL,X
	LDA	ESTKL,X
;   36: 	PHA
	PHA
;   37: 	INX
	INX
;   38: 	LDA	ESTKL,X
	LDA	ESTKL,X
;   39: 	TAY
	TAY
;   40: 	INX
	INX
;   41: 	LDA	ESTKL+1,X
	LDA	ESTKL+1,X
;   42: 	PHA
	PHA
;   43: 	LDA	ESTKL,X
	LDA	ESTKL,X
;   44: 	INX
	INX
;   45: 	STX	TMP+2
	STX	TMP+2
;   46: 	TAX
	TAX
;   47: 	PLA
	PLA
;   48: 	BIT	ROMIN
	BIT	ROMIN
;   49: 	PLP
	PLP
;   50: 	JSR 	JMPTMP
	JSR 	JMPTMP
;   51: 	PHP
	PHP
;   52: 	BIT	LCBNK2
	BIT	LCBNK2
;   53: 	STA	REGVALS+0
	STA	REGVALS+0
;   54: 	STX	REGVALS+1
	STX	REGVALS+1
;   55: 	STY	REGVALS+2
	STY	REGVALS+2
;   56: 	PLA
	PLA
;   57: 	STA	REGVALS+3
	STA	REGVALS+3
;   58: 	LDX	TMP+2
	LDX	TMP+2
;   59: 	LDA 	#<REGVALS
	LDA 	#<REGVALS
;   60: 	LDY 	#>REGVALS
	LDY 	#>REGVALS
;   61: 	STA 	ESTKL,X
	STA 	ESTKL,X
;   62: 	STY 	ESTKH,X
	STY 	ESTKH,X
;   63: 	PLP
	PLP
;   64: 	RTS
	RTS
;   65: JMPTMP:	JMP	(TMP)
JMPTMP:	JMP	(TMP)
;   66: REGVALS: DS	4
REGVALS: DS	4
;   67: end
	RTS
;   68: ;
;   69: ; CHAR OUT
;   70: ; COUT(CHAR)
;   71: ;
;   72: asm cout
C0002:					; cout()
;   73: 	LDA	ESTKL,X
	LDA	ESTKL,X
;   74: 	INX
	INX
;   75: 	ORA	#$80
	ORA	#$80
;   76: 	BIT	ROMIN
	BIT	ROMIN
;   77: 	JSR	$FDED
	JSR	$FDED
;   78: 	BIT	LCBNK2
	BIT	LCBNK2
;   79: end
	RTS
;   80: ;
;   81: ; PRINT STRING
;   82: ; PRSTR(STR)
;   83: ;
;   84: asm prstr
C0004:					; prstr()
;   85: 	LDY	#$00
	LDY	#$00
;   86: 	LDA     ESTKL,X
	LDA     ESTKL,X
;   87:     STA     TMP
    STA     TMP
;   88: 	LDA     ESTKH,X
	LDA     ESTKH,X
;   89: 	STA     TMP+1
	STA     TMP+1
;   90: 	BIT	ROMIN
	BIT	ROMIN
;   91: 	LDA     (TMP),Y
	LDA     (TMP),Y
;   92: 	STA     ESTKL,X
	STA     ESTKL,X
;   93: 	BEQ     :+
	BEQ     :+
;   94: _PRS1:	INY
_PRS1:	INY
;   95: 	LDA	(TMP),Y
	LDA	(TMP),Y
;   96: 	ORA	#$80
	ORA	#$80
;   97: 	JSR	$FDED
	JSR	$FDED
;   98: 	TYA
	TYA
;   99: 	CMP	ESTKL,X
	CMP	ESTKL,X
;  100: 	BNE	_PRS1
	BNE	_PRS1
;  101: :	INX
:	INX
;  102: 	BIT	LCBNK2
	BIT	LCBNK2
;  103: end
	RTS
;  104: def textmode
C0006:					; textmode()
;  105: 	drop romcall(0, 0, 0, 0, $FB39)
	JSR	_INTERP
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$39,$FB		; CW	64313
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
;  106: end
	DB	$5C			; RET
;  107: def home
C0008:					; home()
;  108: 	drop romcall(0, 0, 0, 0, $FC58)
	JSR	_INTERP
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$58,$FC		; CW	64600
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
;  109: end
	DB	$5C			; RET
;  110: def gotoxy(x, y)
C0010:					; gotoxy()
					; x = 2
					; y = 4
;  111: 	^($24) = x
	JSR	_INTERP
	DB	$58,$06,$02		; ENTER	6,2
	DB	$2A,$24			; CB	36
	DB	$66,$02			; LLW	2
	DB	$70			; SB
;  112: 	drop romcall(y, 0, 0, 0, $FB5B)
	DB	$66,$04			; LLW	4
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$5B,$FB		; CW	64347
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
;  113: end
	DB	$5A			; LEAVE
;  114: def crout
C0012:					; crout()
;  115: 	cout($0D)
	JSR	_INTERP
	DB	$2A,$0D			; CB	13
	DB	$54,<C0002,>C0002	; CALL	C0002
;  116: end
	DB	$5C			; RET
;  117: def grmode
C0014:					; grmode()
;  118: 	drop romcall(0, 0, 0, 0, $FB40)
	JSR	_INTERP
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$00			; ZERO
	DB	$2C,$40,$FB		; CW	64320
	DB	$54,<C0000,>C0000	; CALL	C0000
	DB	$30			; DROP
;  119: 	drop ^showlores
	DB	$2C,$56,$C0		; CW	49238
	DB	$60			; LB
	DB	$30			; DROP
;  120: end
	DB	$5C			; RET
;  121: ; grcolor(color)
;  122: asm grcolor
C0016:					; grcolor()
;  123: 	LDA	ESTKL,X
	LDA	ESTKL,X
;  124: 	INX
	INX
;  125: 	STX	TMP+2
	STX	TMP+2
;  126: 	BIT	$C081
	BIT	$C081
;  127: 	JSR 	$F864
	JSR 	$F864
;  128: 	BIT	$C080
	BIT	$C080
;  129: 	LDX	TMP+2
	LDX	TMP+2
;  130: end
	RTS
;  131: ; grplot(x, y)
;  132: asm grplot
C0018:					; grplot()
;  133: 	LDA	ESTKL,X
	LDA	ESTKL,X
;  134: 	INX
	INX
;  135: 	LDY	ESTKL,X
	LDY	ESTKL,X
;  136: 	INX
	INX
;  137: 	STX	TMP+2
	STX	TMP+2
;  138: 	BIT	$C081
	BIT	$C081
;  139: 	JSR 	$F800
	JSR 	$F800
;  140: 	BIT	$C080
	BIT	$C080
;  141: 	LDX	TMP+2
	LDX	TMP+2
;  142: end
	RTS
;  143: defopt colors
C0020:					; colors()
;  144: 	while TRUE
C0022:
	DEX
	LDA	#$FF
	STA	ESTKL,X
	STA	ESTKH,X
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0023
:
;  145: 		for w = 3 to 50
	DEX
	LDA	#$03
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
C0025:
	LDA	ESTKL,X
	STA	D0071
	DEX
	LDA	#$32
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	LDA	ESTKH-1,X
	SBC	ESTKH,X
	BPL	:+
	JMP	C0024
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
;  146: 			for i = 1 to 19
	DEX
	LDA	#$01
	STA	ESTKL,X
	STY	ESTKH,X
C0027:
	LDA	ESTKL,X
	STA	D0068
	DEX
	LDA	#$13
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	LDA	ESTKH-1,X
	SBC	ESTKH,X
	BPL	:+
	JMP	C0026
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
;  147: 				for j = 0 to 19
	DEX
	STY	ESTKL,X
	STY	ESTKH,X
C0029:
	LDA	ESTKL,X
	STA	D0069
	DEX
	LDA	#$13
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	INX
	LDA	ESTKL-1,X
	CMP	ESTKL,X
	LDA	ESTKH-1,X
	SBC	ESTKH,X
	BPL	:+
	JMP	C0028
:
	INC	ESTKL,X
	BNE	:+
	INC	ESTKH,X
:
;  148: 					k = i + j
	DEX
	LDA	D0068
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	D0069
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	LDA	ESTKL,X
	STA	D0070
;  149: 					color = (j * 3) / (i + 3) + i * w / 12
	LDA	D0069
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	MUL
	DEX
	LDA	D0068
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	#$03
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ADD
	JSR	DIV
	DEX
	LDA	D0068
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0071
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	MUL
	DEX
	LDA	#$0C
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	DIV
	JSR	ADD
	LDA	ESTKL,X
	STA	D0074
;  150: 					fmi = 40 - i
	LDA	#$28
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0068
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0072
;  151: 					fmk = 40 - k
	LDA	#$28
	STA	ESTKL,X
	STY	ESTKH,X
	DEX
	LDA	D0070
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	SUB
	LDA	ESTKL,X
	STA	D0073
;  152: 					grcolor(color);
	LDA	D0074
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0016
;  153: 					grplot(i, k);
	DEX
	LDA	D0068
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0070
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  154: 					grplot(k, i);
	DEX
	LDA	D0070
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0068
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  155: 					grplot(fmi, fmk);
	DEX
	LDA	D0072
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0073
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  156: 					grplot(fmk, fmi);
	DEX
	LDA	D0073
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0072
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  157: 					grplot(k, fmi);
	DEX
	LDA	D0070
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0072
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  158: 					grplot(fmi, k);
	DEX
	LDA	D0072
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0070
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  159: 					grplot(i, fmk);
	DEX
	LDA	D0068
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0073
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  160: 					grplot(fmk, i);
	DEX
	LDA	D0073
	STA	ESTKL,X
	LDY	#$00
	STY	ESTKH,X
	DEX
	LDA	D0068
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	C0018
;  161: 					if ^keyboard >= 128
	DEX
	LDY	#$00
	STY	ESTKL,X
	LDA	#$C0
	STA	ESTKH,X
	JSR	LB
	DEX
	LDA	#$80
	STA	ESTKL,X
	STY	ESTKH,X
	JSR	ISGE
	INX
	LDA	ESTKL-1,X
	ORA	ESTKH-1,X
	BNE	:+
	JMP	C0030
:
;  162: 						drop ^keystrobe
	DEX
	LDA	#$10
	STA	ESTKL,X
	LDA	#$C0
	STA	ESTKH,X
	LDY	#$00
	JSR	LB
;  163: 						return
	INX
	INX
	RTS
;  164: 					fin
	INX
	INX
C0030:
C0031:
;  165: 				next
	JMP	C0029
C0028:
;  166: 			next
	INX
	JMP	C0027
C0026:
;  167: 		next
	INX
	JMP	C0025
C0024:
;  168: 	loop
	INX
	JMP	C0022
C0023:
;  169: end
	RTS
;  170: 
;  171: grmode()
START:	; JSR	INTERP
	DB	$54,<C0014,>C0014	; CALL	C0014
;  172: home()
	DB	$54,<C0008,>C0008	; CALL	C0008
;  173: gotoxy(10,22)
	DB	$2A,$0A			; CB	10
	DB	$2A,$16			; CB	22
	DB	$54,<C0010,>C0010	; CALL	C0010
;  174: prstr(@exitmsg)
	DB	$26,<D0000,>D0000	; LA	D0000
	DB	$54,<C0004,>C0004	; CALL	C0004
;  175: colors()
	DB	$54,<C0020,>C0020	; CALL	C0020
;  176: textmode()
	DB	$54,<C0006,>C0006	; CALL	C0006
;  177: home()
	DB	$54,<C0008,>C0008	; CALL	C0008
;  178: prstr(@goodbye)
	DB	$26,<D0023,>D0023	; LA	D0023
	DB	$54,<C0004,>C0004	; CALL	C0004
;  179: while ^keyboard < 128
C0032:
	DB	$2C,$00,$C0		; CW	49152
	DB	$60			; LB
	DB	$2A,$80			; CB	128
	DB	$46			; ISLT
	DB	$4C,<C0033,>C0033	; SKPFLS	C0033
;  180: loop
	DB	$50,<C0032,>C0032	; SKIP	C0032
C0033:
;  181: drop ^keystrobe
	DB	$2C,$10,$C0		; CW	49168
	DB	$60			; LB
	DB	$30			; DROP
;  182: done
	DB	$5C			; RET
