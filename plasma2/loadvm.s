.PC02
.DEFINE	EQU	=
.DEFINE	DB	.BYTE
.DEFINE	DW	.WORD
.DEFINE	DS	.RES
;*
;* LANGUAGE CARD CONTROL
;*
LCBNK2	EQU	$C080
ROMIN	EQU	$C081
;*
;* PRODOS
;*
PRODOS  EQU     $BF00
;*
;* LOAD VMCORE FILE
;*
;LOADVM:
	LDA	#$00
	INC			; ONLY WORKS ON 65C02
	CMP	#$01
	BEQ	LDVMC02
LDVM02:	LDA	#<VMCORE
	STA	$06
	LDA	#>VMCORE
	STA	$07
	BNE	:+
LDVMC02:	LDA	#<VMCOREC
	STA	$06
	LDA	#>VMCOREC
	STA	$07
;*
;* MOVE VM INTO LANGUAGE CARD
;*
:	LDA	#$00
	STA	$08
	LDA	#$D0
	STA	$09
	LDY	#$00
	BIT	$C083		; SELECT AND WE LC BANK 2
	BIT	$C083
MVVM:	LDA	($06),Y         ; COPY VM+CMD INTO LANGUAGE CARD
	STA	($08),Y
	INY
	BNE	MVVM
	INC	$07
	INC	$09
	LDA	$09
	CMP	#$E0
	BNE	MVVM
	LDX     #$FE
	TXS
	LDX     #$00
        STX     $01FF
;*
;* LOOK FOR STARTUP FILE
;*
        JSR     PRODOS          ; OPEN AUTORUN
        DB      $C8
        DW      OPENPARMS
        BCC     :+
        JMP     EXIT
:       LDA     REFNUM
        STA     NLPARMS+1
        JSR     PRODOS
        DB      $C9
        DW      NLPARMS
        BCC     :+
        JMP     EXIT
:       LDA     REFNUM
        STA     READPARMS+1
        JSR     PRODOS
        DB      $CA
        DW      READPARMS
        BCC     :+
        JMP     EXIT
:       LDX     READPARMS+6
        STX     $01FF
EXIT:   JSR     PRODOS
        DB      $CC
        DW      CLOSEPARMS        
        LDY     #$00
        STY     $06
	LDA	#$D1
	STA	$07
LDVM:	LDA	($06),Y         ; LOAD FIRST PAGE OF CMD INTO PLACE
	STA	$1000,Y
	INY
	BNE	LDVM
	LDA     #$7F
	ADC     #$01            ; SET V FLAG
        JMP     $1007           ; CALL CMD
;EXIT:   JSR     PRODOS
;        DB      $65
;        DW      EXITPARMS
;EXITPARMS: DB   4
;        DB      0
AUTORUN: DB     7,"AUTORUN"
OPENPARMS: DB   3
        DW      AUTORUN
        DW      $0800
REFNUM: DB      0
NLPARMS: DB     3
        DB      0
        DB      $7F
        DB      $0D
READPARMS: DB   4
        DB      0
        DW      $0200
        DW      $0080
        DW      0
CLOSEPARMS: DB  1
        DB      0
VMCORE:
	.INCLUDE	"vmcore.byte"
VMCOREC:
	.INCLUDE	"vmcorec.byte"
