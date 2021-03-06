;*
;* JAVA UTILITIES FOR 6502
;*
	.INCLUDE	"global.inc"
	.EXPORT	UTIL_INIT,MEMSRC,MEMDST,MEMCPY,MEMCLR,MEMSET
	.EXPORT	MUL5,MUL9,MUL10

	.SEGMENT "INIT"
UTIL_INIT:	LDA	#<MEMSRC
	STA	LINK_MEMSRC
	LDA	#>MEMSRC
	STA	LINK_MEMSRC+1
	LDA	#<MEMDST
	STA	LINK_MEMDST
	LDA	#>MEMDST
	STA	LINK_MEMDST+1
	LDA	#<MEMCPY
	STA	LINK_MEMCPY
	LDA	#>MEMCPY
	STA	LINK_MEMCPY+1
	LDA	#<MEMCLR
	STA	LINK_MEMCLR
	LDA	#>MEMCLR
	STA	LINK_MEMCLR+1
	LDA	#<MEMSET
	STA	LINK_MEMSET
	LDA	#>MEMSET
	STA	LINK_MEMSET+1
	RTS
	
	.CODE
;*
;* SET MEMORY SRC OPERAND
;* ENTRY: AX = ADDRESS
;*
MEMSRC:	STA	SRCADDR
	STX	SRCADDR+1
	RTS
;*
;* SET MEMORY DST OPERAND
;* ENTRY: AX = ADDRESS
;*
MEMDST:	STA	DSTADDR
	STX	DSTADDR+1
	RTS
;*
;* COPY MEMORY
;*
;* ENTRY: SRCADDR = SOURCE ADDRESS
;*        DSTADDR = DESTINATION ADDRESS
;*             AX = LENGTH IN BYTES
;*
MEMCPY:	TAY
	LDA	SRCADDR+1
	CMP	DSTADDR+1
	BCC	REVCPY
	BNE	:+
	LDA	SRCADDR
	CMP	DSTADDR
:	BCS	FORCPY
REVCPY:	TXA			; REVERSE DIRECTION COPY
;	CLC
	ADC	SRCADDR+1
	STA	SRCADDR+1
	TXA
	CLC
	ADC	DSTADDR+1
	STA	DSTADDR+1
	INX
	CPY	#$00
	BEQ	:++
	DEY
	BEQ	:+
REVCPYLOOP:	LDA	(SRCADDR),Y
	STA	(DSTADDR),Y
	DEY
	BNE	REVCPYLOOP
:	LDA	(SRCADDR),Y		; DO ONE MORE COPY, Y = #$00
	STA	(DSTADDR),Y		; (THIS MAKES FOR A SLIGHTLY FASTER INNER LOOP)
:	DEY			; NOW Y = #$FF
	DEX
	BEQ	:+
	DEC	SRCADDR+1
	DEC	DSTADDR+1
	BNE	REVCPYLOOP
:	RTS
FORCPY:	TYA			; FORWARD DIRECTION COPY
	EOR	#$FF
	TAY
	INY
	BNE	:+
	DEX
:	LDA	SRCADDR
	STY	SRCADDR
;	SEC
	SBC	SRCADDR
	STA	SRCADDR
	LDA	SRCADDR+1
	SBC	#$00
	STA	SRCADDR+1
	LDA	DSTADDR
	STY	DSTADDR
	SEC
	SBC	DSTADDR
	STA	DSTADDR
	LDA	DSTADDR+1
	SBC	#$00
	STA	DSTADDR+1
	INX
FORCPYLOOP:	LDA	(SRCADDR),Y
	STA	(DSTADDR),Y
	INY
	BNE	FORCPYLOOP
	DEX
	BEQ	:+
	INC	SRCADDR+1
	INC	DSTADDR+1
	BNE	FORCPYLOOP
:	RTS
;*
;* CLEAR MEMORY
;*
;* ENTRY: DSTADDR = DESTINATION ADDRESS TO CLEAR
;*             AX = LENGTH IN BYTES
;*
MEMCLR:	LDY	#$00
;*
;* SET MEMORY
;*
;* ENTRY: DSTADDR = DESTINATION ADDRESS TO SET
;*             AX = LENGTH IN BYTES
;*              Y = VALUE TO SET
;*
MEMSET:	STY	TMP
	CPX	#$00
	BEQ	SMALLSET
	STA	TMP+1
	LDY	#$00
	LDA	TMP
:	STA	(DSTADDR),Y
	INY
	BNE	:-
	INC	DSTADDR+1
	DEX
	BNE	:-
	LDA	TMP+1
SMALLSET:	TAY
	BEQ	SETDONE
	LDA	TMP
:	DEY
	STA	(DSTADDR),Y
	BNE	:-
SETDONE:	RTS

;*
;* MULTIPLY BY 5 - TURN INDEX INTO CONSTANT POOL INTO OFFSET
;* ENTRY: AX = VALUE
;* EXIT:  AX = VALUE * 5
;*         C = OVERFLOW
;*
MUL5:	STA	TMP
	STX	TMP+1
	ASL
	ROL	TMP+1
	ASL
	ROL	TMP+1
	ADC	TMP
	PHA
	TXA
	ADC	TMP+1
	TAX
	PLA
	RTS
;*
;* MULTIPLY BY 7 - TURN INDEX INTO FIELD TABLE INTO OFFSET
;* ENTRY: AX = VALUE
;* EXIT:  AX = VALUE * 7
;*         C = OVERFLOW
;*
;MUL7:	STA	TMP
;	STX	TMP+1
;	ASL	TMP
;	ROL	TMP+1
;	ADC	TMP
;	PHA
;	TXA
;	ADC	TMP+1
;	TAX
;	PLA
;	ASL	TMP
;	ROL	TMP+1
;	ADC	TMP
;	PHA
;	TXA
;	ADC	TMP+1
;	TAX
;	PLA
;	RTS
;*
;* MULTIPLY BY 9 - TURN INDEX INTO FIELD TABLE INTO OFFSET
;* ENTRY: AX = VALUE
;* EXIT:  AX = VALUE * 9
;*         C = OVERFLOW
;*
MUL9:	STA	TMP
	STX	TMP+1
	ASL
	ROL	TMP+1
	ASL
	ROL	TMP+1
	ASL
	ROL	TMP+1
	ADC	TMP
	PHA
	TXA
	ADC	TMP+1
	TAX
	PLA
	RTS
;*
;* MULTIPLY BY 10 - TURN INDEX INTO METHOD TABLE INTO OFFSET
;* ENTRY: AX = VALUE
;* EXIT:  AX = VALUE * 10
;*         C = OVERFLOW
;*
MUL10:	STA	TMP		; Y = X + 4X = 5X
	STX	TMP+1
	ASL
	ROL	TMP+1
	ASL
	ROL	TMP+1
	ADC	TMP
	STA	TMP
	TXA
	ADC	TMP+1
	ASL	TMP		; RETURN Y * 4 = 5X * 2 = 10X
	ROL
	TAX
	LDA	TMP
	RTS
;*
;* MULTIPLY BY 20 - TURN INDEX INTO METHOD TABLE INTO OFFSET
;* ENTRY: AX = VALUE
;* EXIT:  AX = VALUE * 20
;*         C = OVERFLOW
;*
;MUL20:	STA	TMP		; Y = X + 4X = 5X
;	STX	TMP+1
;	ASL
;	ROL	TMP+1
;	ASL
;	ROL	TMP+1
;	ADC	TMP
;	STA	TMP
;	TXA
;	ADC	TMP+1
;	ASL	TMP		; RETURN Y * 4 = 5X * 4 = 20X
;	ROL
;	ASL	TMP
;	ROL
;	TAX
;	LDA	TMP
;	RTS
