;*
;* DAVE'S VIRTUAL MACHINE BYTECODE INTERPRETER V. 1.0
;* STACK IS PUSHED MSB/LSB AND POPPED LSB/MSB
;*

;*
;* ZERO PAGE LOCATIONS
;*
;DVM_PC	=	$08	; PROGRAM COUNTER
;DVM_PTR	=	$0A	; MEMORY POINTER;
	.INCLUDE	"global.inc"

	.IMPORT	MEMSRC,MEMDST,MEMCPY,MEMCLR,KBWAIT
	.EXPORT	DVM, DVM_INIT

	.DATA
DVM_SP:	.BYTE	0
DVM_CALLSTACK:	.RES	6*2

	.CODE
.IFDEF	DEBUG_DVM
CHK_OP:	PHP
	CMP	#$6C
	BCC	:+
	PERR	"INVALID DVM OP"
	BRK
:	PLP
	RTS
.ENDIF
;*
;* LOAD OPS $00-$0F/$80-$8F
;*
; LDPDECB $06 (LoaD Pointer Dec Byte)
; LDPDECW $86 (LoaD Pointer Dec Word)
LDPDEC:	PHP
	LDA	#$01
	ADC	#$00
	STA	DVM_PTR
	LDA	(DVM_PC),Y
	TAX
	LDA	$00,X
	SEC
	SBC	DVM_PTR
	STA	$00,X
	STA	DVM_PTR
	LDA	$01,X
	SBC	#$00
	STA	$01,X
	STA	DVM_PTR+1
	PLP
	BCC	:++
	BCS	:+
; LDPINCB $05 (LoaD Pointer Inc Byte)
; LDPINCW $85 (LoaD Pointer Inc Word)
LDPINC:	PHP
	LDA	(DVM_PC),Y
	TAX
	LDA	$00,X
	STA	DVM_PTR
	ADC	#$01
	STA	$00,X
	LDA	$01,X
	STA	DVM_PTR+1
	ADC	#$00
	STA	$01,X
	PLP
	BCC	:++
:	LDA	(DVM_PTR),Y
	PHA
:	DEY
	LDA	(DVM_PTR),Y
	PHA
	LDA	#$02
	JMP	DVM_NEXTOPA
; LDPB $04 (LoaD Pointer Byte)
; LDPW $84 (LoaD Pointer Word)
LDP:	LDA	(DVM_PC),Y
	INY
	TAX
	LDA	$00,X
	STA	DVM_PTR
	LDA	$01,X
	STA	DVM_PTR+1
	LDA	(DVM_PC),Y
	TAY
	BCC	:+
	INY
	LDA	(DVM_PTR),Y
	PHA
	DEY
:	LDA	(DVM_PTR),Y
	PHA
	LDA	#$03
	JMP	DVM_NEXTOPA
; LDINDB $04 (LoaD INDirect Byte)
; LDINDW $84 (LoaD INDirect Word)
LDIND:	PLA
	STA	DVM_PTR
	PLA
	STA	DVM_PTR+1
	BCC	:+
	LDA	(DVM_PTR),Y
	PHA
:	DEY
	LDA	(DVM_PTR),Y
	INY
	PHA
	BNE	DVM_NEXTOP
; LDB $03  (LoaD Byte)
; LDW $83  (LoaD Word)
LD:	LDA	(DVM_PC),Y
	INY
	STA	DVM_PTR
	LDA	(DVM_PC),Y
	STA	DVM_PTR+1
	BCC	:+
	DEY
	LDA	(DVM_PTR),Y
	PHA
:	LDY	#$00
	LDA	(DVM_PTR),Y
	PHA
	LDA	#$03
	BNE	DVM_NEXTOPA
; LDZPB $02 (LoaD Zero Page Byte)
; LDZPW $82 (LoaD Zero Page Word)
LDZP:	LDA	(DVM_PC),Y
	TAX
	BCC	:+
	LDA	$01,X
	PHA
:	LDA	$00,X
	PHA
	INY
	BNE	DVM_NEXTOP
; LDCB $09 (LoaD Constant Byte)
; LDCW $89 (LoaD Constant Word) NOTE word is MSB FIRST!
LDC:	LDA	(DVM_PC),Y
	INY
	PHA
	BCC	DVM_NEXTOP
	LDA	(DVM_PC),Y
	INY
	PHA
	BCS	DVM_NEXTOP	
; LDM1B $08 (LoaD Minus 1 Byte)
; LDM1W $88 (Load Minus Word)
;LDM1:	LDA	#$FF
;	PHA
;	BCC	DVM_NEXTOP
;	PHA
;	BCS	DVM_NEXTOP
; LDCB $00-$07 (Load Literal Byte)
; LDCW $80-$87 (Load Literal Word)
LDL:	BCC	:+
	LDA	#$00
	PHA
:	TXA
	LSR			; CLEARS CARRY
	PHA
	BCC	DVM_NEXTOP
; POP2B $10 (POP 2 Bytes)
; POP2W $90 (POP 2 Words)
POP2:	PLA
	PLA
	BCC	DVM_NEXTOP
;	PLA
;	PLA
;	BCS	DVM_NEXTOP
; POPB $10 (POP Byte)
; POPW $90 (POP Word)
POP:	PLA
	BCC	DVM_NEXTOP
	PLA
	BCS	DVM_NEXTOP
; DUP2B $17 (DUPlicate 2 bytes) - same as DUPW
; DUP2W $97 (DUPlicate 2 Words)
DUP2:	TSX
	BCC	:+
	LDA	$0104,X
	PHA
	LDA	$0103,X
	PHA
	BCS	:+
; DUPB $0F (DUPlicate Byte)
; DUPW $8F (DUPlicate Word)
DUP:	TSX
	BCC	:++
:	LDA	$0102,X
	PHA
:	LDA	$0101,X
	PHA
;	JMP	DVM_NEXTOP
NOOP:
DVM_NEXTOP:	TYA
DVM_NEXTOPA:	CLC
	ADC	DVM_PC
	STA	DVM_PC
	BCC	DVM_DISPATCH
	INC	DVM_PC+1
DVM_DISPATCH:
	LDY	#$00
	LDA	(DVM_PC),Y
	INY
	ASL
.IFDEF	DEBUG_DVM
	JSR	CHK_OP
.ENDIF
	TAX
	LDA	DVM_OPTBL+1,X
	PHA
	LDA	DVM_OPTBL,X
	PHA
	RTS
;*
;* STORE OPS $08-$0F/$88-$8F
;*
; SWAPB $09 (SWAP Bytes)
; SWAPW $89 (SWAP Words)
SWAP:	TSX
	PLA		; LDA	$0101,X
	BCS	:+
	LDY	$0102,X
	STA	$0102,X
	TYA
	PHA		; STA	$0101,X
	LDA	#$01
	BNE	DVM_NEXTOPA
:	LDY	$0103,X
	STA	$0103,X
	TYA
	PHA		; STA	$0101,X
	LDY	$0104,X
	LDA	$0102,X
	STA	$0104,X
	TYA
	STA	$0102,X
	LDA	#$01
	BNE	DVM_NEXTOPA
; STZPW $0A (STore Zero Page Word)
; STZPB $8A (STore Zero Page Byte)
STZP:	LDA	(DVM_PC),Y
	INY
	TAX
	PLA
	STA	$00,X
	BCC	DVM_NEXTOP
	PLA
	STA	$01,X
	BCS	DVM_NEXTOP
; STB $0B  (STore Byte)
; STW $8B  (STore Word)
ST:	LDA	(DVM_PC),Y
	INY
	STA	DVM_PTR
	LDA	(DVM_PC),Y
	STA	DVM_PTR+1
	PLA
	LDY	#$00
	STA	(DVM_PTR),Y
	BCC	:+
	PLA
	INY
	STA	(DVM_PTR),Y
:	LDA	#$03
	BNE	DVM_NEXTOPA
; STINDB $0C (STore INDirect Byte)
; STINDW $8C (STore INDirect Word)
STIND:	PLA
	STA	DVM_PTR	
	PLA
	STA	DVM_PTR+1
	PLA
	DEY
	STA	(DVM_PTR),Y
	INY
	BCC	:+
	PLA
	STA	(DVM_PTR),Y
:	JMP	DVM_NEXTOP
; STPB $0C (STore Byte Pointer)
; STPW $8C (STore Word Pointer)
STP:	LDA	(DVM_PC),Y
	INY
	TAX
	LDA	$00,X
	STA	DVM_PTR
	LDA	$01,X
	STA	DVM_PTR+1
	LDA	(DVM_PC),Y
	TAY
	PLA
	STA	(DVM_PTR),Y
	BCC	:+
	INY
	PLA
	STA	(DVM_PTR),Y
:	LDA	#$03
	JMP	DVM_NEXTOPA
; STPINCB $0D (STore Pointer INC Byte)
; STPINCW $8D (STore Pointer INC Word)
STPINC:	PHP
	LDA	(DVM_PC),Y
	TAX
	LDA	$00,X
	STA	DVM_PTR
	ADC	#$01
	STA	$00,X
	LDA	$01,X
	STA	DVM_PTR+1
	ADC	#$00
	STA	$01,X
	PLP
:	PLA
	DEY
	STA	(DVM_PTR),Y
	BCC	:+
	PLA
	INY
	STA	(DVM_PTR),Y
:	LDA	#$02
	JMP	DVM_NEXTOPA
; STPDECB $0E (STore Pointer DEC Byte)
; STPDECW $8E (STore Pointer DEC Word)
STPDEC:	PHP
	LDA	#$01
	ADC	#$00
	STA	DVM_PTR
	LDA	(DVM_PC),Y
	TAX
	LDA	$00,X
	SEC
	SBC	DVM_PTR
	STA	$00,X
	STA	DVM_PTR
	LDA	$01,X
	SBC	#$00
	STA	$01,X
	STA	DVM_PTR+1
	PLP
	JMP	:--
;*
;* MATH OPS $10-$17/$90-$97
;*
; ZEXTB $10 (Zero EXTend Byte)
; SEXTB $90 (Sign EXTend Byte)
EXT:	PLA
	TAX
	BCC	:+
	BPL	:+
	LDA	#$FF
	BMI	:++
:	LDA	#$00
:	PHA
	TXA
	PHA
	JMP	DVM_NEXTOP
; NEGB $11 (NEGate Byte)
; NEGW $91 (NEGate Word)
NEG:	TSX
	BCS	:+
	SEC
	LDA	#$00
	SBC	$0101,X
	STA	$0101,X
	JMP	DVM_NEXTOP
:	LDA	#$00
	SBC	$0101,X
	STA	$0101,X
	LDA	#$00
	SBC	$0102,X
	STA	$0102,X
	JMP	DVM_NEXTOP
; NOTB $12 (NOT Byte)
; NOTW $92 (NOT Word)
NOT:	TSX
	LDA	$0101,X
	EOR	#$FF
	STA	$0101,X
	BCC	:+
	LDA	$0102,X
	EOR	#$FF
	STA	$0102,X
:	JMP	DVM_NEXTOP
; ADDB $13 (ADD Byte)
; ADDW $93 (ADD Word)
ADD:	TSX
	PLA
	BCS	:+
	ADC	$0102,X
	STA	$0102,X
	JMP	DVM_NEXTOP
:	CLC
	ADC	$0103,X
	STA	$0103,X
	PLA
	ADC	$0104,X
	STA	$0104,X
	JMP	DVM_NEXTOP
; SUBB $14 (SUBtract Byte)
; SUBW $94 (SUBtract Word)
SUB:	TSX
	BCS	:+
	SEC
	LDA	$0102,X
	SBC	$0101,X
	STA	$0102,X
	PLA
	JMP	DVM_NEXTOP
:	LDA	$0103,X
	SBC	$0101,X
	STA	$0103,X
	LDA	$0104,X
	SBC	$0102,X
	STA	$0104,X
	PLA
	PLA
	JMP	DVM_NEXTOP
; ANDB $15 (AND Bytes)
; ANDW $95 (AND Words)
L_AND:	TSX
	PLA
	BCS	:+
	AND	$0102,X
	STA	$0102,X
	JMP	DVM_NEXTOP
:	AND	$0103,X
	STA	$0103,X
	PLA
	AND	$0104,X
	STA	$0104,X
	JMP	DVM_NEXTOP
; ORB $16  (OR Bytes)
; ORW $96  (OR Words)
L_OR:	TSX
	PLA
	BCS	:+
	ORA	$0102,X
	STA	$0102,X
	JMP	DVM_NEXTOP
:	ORA	$0103,X
	STA	$0103,X
	PLA
	ORA	$0104,X
	STA	$0104,X
	JMP	DVM_NEXTOP
; XORB $17 (XOR Bytes)
; XORW $97 (XOR Words)
L_XOR:	TSX
	PLA
	BCS	:+
	EOR	$0102,X
	STA	$0102,X
	JMP	DVM_NEXTOP
:	EOR	$0103,X
	STA	$0103,X
	PLA
	EOR	$0104,X
	STA	$0104,X
	JMP	DVM_NEXTOP
; BRZB $18  (BRanch Zero Byte)
; BRZW $98  (BRanch Zero Word)
BRZ:	PLA
	BCC	:+
	STA	DVM_PTR
	PLA
	ORA	DVM_PTR
	BNE	:++
:	BEQ	BRNCH
:	INY
	JMP	DVM_NEXTOP
; BRNZB $19 (BRanch Not Zero Byte)
; BRNZW $99 (BRanch Not Zero Word)
BRNZ:	PLA
	BCC	:+
	STA	DVM_PTR
	PLA
	ORA	DVM_PTR
	BEQ	:++
:	BNE	BRNCH
:	INY
	JMP	DVM_NEXTOP
; BRPOSB $1A (BRanch Positive Byte)
; BRPOSW $9A (BRanch Positive Word)
BRPOS:	PLA
	BCC	:+
	PLA
:	BPL	BRNCH
	INY
	JMP	DVM_NEXTOP
; BRNEGB $1B (BRanch NEGative Byte)
; BRNEGW $9B (BRanch NEGative Word)
BRNEG:	PLA
	BCC	:+
	PLA
:	BMI	BRNCH
	INY
	JMP	DVM_NEXTOP
; BREQUB $1C (BRanch EQUal Bytes)              - branch if top bytes equal
; BREQUW $9C (BRanch EQUal Words)              - branch if top words equal
BREQU:	TSX
	PLA
	BCS	:+
	PLA
	CMP	$0101,X
	BNE	:++++
	BEQ	BRNCH
:	CMP	$0103,X
	BNE	:+
	PLA
	CMP	$0104,X
	BEQ	:+++++++
	BNE	:++
:	PLA
:	PLA
	PLA
:	INY
	JMP	DVM_NEXTOP
; BRNEQB $1D (BRanch Not EQual Bytes)          - branch if top bytes not equal
; BRNEQW $9D (BRanch Not EQual Words)          - branch if top words not equal
BRNEQ:	TSX
	PLA
	BCS	:+
	PLA
	CMP	$0101,X
	BEQ	:++
	BNE	BRNCH
:	CMP	$0103,X
	BNE	:++
	PLA
	CMP	$0104,X
	BNE	:+++
	PLA
	PLA
:	INY
	JMP	DVM_NEXTOP
:	PLA
:	PLA
	PLA
; BRNCH $22 (BRaNCH)
BRNCH:	LDX	#$00
	LDA	(DVM_PC),Y
	BPL	:+
	LDX	#$FF
:	CLC
	ADC	DVM_PC
	STA	DVM_PC
	TXA
	ADC	DVM_PC+1
	STA	DVM_PC+1
	JMP	DVM_DISPATCH
; BRGTB $1E (BRanch Greater Than Bytes)
; BRGTW $9E (BRanch Greater Than Words)
; TOS-1 > TOS -> TOS < TOS-1
BRGT:	TSX
	PLA
	BCS	:++
	SEC
	SBC	$0102,X
	BVC	:+
	EOR	#$80
:	BMI	:++++
	PLA
	LDA	#$03
	JMP	DVM_NEXTOPA
:	SBC	$0103,X
	PLA
	SBC	$0104,X
	BVC	:+
	EOR	#$80
:	BMI	:+
	PLA
	PLA
	INY
	JMP	DVM_NEXTOP
:	PLA
:	PLA
	JMP	BRNCH
; BRLEB $1F (BRanch Less than or Equal Bytes)
; BRLEW $9F (BRanch Less than or Equal Words)
; TOS-1 <= TOS -> TOS >= TOS-1
BRLE:	TSX
	PLA
	BCS	:++
	SEC
	SBC	$0102,X
	BVC	:+
	EOR	#$80
:	BPL	:--
	PLA
	INY
	JMP	DVM_NEXTOP
:	SBC	$0103,X
	PLA
	SBC	$0104,X
	BVC	:+
	EOR	#$80
:	BPL	:-----
	PLA
	PLA
	INY
	JMP	DVM_NEXTOP
; BRAB $22 (BRanch Above Bytes)
; BRAW $A2 (BRanch Above Than Words)
; (UNSIGNED) TOS-1 > TOS -> TOS < TOS-1
BRA:	TSX
	PLA
	BCS	:+
	CMP	$0102,X
	BCC	:+++
	PLA
	INY
	JMP	DVM_NEXTOP
:	SBC	$0103,X
	PLA
	SBC	$0104,X
	BCC	:+
	PLA
	PLA
	INY
	JMP	DVM_NEXTOP
:	PLA
:	PLA
	JMP	BRNCH
; BRBEB $23 (BRanch Below or Equal Bytes)
; BRBEW $A3 (BRanch Below or Equal Words)
; (UNSIGNED) TOS-1 <= TOS -> TOS >= TOS-1
BRBE:	TSX
	PLA
	BCS	:+
	CMP	$0102,X
	BCS	:-
	PLA
	INY
	JMP	DVM_NEXTOP
:	SBC	$0103,X
	PLA
	SBC	$0104,X
	BCS	:---
	PLA
	PLA
	INY
	JMP	DVM_NEXTOP
; SHLB $28 (SHift Left Byte)
; SHLW $A8 (SHift Left Word)
SHL:	TSX
	BCS	:+
	ASL	$0101,X
	JMP	DVM_NEXTOP
:	ASL	$0101,X
	ROL	$0102,X
	JMP	DVM_NEXTOP
; SHRB $29 (SHift Right Byte)
; SHRW $A9 (SHift Right Word)
SHR:	TSX
	BCS	:+
	LSR	$0101,X
	JMP	DVM_NEXTOP
:	LSR	$0102,X
	ROR	$0101,X
	JMP	DVM_NEXTOP
; INCRB $28 (INCRement Byte)
; INCRW $A8 (INCRement Word)
INCR:	TSX
	INC	$0101,X
	BCS	:+
	JMP	DVM_NEXTOP
:	BNE	:+
	INC	$0102,X
:	JMP	DVM_NEXTOP
; DECRB $28 (DECRement Byte)
; DECRW $A8 (DECRement Word)
DECR:	TSX
	DEC	$0101,X
	BCS	:+
	JMP	DVM_NEXTOP
:	LDA	$0101,X
	CMP	#$FF
	BNE	:+
	DEC	$0102,X
:	JMP	DVM_NEXTOP
; SWITCHB $35 (SWiTCH Byte)
; SWITCHW $B5 (SWiTCH Word)
SWTCH:	BCS	SWTCHW
	LDA	(DVM_PC),Y
	INY
	TAX
	PLA
	STA	DVM_PTR
SWTCHBLP:	LDA	(DVM_PC),Y
	INY
	CMP	DVM_PTR
	BNE	:++
	LDX	DVM_PC+1
	TYA
	CLC
	ADC	DVM_PC
	BCC	:+
	INX
	CLC
:	ADC	(DVM_PC),Y
	INY
	PHA
	TXA
	ADC	(DVM_PC),Y
	STA	DVM_PC+1
	PLA
	STA	DVM_PC
	LDA	#$00
	JMP	DVM_NEXTOPA
:	INY
	INY
	DEX
	BNE	SWTCHBLP
	JMP	DVM_NEXTOP
SWTCHW:	LDA	(DVM_PC),Y
	INY
	TAX
	PLA
	STA	DVM_PTR
	PLA
	STA	DVM_PTR+1
SWTCHWLP:	LDA	(DVM_PC),Y
	INY
	CMP	DVM_PTR
	BNE	:++
	LDA	(DVM_PC),Y
	INY
	CMP	DVM_PTR+1
	BNE	:+++
	LDX	DVM_PC+1
	TYA
	CLC
	ADC	DVM_PC
	BCC	:+
	INX
	CLC
:	ADC	(DVM_PC),Y
	INY
	PHA
	TXA
	ADC	(DVM_PC),Y
	STA	DVM_PC+1
	PLA
	STA	DVM_PC
	LDA	#$00
	JMP	DVM_NEXTOPA
:	INY
:	INY
	INY
	DEX
	BNE	SWTCHWLP
	JMP	DVM_NEXTOP
; EXIT $28 (EXIT)
EXIT:	LDA	DVM_PC+1
	PHA
	LDA	DVM_PC
	PHA
	RTS
;*
;* ENTRYPOINT INTO DVM
;*
DVM:	LDY	#$01
	BNE	:+
; JUMP $29 (JUMP)
JUMP:	BCS	JUMPIND
	LDA	(DVM_PC),Y
	INY
	ADC	DVM_PC
	TAX
	LDA	(DVM_PC),Y
	ADC	DVM_PC+1
	PHA
	TXA
	PHA
; JUMP $A9 (JUMP INDirect)
JUMPIND:	LDY	#$00
:	PLA
	STA	DVM_PC
	PLA
	STA	DVM_PC+1
	JMP	DVM_NEXTOP
; DECJNZB $26 (DECrement memory Jump Not Zero Byte)
; DECJNZW $A6 (DECrement memory Jump Not Zero Word)
DECJNZ:	LDA	(DVM_PC),Y
	INY
	STA	DVM_PTR
	LDA	(DVM_PC),Y
	STA	DVM_PTR+1
	LDY	#$00
	LDA	(DVM_PTR),Y
	BCS	:++
	ADC	#$FF
	STA	(DVM_PTR),Y
	BEQ	:+++
:	LDY	#$03
	CLC
	BCC	JUMP
:	SBC	#$01
	STA	(DVM_PTR),Y
	INY
	LDA	(DVM_PTR),Y
	SBC	#$00
	STA	(DVM_PTR),Y
	DEY
	ORA	(DVM_PTR),Y
	BNE	:--
:	LDA	#$05
	JMP	DVM_NEXTOPA
; CALL $2A (CALL)
CALL:	BCS	CALLIND
	LDA	(DVM_PC),Y
	INY
	ADC	DVM_PC
	TAX
	LDA	(DVM_PC),Y
	INY
	ADC	DVM_PC+1
	PHA
	TXA
	PHA
; CALL $AA (CALL INDirect)
CALLIND:	LDX	DVM_SP
	TYA
	CLC
	ADC	DVM_PC
	STA	DVM_CALLSTACK,X
	INX
	LDA	#$00
	ADC	DVM_PC+1
	STA	DVM_CALLSTACK,X
	INX
	STX	DVM_SP
	BNE	JUMPIND
; RET  $2B (RETurn)
RET:	LDX	DVM_SP
	DEX
	LDA	DVM_CALLSTACK,X
	STA	DVM_PC+1
	DEX
	LDA	DVM_CALLSTACK,X
	STA	DVM_PC
	STX	DVM_SP
	LDA	#$00
	JMP	DVM_NEXTOPA
; CALL_02 $34 (CALL 6502)
CALL_02:	BCS	CALLIND_02
	LDA	(DVM_PC),Y
	INY
	TAX
	LDA	(DVM_PC),Y
	INY
	PHA
	TXA
	PHA
; CALLIND_02 $B4 (CALL INDirect 6502)
CALLIND_02:	TYA
	CLC
	ADC	DVM_PC
	STA	DVM_PC
	BCC	:+
	INC	DVM_PC+1
:	PLA
	STA	JSRADDR+1		; BAD, SELF MODIFYING CODE
	PLA
	STA	JSRADDR+2
	PLA
	STA	DVM_PTR
	PLA
	TAX
	PLA
	TAY
	PLP
	LDA	DVM_PTR
JSRADDR:	JSR	$0000
	PHP
	STA	DVM_PTR
	TYA
	PHA
	TXA
	PHA
	LDA	DVM_PTR
	PHA
	JMP	DVM_DISPATCH

	.SEGMENT	"INIT"
DVM_INIT:	LDA	#$EA		; NOP OPCODE
	STA	ENTER_DVM-1
	LDA	#$4C		; JUMP OPCODE
	STA	ENTER_DVM
	LDA	#<DVM
	STA	LINK_DVM
	LDA	#>DVM
	STA	LINK_DVM+1
	LDA	#<DVMOPTBL_RELOC
	LDX	#>DVMOPTBL_RELOC
	JSR	MEMSRC
	LDA	#<DVM_OPTBL
	LDX	#>DVM_OPTBL
	JSR	MEMDST
	LDA	#$6C
	LDX	#$00
	JSR	MEMCPY		; RELOCATE TABLE TO LOW MEMORY
	RTS
DVMOPTBL_RELOC:
	.ORG	$290

DVM_OPTBL:	.ADDR	LDL-1,  LDL-1,  LDL-1,   LDL-1,    LDL-1,     LDL-1,    DUP-1,    DUP2-1	; $00-$07
	.ADDR	SWAP-1, LDC-1,  LDZP-1,  LD-1,     LDP-1,     LDPINC-1, LDPDEC-1, LDIND-1	; $08-$0F
	.ADDR	POP-1,  POP2-1, STZP-1,  ST-1,     STP-1,     STPINC-1, STPDEC-1, STIND-1	; $10-$17
	.ADDR	EXT-1,  NEG-1,  NOT-1,   ADD-1,    SUB-1,     L_AND-1,  L_OR-1,   L_XOR-1	; $18-$1F
	.ADDR	BRZ-1,  BRNZ-1, BRPOS-1, BRNEG-1,  BREQU-1,   BRNEQ-1,  BRGT-1,   BRLE-1	; $20-$27
	.ADDR	BRA-1,  BRBE-1, BRNCH-1, DECJNZ-1, SHL-1,     SHR-1,    INCR-1,   DECR-1	; $28-$2F
	.ADDR	EXIT-1, JUMP-1, CALL-1,  RET-1,    CALL_02-1, SWTCH-1		; $30-$37