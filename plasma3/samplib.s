;
; Sample PLASMA library.
;
!TO "samplib.bin", PLAIN
* = $1000
;
; DATA/CODE SEGMENT
;
_SEGBEGIN
	!WORD	_SEGEND-_SEGBEGIN	; LENGTH OF HEADER + CODE/DATA + BYTECODE SEGMENT
;
; MODULE HEADER
;
	!WORD	$DA7E		; MAGIC #
	!WORD	_SUBSEG		; BYTECODE SUB-SEGMENT
;
; MODULE DEPENDENCY LIST
; NOTE: DCI = PSUEDO OP FOR ASCII STRING WITH HI BIT SET EXCEPT LAST CHAR
;
	;DCI	"STDLIB"
	!CT		"hi.ascii"
	!TX		"STDLI"
	!CT		RAW
	!TX		'B'
	;DCI	"FILEIO"
	!CT		"hi.ascii"
	!TX		"FILEI"
	!CT		RAW
	!TX		'O'
	!BYTE	0
;
; NATIVE CODE + GLOBAL DATA
;
COUNT	!WORD	0
INCCNT
FIXUP1	INC	COUNT
	BNE	XINIT
FIXUP2	INC	COUNT+1
XINIT	RTS
;
; BYTECODE SUB-SEGMENT
;
_SUBSEG
MYFUNC	!BYTE	$58, $01, $16	; ENTER	1,16
	!BYTE	$66, $02	; LLW	2
	!BYTE	$2A, $01	; CB	1
	!BYTE	$54		; CALL	EXTERN(1) "OPEN"
FIXUP4	!WORD	$0000
	!BYTE	$6E, $04	; DLW	4
	!BYTE	$54		; CALL	EXTERN(3) "READ"
FIXUP5	!WORD	$0000
	!BYTE	$30		; DROP
	!BYTE	$66, $04	; LLW	4
	!BYTE	$54		; CALL	EXTERN(2)	; "CLOSE"
FIXUP6	!WORD	$0000
	!BYTE	$30		; DROP
	!BYTE	$6A		; LAW	COUNT
FIXUP7	!WORD	$0000
	!BYTE	$54		; CALL	INCNT
FIXUP8	!WORD	$0000
	!BYTE   $5A		; LEAVE
;
; END OF CODE/DATA + BYTECODE SEGMENT
;
_SEGEND
;
; BYTCODE FUNCTION DICTIONARY
;
	!BYTE	$A1		; FIXUP FLAGS
	!WORD	MYFUNC		; FIXUP OFFSET
	!BYTE	$00		; FIXUP LO BYTE (OF HI BYTE)/IMPORT INDEX
;
; RE-LOCATION DICTIONARY (FIXUP TABLE)
;
	!BYTE	$81		; FIXUP FLAGS
	!WORD	FIXUP1+1	; FIXUP OFFSET
	!BYTE	$00		; FIXUP LO BYTE (OF HI BYTE)/IMPORT INDEX
	!BYTE	$81
	!WORD	FIXUP2+1
	!BYTE	$00
	!BYTE	$91		; IMPORT FIXUP
	!WORD	FIXUP4
	!BYTE	$01	      	; IMPORT INDEX 1
	!BYTE	$91
	!WORD	FIXUP5
	!BYTE	$03
	!BYTE	$91
	!WORD	FIXUP6
	!BYTE	$02
	!BYTE	$81
	!WORD	FIXUP7
	!BYTE	$00
	!BYTE	$81
	!WORD	FIXUP8
	!BYTE	$00
	!BYTE	0		; END OF RLD
;
; EXTERNAL/ENTRY SYMBOL DIRECTORY
;;
; IMPORT TABLE
;
IMPTBL	;DCI	"OPEN"	; EXTERNAL SYMBOL NAME
	!CT "hi.ascii"
	!TX "OPE"
	!CT RAW
	!TX 'N'
	!BYTE	$10		; EXTERNAL SYMBOL FLAG
	!WORD	1		; SYMBOL INDEX
	;DCI	"CLOSE"
	!CT "hi.ascii"
	!TX "CLOS"
	!CT RAW
	!TX 'E'
	!BYTE	$10
	!WORD	2
	;DCI	"READ"
	!CT "hi.ascii"
	!TX "REA"
	!CT RAW
	!TX 'D'
	!BYTE	$10
	!WORD	3
	;DCI	"MEMSET"
	!CT "hi.ascii"
	!TX "MEMSE"
	!CT RAW
	!TX 'T'
	!BYTE	$10
	!WORD	4
;
; EXPORT TABLE
;
EXPTBL	;DCI	"INCNT"	; ENTRY SYMBOL NAME
	!CT "hi.ascii"
	!TX "INCN"
	!CT RAW
	!TX 'T'
	!BYTE	$08		; ENTRY SYMBOL FLAG
	!WORD	INCCNT		; OFFSET
	;DCI	"MYFUNC"
	!CT "hi.ascii"
	!TX "MYFUN"
	!CT RAW
	!TX 'C'
	!BYTE	$08
	!WORD	MYFUNC
	!BYTE	0		; END OF ESD
