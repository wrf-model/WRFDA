FSLEN	SEGMENT  BYTE PUBLIC 'CODE'
FSLEN	ENDS
_DATA	SEGMENT  WORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS
DGROUP	GROUP	CONST, _BSS, _DATA
	ASSUME  CS: FSLEN, DS: DGROUP, SS: DGROUP, ES: DGROUP

EXTRN	__FCclenv:DWORD

FSLEN   SEGMENT
		PUBLIC	_fslen
_fslen	PROC FAR
		push	bp
		mov		bp,sp

		shl 	WORD PTR [bp+6],1		; desired string # multiplied by 2
		mov		ax,WORD PTR __FCclenv
		mov		dx,WORD PTR __FCclenv+2
		mov		WORD PTR [bp-4],ax
		mov		WORD PTR [bp-2],dx
		les		bx,DWORD PTR [bp-4]
		add		bx,WORD PTR [bp+6]		; offset address of string 'n'
		mov		ax,WORD PTR es:[bx]		; length of string
		cwd

		mov		sp,bp
		pop		bp
		ret

_fslen	ENDP
FSLEN	ENDS
		END
