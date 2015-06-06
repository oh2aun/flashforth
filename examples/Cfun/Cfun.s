; Calling user's C function
; IgorM (c) 3.6.2015
;
; XC16 compiler doc says:
; 	. the caller shall save/restore the W0-W7
; 	. registers W8 to W14 are callee saved, W14 will be used as a frame pointer
; 	. function arguments are passed in registers W0-W7 (left to right)
; 	. function return value is in W0-W3
;
; What fits into W0-W7 arguuments and W0-W1 returned (examples):
; 	float Ctest( int a, int b, long int c, float x, float y)
; 	long int Ctest( int a, int b, long int c, long int d, long int e)
;
; Ctest ( a b c x y -- Ctest() )
;
; Place this Cfun.s into ../src/ folder
;


	.pword  paddr(9b)+PFLASH
; Ctest
9:
	.byte   NFA|5
	.ascii  "Ctest"
	.align  2
CCTEST_:
	push	W4
	push	W5
	push	W6
	push 	W7
	mov     [W14--], W7
	mov     [W14--], W6	;
	mov     [W14--], W5
	mov     [W14--], W4	;
	mov     [W14--], W3
	mov     [W14--], W2	;
	mov     [W14--], W1	;
	mov     [W14], W0	;
	.extern _Ctest
	rcall  _Ctest
	mov     W0, [W14++]
	mov     W1, [W14]	; float return y
	pop	W7
	pop	W6
	pop	W5
	pop	W4
	return
