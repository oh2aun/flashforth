; Distance and Bearing demo
; IgorM c 6/2015
;

	.pword  paddr(9b)+PFLASH
; Cdist
9:
	.byte   NFA|4
	.ascii  "dist"
	.align  2
CDIST_:
	push	W0
	push	W1
	push	W2
	push 	W3
	push	W4
	push	W5
	push	W6
	push 	W7
	mov     [W14--], W7
	mov     [W14--], W6
	mov     [W14--], W5
	mov     [W14--], W4
	mov     [W14--], W3
	mov     [W14--], W2
	mov     [W14--], W1
	mov     [W14], W0
	.extern _Cdist
	call  _Cdist
	mov     W0, [W14++]
	mov     W1, [W14]   ; returns float
	pop     W7
	pop     W6
	pop     W5
	pop     W4
	pop     W3
	pop     W2
	pop     W1
	pop     W0
	return

	.pword  paddr(9b)+PFLASH
; Cbear
9:
	.byte   NFA|4
	.ascii  "bear"
	.align  2
CBEAR_:
	push	W0
	push	W1
	push	W2
	push 	W3
	push	W4
	push	W5
	push	W6
	push 	W7
	mov     [W14--], W7
	mov     [W14--], W6
	mov     [W14--], W5
	mov     [W14--], W4
	mov     [W14--], W3
	mov     [W14--], W2
	mov     [W14--], W1
	mov     [W14], W0
	.extern _Cbear
	call  _Cbear
	mov     W0, [W14++]
	mov     W1, [W14]   ; returns float
	pop     W7
	pop     W6
	pop     W5
	pop     W4
	pop     W3
	pop     W2
	pop     W1
	pop     W0
	return
