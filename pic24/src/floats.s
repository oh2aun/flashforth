; Calling directly the math library assembly entry points
; Mikael Nordman 14.5.2015

        .pword  paddr(9b)+PFLASH
; float add
9:
        .byte   NFA|2
        .ascii  "f+"
        .align  2
CFADD_:
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14], W0
        .extern ___addsf3
        rcall    ___addsf3
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float sub
9:
        .byte   NFA|2
        .ascii  "f-"
        .align  2
CFSUB_:
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14], W0
        .extern ___subsf3
        rcall    ___subsf3
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float mul
9:
        .byte   NFA|2
        .ascii  "f*"
        .align  2
CFMUL_:
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14], W0
        .extern ___mulsf3
        rcall    ___mulsf3
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float div
9:
        .byte   NFA|2
        .ascii  "f/"
        .align  2
CFDIV_:
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14], W0
        .extern ___divsf3
        rcall    ___divsf3
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float pow
9:
        .byte   NFA|4
        .ascii  "fpow"
        .align  2
CFPOW_:
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14], W0
        .extern _powf
        rcall    _powf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH		
; float sin
9:
        .byte   NFA|4
        .ascii  "fsin"
        .align  2
CFSIN_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _sinf
        rcall    _sinf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float cos
9:
        .byte   NFA|4
        .ascii  "fcos"
        .align  2
CFCOS_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _cosf
        rcall    _cosf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float tan
9:
        .byte   NFA|4
        .ascii  "ftan"
        .align  2
CFTAN_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _tanf
        rcall    _tanf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float sqrt
9:
        .byte   NFA|5
        .ascii  "fsqrt"
        .align  2
CFSQRT_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _sqrtf
        rcall    _sqrtf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH	
; float exp
9:
        .byte   NFA|4
        .ascii  "fexp"
        .align  2
CFEXP_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _expf
        rcall    _expf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH	
; float log
9:
        .byte   NFA|4
        .ascii  "flog"
        .align  2
CFLOG_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _logf
        rcall    _logf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float log10
9:
        .byte   NFA|6
        .ascii  "flog10"
        .align  2
CFLoG10_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _log10f
        rcall    _log10f
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH	

; float atan
9:
        .byte   NFA|5
        .ascii  "fatan"
        .align  2
CFATAN_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _atanf
        rcall    _atanf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH	
; float asin
9:
        .byte   NFA|5
        .ascii  "fasin"
        .align  2
CFASIN_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _asinf
        rcall    _asinf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float acos
9:
        .byte   NFA|5
        .ascii  "facos"
        .align  2
CFACOS_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _acosf
        rcall    _acosf
        mov     W0, [W14++]
        mov     W1, [W14]
        return


