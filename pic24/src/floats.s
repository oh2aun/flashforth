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
        call    ___addsf3
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
        call    ___subsf3
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
        call    ___mulsf3
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
        call    ___divsf3
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
        call    _powf
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
        call    _sinf
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
        call    _cosf
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
        call    _tanf
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
        call    _sqrtf
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
        call    _expf
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
        call    _logf
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
        call    _log10f
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
        call    _atanf
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
        call    _asinf
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
        call    _acosf
        mov     W0, [W14++]
        mov     W1, [W14]
        return


; Additional math words, IgorM

		.pword  paddr(9b)+PFLASH
; float atan2
9:
        .byte   NFA|6
        .ascii  "fatan2"
        .align  2
CFATAN2_:
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14], W0
        .extern _atan2f
        call    _atan2f
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float cosh
9:
        .byte   NFA|5
        .ascii  "fcosh"
        .align  2
CFCOSH_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _coshf
        call    _coshf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

        .pword  paddr(9b)+PFLASH
; float sinh
9:
        .byte   NFA|5
        .ascii  "fsinh"
        .align  2
CFSINH_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _sinhf
        call    _sinhf
        mov     W0, [W14++]
        mov     W1, [W14]
        return

       .pword  paddr(9b)+PFLASH
; float tanh
9:
        .byte   NFA|5
        .ascii  "ftanh"
        .align  2
CFTANH_:
        mov     [W14--], W1
        mov     [W14], W0
        .extern _tanhf
        call    _tanhf
        mov     W0, [W14++]
        mov     W1, [W14]
        return
