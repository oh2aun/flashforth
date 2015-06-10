; Macros for registers
.macro mreg flags, len, reg, name
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|\flags|\len
        .ascii  "\name"
        .align  2
        mov     #\reg, W0
        mov     W0, [++W14]
        return
.endm

; list here the registers you want to include in the FF core dictionary
;mreg 0,5,PORTA, porta
;mreg 0,5,TRISA, trisa
mreg 0,5,PORTB, portb
mreg 0,5,TRISB, trisb
;mreg 0,3,PR1
;mreg 0,5,U1BRG
;mreg 0,6,OSCCON
;mreg 0,6,CLKDIV
;mreg 0,6,PLLFBD
