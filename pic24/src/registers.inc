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
.ifdecl TRISA
mreg 0,5,PORTA, porta
mreg 0,5,TRISA, trisa
mreg 0,4,LATA, lata
.ifdecl ANSELA
mreg 0,6,ANSELA, ansela
.endif
.endif
.ifdecl TRISB
mreg 0,5,PORTB, portb
mreg 0,5,TRISB, trisb
mreg 0,4,LATB, latb
.ifdecl ANSELB
mreg 0,6,ANSELB, anselb
.endif
.endif
.ifdecl TRISC
mreg 0,5,PORTC, portc
mreg 0,5,TRISC, trisc
mreg 0,4,LATC, latc
.ifdecl ANSELC
mreg 0,6,ANSELC, anselc
.endif
.endif
.ifdecl TRISD
mreg 0,5,PORTD, portd
mreg 0,5,TRISD, trisd
mreg 0,4,LATD, latd
.ifdecl ANSELD
mreg 0,6,ANSELD, anseld
.endif
.endif
;mreg 0,3,PR1
;mreg 0,5,U1BRG
;mreg 0,6,OSCCON
;mreg 0,6,CLKDIV
;mreg 0,6,PLLFBD
