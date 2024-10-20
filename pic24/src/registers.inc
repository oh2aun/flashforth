; Macros for registers
.macro mreg len, reg, name
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|\len
        .ascii  "\name"
        .align  2
        mov     #\reg, W2
        mov     W2, [++W14]
        return
.endm

; list here the registers you want to include in the FF core dictionary
.ifdecl TRISA
mreg 5,PORTA, porta
mreg 5,TRISA, trisa
mreg 4,LATA, lata
.ifdecl ANSELA
mreg 6,ANSELA, ansela
.endif
.endif
.ifdecl TRISB
mreg 5,PORTB, portb
mreg 5,TRISB, trisb
mreg 4,LATB, latb
.ifdecl ANSELB
mreg 6,ANSELB, anselb
.endif
.endif
.ifdecl TRISC
mreg 5,PORTC, portc
mreg 5,TRISC, trisc
mreg 4,LATC, latc
.ifdecl ANSELC
mreg 6,ANSELC, anselc
.endif
.endif
.ifdecl TRISD
mreg 5,PORTD, portd
mreg 5,TRISD, trisd
mreg 4,LATD, latd
.ifdecl ANSELD
mreg 6,ANSELD, anseld
.endif
.endif
;mreg 3,PR1
;mreg 5,U1BRG
;mreg 6,OSCCON
;mreg 6,CLKDIV
;mreg 6,PLLFBD
