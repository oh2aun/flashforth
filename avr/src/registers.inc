; Macros for registers
.macro mreg flags, len, reg, name
        fdw     9b
9:
        .byte   NFA|\flags|\len
        .ascii  "\name"
        .align  1
	m_dup
        ldi     tosl,lo8(\reg)
        ldi     tosh,hi8(\reg)
        ret
.endm

; list here the registers you want to include in the FF core dictionar
mreg 0,5,PORTB,portb
mreg 0,4,DDRB,ddrb

