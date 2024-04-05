; list registers here
; The first one must always link to MEMHI
M_REGISTER 5,porta,PORTA,MEMHI
M_REGISTER 4,lata,LATA,PORTA
M_REGISTER 5,trisa,TRISA,LATA
M_REGISTER 5,portb,PORTB,TRISA
M_REGISTER 4,latb,LATB,PORTB
M_REGISTER 5,trisb,TRISB,LATB
M_REGISTER 5,portc,PORTC,TRISB
M_REGISTER 4,latc,LATC,PORTC
M_REGISTER 5,trisc,TRISC,LATC
;
        dw      L_TRISC    ; Link to the last definition
