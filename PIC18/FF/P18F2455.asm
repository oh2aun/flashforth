;        LIST P=18F2455
;        NOLIST
;-------------------------------------------------------------------------
; MPLAB-Cxx  PIC18F2455 processor definition module
; 
; (c) Copyright 1999-2009 Microchip Technology, All rights reserved
;-------------------------------------------------------------------------

SFR_UNBANKED0       UDATA_ACS H'F66'

UFRM                
UFRML               
UFRMLbits           RES 1     ; 0xF66
UFRMH               
UFRMHbits           RES 1     ; 0xF67
UIR                 
UIRbits             RES 1     ; 0xF68
UIE                 
UIEbits             RES 1     ; 0xF69
UEIR                
UEIRbits            RES 1     ; 0xF6A
UEIE                
UEIEbits            RES 1     ; 0xF6B
USTAT               
USTATbits           RES 1     ; 0xF6C
UCON                
UCONbits            RES 1     ; 0xF6D
UADDR               
UADDRbits           RES 1     ; 0xF6E
UCFG                
UCFGbits            RES 1     ; 0xF6F
UEP0                
UEP0bits            RES 1     ; 0xF70
UEP1                
UEP1bits            RES 1     ; 0xF71
UEP2                
UEP2bits            RES 1     ; 0xF72
UEP3                
UEP3bits            RES 1     ; 0xF73
UEP4                
UEP4bits            RES 1     ; 0xF74
UEP5                
UEP5bits            RES 1     ; 0xF75
UEP6                
UEP6bits            RES 1     ; 0xF76
UEP7                
UEP7bits            RES 1     ; 0xF77
UEP8                
UEP8bits            RES 1     ; 0xF78
UEP9                
UEP9bits            RES 1     ; 0xF79
UEP10               
UEP10bits           RES 1     ; 0xF7A
UEP11               
UEP11bits           RES 1     ; 0xF7B
UEP12               
UEP12bits           RES 1     ; 0xF7C
UEP13               
UEP13bits           RES 1     ; 0xF7D
UEP14               
UEP14bits           RES 1     ; 0xF7E
UEP15               
UEP15bits           RES 1     ; 0xF7F
PORTA               
PORTAbits           RES 1     ; 0xF80
PORTB               
PORTBbits           RES 1     ; 0xF81
PORTC               
PORTCbits           RES 1     ; 0xF82
                    RES 1
PORTE               
PORTEbits           RES 1     ; 0xF84
                    RES 4
LATA                
LATAbits            RES 1     ; 0xF89
LATB                
LATBbits            RES 1     ; 0xF8A
LATC                
LATCbits            RES 1     ; 0xF8B
                    RES 6
DDRA                
DDRAbits            
TRISA               
TRISAbits           RES 1     ; 0xF92
DDRB                
DDRBbits            
TRISB               
TRISBbits           RES 1     ; 0xF93
DDRC                
DDRCbits            
TRISC               
TRISCbits           RES 1     ; 0xF94
                    RES 6
OSCTUNE             
OSCTUNEbits         RES 1     ; 0xF9B
                    RES 1
PIE1                
PIE1bits            RES 1     ; 0xF9D
PIR1                
PIR1bits            RES 1     ; 0xF9E
IPR1                
IPR1bits            RES 1     ; 0xF9F
PIE2                
PIE2bits            RES 1     ; 0xFA0
PIR2                
PIR2bits            RES 1     ; 0xFA1
IPR2                
IPR2bits            RES 1     ; 0xFA2
                    RES 3
EECON1              
EECON1bits          RES 1     ; 0xFA6
EECON2              RES 1     ; 0xFA7
EEDATA              RES 1     ; 0xFA8
EEADR               RES 1     ; 0xFA9
                    RES 1
RCSTA               
RCSTAbits           RES 1     ; 0xFAB
TXSTA               
TXSTAbits           RES 1     ; 0xFAC
TXREG               RES 1     ; 0xFAD
RCREG               RES 1     ; 0xFAE
SPBRG               RES 1     ; 0xFAF
SPBRGH              RES 1     ; 0xFB0
T3CON               
T3CONbits           RES 1     ; 0xFB1
TMR3L               RES 1     ; 0xFB2
TMR3H               RES 1     ; 0xFB3
CMCON               
CMCONbits           RES 1     ; 0xFB4
CVRCON              
CVRCONbits          RES 1     ; 0xFB5
CCP1AS              
CCP1ASbits          
ECCP1AS             
ECCP1ASbits         RES 1     ; 0xFB6
CCP1DEL             
CCP1DELbits         
ECCP1DEL            
ECCP1DELbits        RES 1     ; 0xFB7
BAUDCON             
BAUDCONbits         RES 1     ; 0xFB8
                    RES 1
CCP2CON             
CCP2CONbits         RES 1     ; 0xFBA
CCPR2               
CCPR2L              RES 1     ; 0xFBB
CCPR2H              RES 1     ; 0xFBC
CCP1CON             
CCP1CONbits         RES 1     ; 0xFBD
CCPR1               
CCPR1L              RES 1     ; 0xFBE
CCPR1H              RES 1     ; 0xFBF
ADCON2              
ADCON2bits          RES 1     ; 0xFC0
ADCON1              
ADCON1bits          RES 1     ; 0xFC1
ADCON0              
ADCON0bits          RES 1     ; 0xFC2
ADRES               
ADRESL              RES 1     ; 0xFC3
ADRESH              RES 1     ; 0xFC4
SSPCON2             
SSPCON2bits         RES 1     ; 0xFC5
SSPCON1             
SSPCON1bits         RES 1     ; 0xFC6
SSPSTAT             
SSPSTATbits         RES 1     ; 0xFC7
SSPADD              RES 1     ; 0xFC8
SSPBUF              RES 1     ; 0xFC9
T2CON               
T2CONbits           RES 1     ; 0xFCA
PR2                 RES 1     ; 0xFCB
TMR2                RES 1     ; 0xFCC
T1CON               
T1CONbits           RES 1     ; 0xFCD
TMR1L               RES 1     ; 0xFCE
TMR1H               RES 1     ; 0xFCF
RCON                
RCONbits            RES 1     ; 0xFD0
WDTCON              
WDTCONbits          RES 1     ; 0xFD1
HLVDCON             
HLVDCONbits         
LVDCON              
LVDCONbits          RES 1     ; 0xFD2
OSCCON              
OSCCONbits          RES 1     ; 0xFD3
                    RES 1
T0CON               
T0CONbits           RES 1     ; 0xFD5
TMR0L               RES 1     ; 0xFD6
TMR0H               RES 1     ; 0xFD7
STATUS              
STATUSbits          RES 1     ; 0xFD8
FSR2                
FSR2L               RES 1     ; 0xFD9
FSR2H               RES 1     ; 0xFDA
PLUSW2              RES 1     ; 0xFDB
PREINC2             RES 1     ; 0xFDC
POSTDEC2            RES 1     ; 0xFDD
POSTINC2            RES 1     ; 0xFDE
INDF2               RES 1     ; 0xFDF
BSR                 RES 1     ; 0xFE0
FSR1                
FSR1L               RES 1     ; 0xFE1
FSR1H               RES 1     ; 0xFE2
PLUSW1              RES 1     ; 0xFE3
PREINC1             RES 1     ; 0xFE4
POSTDEC1            RES 1     ; 0xFE5
POSTINC1            RES 1     ; 0xFE6
INDF1               RES 1     ; 0xFE7
WREG                RES 1     ; 0xFE8
FSR0                
FSR0L               RES 1     ; 0xFE9
FSR0H               RES 1     ; 0xFEA
PLUSW0              RES 1     ; 0xFEB
PREINC0             RES 1     ; 0xFEC
POSTDEC0            RES 1     ; 0xFED
POSTINC0            RES 1     ; 0xFEE
INDF0               RES 1     ; 0xFEF
INTCON3             
INTCON3bits         RES 1     ; 0xFF0
INTCON2             
INTCON2bits         RES 1     ; 0xFF1
INTCON              
INTCONbits          RES 1     ; 0xFF2
PROD                
PRODL               RES 1     ; 0xFF3
PRODH               RES 1     ; 0xFF4
TABLAT              RES 1     ; 0xFF5
TBLPTR              
TBLPTRL             RES 1     ; 0xFF6
TBLPTRH             RES 1     ; 0xFF7
TBLPTRU             RES 1     ; 0xFF8
PC                  
PCL                 RES 1     ; 0xFF9
PCLATH              RES 1     ; 0xFFA
PCLATU              RES 1     ; 0xFFB
STKPTR              
STKPTRbits          RES 1     ; 0xFFC
TOS                 
TOSL                RES 1     ; 0xFFD
TOSH                RES 1     ; 0xFFE
TOSU                RES 1     ; 0xFFF


;*** Set all of memory to zeroes ***/
; Use FSR0 to increment through memory from address 0x0
; to the end of the last bank skipping sfrs and
; unimplemented addresses.
    CODE
#if 0
__zero_memory
    movlw  0x0       ;low byte of (end address + 1)
    movwf  PRODL, 0
    lfsr   0, 0x0    ;start address
    movlw  0x4       ;high byte of (end address + 1)
    rcall zero_block
    return 0
zero_block
loop_h
    cpfslt FSR0H, 0
    bra    compare_l
    clrf   POSTINC0, 0
    bra    loop_h
compare_l
    movf   PRODL, 0, 0
loop_l
    cpfslt FSR0L, 0
    return 0
    clrf   POSTINC0, 0
    bra    loop_l


        GLOBAL __zero_memory
#endif
        GLOBAL UFRM
        GLOBAL UFRMLbits
        GLOBAL UFRML
        GLOBAL UFRMHbits
        GLOBAL UFRMH
        GLOBAL UIRbits
        GLOBAL UIR
        GLOBAL UIEbits
        GLOBAL UIE
        GLOBAL UEIRbits
        GLOBAL UEIR
        GLOBAL UEIEbits
        GLOBAL UEIE
        GLOBAL USTATbits
        GLOBAL USTAT
        GLOBAL UCONbits
        GLOBAL UCON
        GLOBAL UADDRbits
        GLOBAL UADDR
        GLOBAL UCFGbits
        GLOBAL UCFG
        GLOBAL UEP0bits
        GLOBAL UEP0
        GLOBAL UEP1bits
        GLOBAL UEP1
        GLOBAL UEP2bits
        GLOBAL UEP2
        GLOBAL UEP3bits
        GLOBAL UEP3
        GLOBAL UEP4bits
        GLOBAL UEP4
        GLOBAL UEP5bits
        GLOBAL UEP5
        GLOBAL UEP6bits
        GLOBAL UEP6
        GLOBAL UEP7bits
        GLOBAL UEP7
        GLOBAL UEP8bits
        GLOBAL UEP8
        GLOBAL UEP9bits
        GLOBAL UEP9
        GLOBAL UEP10bits
        GLOBAL UEP10
        GLOBAL UEP11bits
        GLOBAL UEP11
        GLOBAL UEP12bits
        GLOBAL UEP12
        GLOBAL UEP13bits
        GLOBAL UEP13
        GLOBAL UEP14bits
        GLOBAL UEP14
        GLOBAL UEP15bits
        GLOBAL UEP15
        GLOBAL PORTAbits
        GLOBAL PORTA
        GLOBAL PORTBbits
        GLOBAL PORTB
        GLOBAL PORTCbits
        GLOBAL PORTC
        GLOBAL PORTEbits
        GLOBAL PORTE
        GLOBAL LATAbits
        GLOBAL LATA
        GLOBAL LATBbits
        GLOBAL LATB
        GLOBAL LATCbits
        GLOBAL LATC
        GLOBAL DDRAbits
        GLOBAL DDRA
        GLOBAL TRISAbits
        GLOBAL TRISA
        GLOBAL DDRBbits
        GLOBAL DDRB
        GLOBAL TRISBbits
        GLOBAL TRISB
        GLOBAL DDRCbits
        GLOBAL DDRC
        GLOBAL TRISCbits
        GLOBAL TRISC
        GLOBAL OSCTUNEbits
        GLOBAL OSCTUNE
        GLOBAL PIE1bits
        GLOBAL PIE1
        GLOBAL PIR1bits
        GLOBAL PIR1
        GLOBAL IPR1bits
        GLOBAL IPR1
        GLOBAL PIE2bits
        GLOBAL PIE2
        GLOBAL PIR2bits
        GLOBAL PIR2
        GLOBAL IPR2bits
        GLOBAL IPR2
        GLOBAL EECON1bits
        GLOBAL EECON1
        GLOBAL EECON2
        GLOBAL EEDATA
        GLOBAL EEADR
        GLOBAL RCSTAbits
        GLOBAL RCSTA
        GLOBAL TXSTAbits
        GLOBAL TXSTA
        GLOBAL TXREG
        GLOBAL RCREG
        GLOBAL SPBRG
        GLOBAL SPBRGH
        GLOBAL T3CONbits
        GLOBAL T3CON
        GLOBAL TMR3L
        GLOBAL TMR3H
        GLOBAL CMCONbits
        GLOBAL CMCON
        GLOBAL CVRCONbits
        GLOBAL CVRCON
        GLOBAL CCP1ASbits
        GLOBAL CCP1AS
        GLOBAL ECCP1ASbits
        GLOBAL ECCP1AS
        GLOBAL CCP1DELbits
        GLOBAL CCP1DEL
        GLOBAL ECCP1DELbits
        GLOBAL ECCP1DEL
        GLOBAL BAUDCONbits
        GLOBAL BAUDCON
        GLOBAL CCP2CONbits
        GLOBAL CCP2CON
        GLOBAL CCPR2
        GLOBAL CCPR2L
        GLOBAL CCPR2H
        GLOBAL CCP1CONbits
        GLOBAL CCP1CON
        GLOBAL CCPR1
        GLOBAL CCPR1L
        GLOBAL CCPR1H
        GLOBAL ADCON2bits
        GLOBAL ADCON2
        GLOBAL ADCON1bits
        GLOBAL ADCON1
        GLOBAL ADCON0bits
        GLOBAL ADCON0
        GLOBAL ADRES
        GLOBAL ADRESL
        GLOBAL ADRESH
        GLOBAL SSPCON2bits
        GLOBAL SSPCON2
        GLOBAL SSPCON1bits
        GLOBAL SSPCON1
        GLOBAL SSPSTATbits
        GLOBAL SSPSTAT
        GLOBAL SSPADD
        GLOBAL SSPBUF
        GLOBAL T2CONbits
        GLOBAL T2CON
        GLOBAL PR2
        GLOBAL TMR2
        GLOBAL T1CONbits
        GLOBAL T1CON
        GLOBAL TMR1L
        GLOBAL TMR1H
        GLOBAL RCONbits
        GLOBAL RCON
        GLOBAL WDTCONbits
        GLOBAL WDTCON
        GLOBAL HLVDCONbits
        GLOBAL HLVDCON
        GLOBAL LVDCONbits
        GLOBAL LVDCON
        GLOBAL OSCCONbits
        GLOBAL OSCCON
        GLOBAL T0CONbits
        GLOBAL T0CON
        GLOBAL TMR0L
        GLOBAL TMR0H
        GLOBAL STATUSbits
        GLOBAL STATUS
        GLOBAL FSR2
        GLOBAL FSR2L
        GLOBAL FSR2H
        GLOBAL PLUSW2
        GLOBAL PREINC2
        GLOBAL POSTDEC2
        GLOBAL POSTINC2
        GLOBAL INDF2
        GLOBAL BSR
        GLOBAL FSR1
        GLOBAL FSR1L
        GLOBAL FSR1H
        GLOBAL PLUSW1
        GLOBAL PREINC1
        GLOBAL POSTDEC1
        GLOBAL POSTINC1
        GLOBAL INDF1
        GLOBAL WREG
        GLOBAL FSR0
        GLOBAL FSR0L
        GLOBAL FSR0H
        GLOBAL PLUSW0
        GLOBAL PREINC0
        GLOBAL POSTDEC0
        GLOBAL POSTINC0
        GLOBAL INDF0
        GLOBAL INTCON3bits
        GLOBAL INTCON3
        GLOBAL INTCON2bits
        GLOBAL INTCON2
        GLOBAL INTCONbits
        GLOBAL INTCON
        GLOBAL PROD
        GLOBAL PRODL
        GLOBAL PRODH
        GLOBAL TABLAT
        GLOBAL TBLPTR
        GLOBAL TBLPTRL
        GLOBAL TBLPTRH
        GLOBAL TBLPTRU
        GLOBAL PC
        GLOBAL PCL
        GLOBAL PCLATH
        GLOBAL PCLATU
        GLOBAL STKPTRbits
        GLOBAL STKPTR
        GLOBAL TOS
        GLOBAL TOSL
        GLOBAL TOSH
        GLOBAL TOSU

;-------------------------------------------------------------------------

        LIST
        END
