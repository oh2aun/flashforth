;**********************************************************************
;                                                                     *
;    Filename:      p18f2420-2520-4420-4520.inc                       *
;    Date:          05.04.2024                                        *
;    FF Version:    5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;    Modified:                                                        *
;**********************************************************************
;    FlashForth is licensed acording to the GNU General Public License*
;**********************************************************************
;;; FlashForth processor specific configuration

; The config directive is used control the processor configuration bits
; See processor specific include file for details.

        config OSC = INTIO7 ; HS
        config FCMEN = OFF
        config IESO = OFF
        config PWRT = ON
        config BOREN = SBORDIS
        config WDT = ON
        config WDTPS = 256
        config MCLRE = ON
        config LPT1OSC = OFF
        config PBADEN = OFF
        config CCP2MX = PORTC
        config STVREN = ON
        config LVP = OFF
        config XINST = ON
        config DEBUG = OFF
        config CP0 = ON
        config CPB = ON
        config WRTC = ON

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
