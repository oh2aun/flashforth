;**********************************************************************
;                                                                     *
;    Filename:      p18f6722-family.inc                               *
;    Date:          05.04.2024                                        *
;    FF Version:    5X                                                *
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

        config OSC = HS
        config FCMEN = OFF
        config IESO = OFF
        config PWRT = ON
        config BOREN = OFF
        config BORV = 3        ; Not recognised by GPASM
        config WDT = ON
        config WDTPS = 128
        config MCLRE = ON
        config LPT1OSC = OFF
        config CCP2MX = PORTC
        config STVREN = ON
        config LVP = OFF
        config BBSIZ = BB8K
        config XINST = ON
        config DEBUG = OFF
        config WRTC = ON
        config WRTB = ON

;;; Activate 4xPLL from SW for INTOSC modes.
#define PLL_ENABLE 0	         // 1=ENABLE or 0=DISABLE

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
#define REG_U2RXIF PIR3
#define REG_U2RXIE PIE3 
#define REG_U2RXIP IPR3
