;**********************************************************************
;                                                                     *
;    Filename:      p18f8722-family.inc                               *
;    Date:          01.01.2022                                        *
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

        config OSC = HS
        config FCMEN = OFF
        config IESO = OFF
        config PWRT = ON
        config BOREN = ON
        config BORV = 3
        config WDT = ON
        config WDTPS = 128
        config MCLRE = ON
        config MODE = MC
        config LPT1OSC = OFF
        config ECCPMX = PORTE
        config CCP2MX = PORTC
        config STVREN = ON
        config LVP = OFF
        config BBSIZ = BB8K
        config XINST = OFF
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
