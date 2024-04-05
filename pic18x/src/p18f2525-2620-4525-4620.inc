;**********************************************************************
;                                                                     *
;    Filename:      p18f2525-2620-4525-4620.inc                       *
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

        config OSC = HS; PLL
        config FCMEN = OFF
        config IESO = OFF
        config PWRT = ON
        config BOREN = SBORDIS
        config BORV = 3
        config WDT = ON
        config WDTPS = 1024
        config MCLRE = ON
        config LPT1OSC = OFF
        config PBADEN = OFF
        config CCP2MX = PORTC
        config STVREN = ON
        config LVP = OFF
        config XINST = ON
        config DEBUG = OFF
        config WRT0 = OFF
        config WRTB = ON
        config WRTC = ON

;;; Activate 4xPLL from SW for INTOSC modes.
#define PLL_ENABLE 0	         // 1=ENABLE or 0=DISABLE

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
