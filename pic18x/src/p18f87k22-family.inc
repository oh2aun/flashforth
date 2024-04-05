;**********************************************************************
;                                                                     *
;    Filename:      p18f87k22-family.inc                              *
;    Date:          05.04.2024                                        *
;    FF Version:    5X                                                *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;    NOTE: Only 65k22, 66k22, 85k22, 86k22 supported                  *
;**********************************************************************
;    FlashForth is licensed acording to the GNU General Public License*
;**********************************************************************
;;; FlashForth processor specific configuration

; The config directive is used control the processor configuration bits
; See processor specific include file for details.

        config FOSC = INTIO2
        config PLLCFG = OFF
        config FCMEN = OFF
        config IESO = OFF
        config PWRTEN = ON
        config BOREN = OFF
        config BORV = 3
        config WDTEN = OFF
        config WDTPS = 256
        config CCP2MX = PORTC
        config MCLRE = ON
        config XINST = ON
        config DEBUG = OFF
        config CP0 = OFF
        config CPB = OFF
        config WRTC = ON


;;; Activate 4xPLL from SW 
#define PLL_ENABLE 0	         // 1=ENABLE or 0=DISABLE

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
#define REG_U2RXIF PIR3
#define REG_U2RXIE PIE3 
#define REG_U2RXIP IPR3
#define REG_TMR1IF PIR1
#define REG_TMR3IF PIR2
