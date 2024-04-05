;**********************************************************************
;                                                                     *
;    Filename:      p18f2x4xk22.inc                                   *
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

        config FOSC = INTIO67
        config PLLCFG = OFF
        config PRICLKEN = ON
        config FCMEN = OFF
        config IESO = OFF
        config PWRTEN = ON
        config BOREN = OFF
        config BORV = 285
        config WDTEN = ON
        config WDTPS = 256
        config CCP2MX = PORTB3
        config MCLRE = EXTMCLR
        config HFOFST = OFF
        config PBADEN = OFF
        config STVREN = ON
        config LVP = OFF
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
