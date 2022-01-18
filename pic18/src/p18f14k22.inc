;**********************************************************************
;                                                                     *
;    Filename:      p18f14k22.inc                                     *
;    Date:          17.03.2014                                        *
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

        config FOSC = IRC
        config PLLEN = OFF
        config PCLKEN = ON
        config FCMEN = OFF
        config IESO = OFF
        config PWRTEN = ON
        config BOREN = OFF
        config BORV = 30
        config WDTEN = ON
        config WDTPS = 8192
        config MCLRE = ON
        config HFOFST = OFF
        config STVREN = ON
        config LVP = OFF
        config XINST = OFF
        config BBSIZ = ON
        config CP0 = ON
        config CPB = ON
        config WRTC = ON
        
;;; Activate 4xPLL from SW
#define PLL_ENABLE 0	         // 1=ENABLE or 0=DISABLE

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
