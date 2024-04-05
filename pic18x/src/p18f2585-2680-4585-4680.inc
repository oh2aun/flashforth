;**********************************************************************
;                                                                     *
;    Filename:      p18f2585-2680-4585-4680.inc                       *
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
        config BOREN = BOHW
        config BORV = 3
        config WDT = ON
        config WDTPS = 128
        config MCLRE = ON
        config PBADEN = OFF
        config LPT1OSC = OFF
        config STVREN = ON
        config LVP = OFF
        config BBSIZ = 4096 ; How does he boot block size work. Not described in the datasheet.
        config XINST = ON
        config DEBUG = OFF
        config WRT0 = OFF ; Not needed because of BBSIZ = 4096 ?
        config WRTC = ON
        config WRTB = ON

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
