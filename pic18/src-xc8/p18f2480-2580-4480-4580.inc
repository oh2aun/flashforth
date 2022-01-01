;**********************************************************************
;                                                                     *
;    Filename:      p18f2480-2580-4480-4580.inc                       *
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

; Use PIC-AS global options:
;-Wa,-a -Wl,-presetVec=0h,-phi_int=08h,-plo_int=18h,-pudata_acs=000h,-pudatabig=060h -Wl,-Map=test.map

; The config directive is used control the processor configuration bits
; See processor specific include file for details.

        config OSC = XT
        config FCMEN = OFF
        config IESO = OFF
        config PWRT = ON
        config BOREN = BOHW
        config BORV = 3
        config WDT = ON
        config WDTPS = 256
        config MCLRE = ON
        config PBADEN = OFF
        config LPT1OSC = OFF
        config STVREN = ON
        config LVP = OFF
        config BBSIZ = 2048 ; How does he boot block size work. Not described in the datasheet.
        config XINST = OFF
        config DEBUG = OFF
        config WRT0 = OFF
        config WRTC = OFF
        config WRTB = OFF

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
