;**********************************************************************
;                                                                     *
;    Filename:      p18fxx2xx8.inc                                    *
;    Date:          01.01.2022                                        *
;    FF Version:    5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     *
;**********************************************************************
;    FlashForth is licensed acording to the GNU General Public License*
;**********************************************************************
;;; FlashForth processor specific configuration

; Use PIC-AS global options:
;-Wa,-a -Wl,-presetVec=0h,-phi_int=08h,-plo_int=18h,-pudata_acs=000h,-pudatabig=060h -Wl,-Map=test.map

; The config directive is used control the processor configuration bits
; See processor specific include file for details.

        config OSC = HS
        config OSCS = OFF
        config PWRT = ON
        config BOR = OFF
        config WDT = ON
        config WDTPS = 128
        config STVR = ON
        config LVP = OFF
        config WRT0 = ON
        config WRTB = ON
        config WRTC = ON
        
#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
