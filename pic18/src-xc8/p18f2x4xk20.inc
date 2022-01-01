;**********************************************************************
;                                                                     *
;    Filename:      p18f2x4xk20.inc                                   *
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

        config FOSC = INTIO67
        config FCMEN = OFF
        config IESO = OFF
        config PWRT= ON
        config BOREN = OFF
        config BORV = 30
        config WDTEN = ON
        config WDTPS = 256
        config MCLRE = ON
        config HFOFST = OFF
        config LPT1OSC = OFF
        config PBADEN = OFF
        config CCP2MX = PORTC
        config STVREN = ON
        config LVP = OFF
        config XINST = OFF
        config DEBUG = OFF
        config CP0 = OFF
        config CPB = OFF
        config WRTC = ON

;;; Activate 4xPLL from SW
#define PLL DISABLE	         ; ENABLE or DISABLE

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
