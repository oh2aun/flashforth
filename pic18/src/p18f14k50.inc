;**********************************************************************
;                                                                     *
;    Filename:      p18f14k50.inc                                     *
;    Date:          17.01.2022                                        *
;    FF Version:    5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;    Modified:                                                        *
;**********************************************************************
;    FlashForth is licensed acording to the GNU General Public License*
;**********************************************************************
;;; FlashForth processor specific configuration

;;; Define USB_CDC if you want to use USB serial emulation
#define USB_CDC
#define USB_SPEED 0  // 0 = low speed, 1 = high speed
#define USB_BANK 2

; The config directive is used control the processor configuration bits
; See processor specific include file for details.
#if USB_SPEED == 0
        config CPUDIV = NOCLKDIV    ; 24 MHz cpu clock
        config USBDIV = OFF         ; 6 MHz USB clock
        config FOSC = HS            ; 6 MHz HS Xtal oscillator
        config PLLEN = ON           ; 24 MHz System clock
#else
        config CPUDIV = NOCLKDIV;   ; 48 MHz cpu clock
        config USBDIV = ON          ; USB clock from 96 MHZ PLL divided by 2
        config FOSC = HS            ; 12 MHz HS Xtal oscillator
        config PLLEN = ON           ; 48 MHz system clock 
#endif
        config PCLKEN = ON
        config FCMEN = OFF
        config IESO = OFF
        config PWRTEN = ON
        config BOREN = SBORDIS
        config BORV = 30
        config WDTEN = OFF
        config WDTPS = 1024
        config MCLRE = OFF
        config HFOFST = OFF
        config STVREN = ON
        config LVP = OFF
        config XINST = OFF
        config BBSIZ = ON
        config CP0 = OFF
        config CP1 = OFF
        config CPB = OFF
        config WRTC = OFF

;;; Activate 4xPLL from SW
#define PLL_ENABLE 0	         // 1=ENABLE or 0=DISABLE

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1
