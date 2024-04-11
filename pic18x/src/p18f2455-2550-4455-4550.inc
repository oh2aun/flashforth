;**********************************************************************
;                                                                     *
;    Filename:      p18f2455-2550-4455-4550.inc                       *
;    Date:          08.04.2024                                        *
;    FF Version:    5X                                                *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;    Modified:                                                        *
;**********************************************************************
;    FlashForth is licensed acording to the GNU General Public License*
;**********************************************************************
;;; FlashForth processor specific configuration

;;; Define USB_CDC if you want to use USB serial emulation
;;; If you use CDC, you must also compile usbcdc.asm
#define USB_CDC
#define USB_SPEED 1  // 0 = low speed, 1 = high speed
#define USB_BANK 4
; The config directive is used control the processor configuration bits
; See processor specific include file for details.

#if USB_SPEED ==1
        config PLLDIV = 3           ; 1 for 4 MHZ; 3 for 12 MHz XT xtal
        config CPUDIV = OSC1_PLL2   ; 48 MHz cpu clock
        config USBDIV = 2           ; USB clock from 96 MHZ PLL divided by 2
        config FOSC = HSPLL_HS
#else
        config PLLDIV = 3           ; 1 for 4 MHZ; 3 for 12 MHz XT xtal
        config CPUDIV = OSC3_PLL4   ; 24 MHz cpu clock/6MHz USB clock
        config USBDIV = 2           ; Not used
        config FOSC = HSPLL_HS
#endif
 ;       config FCMEN = OFF
        config IESO = OFF
        config PWRT = ON
;        config BOREN = OFF
        config BORV = 3
#ifdef USB_CDC
        config VREGEN = ON
#else
        config VREGEN = OFF
#endif
        config WDT = ON
        ;config WDTPS = 128
        config MCLRE = ON
        config LPT1OSC = OFF
        config PBADEN = OFF
        config CCP2MX = ON
        config STVREN = ON
        config LVP = OFF
        config XINST = ON
        config DEBUG = OFF
        config WRT1 = OFF
        config WRT0 = OFF
        config WRTB = OFF
        config WRTC = OFF

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1 
#define REG_U1RXIP IPR1