;**********************************************************************
;                                                                     *
;    Filename:      p18f24k50-25k50-45k50.inc                         *
;    Date:          05.04.2024                                        *
;    FF Version:    5X                                                *
;    MCU            18f24k50 18f25k50 18f45k50                        *
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
;;; Low speed USB CDC is not standard, but it works on linux.
;;; Windows does not support a low speed USB CDC device.

#define USB_CDC
#define USB_SPEED 1  // 0 = low speed, 1 = high speed
#define USB_BANK 4
#define INTOSC  0    // 0 = external oscillator  1 = internal oscillator

; The config directive is used control the processor configuration bits
; See processor specific include file for details.
; See datasheet for USB clock configuration rules.

#ifdef USB_CDC
#if INTOSC == 0                     // External oscillator
        config FOSC = HSH           ; 12 MHz HS Xtal oscillator
        config CPUDIV = NOCLKDIV    ; 48 MHZ cpu clock
        config PLLSEL = PLL4X       ; 4 x 12 MHz = 48 MHz system and USB clock
#endif
#if INTOSC == 1                     // Internal oscillator
#if USB_SPEED == 1                  // High speed
        config FOSC = INTOSCIO      ; Internal oscillator
        config CPUDIV = NOCLKDIV    ; 48 MHZ cpu clock
        config PLLSEL = PLL3X       ; 3 x 16 MHz = 48 MHz system clock
#else                               // low speed
        config FOSC = INTOSCIO      ; Internal oscillator
        config CPUDIV = NOCLKDIV    ; 48 MHZ cpu clock
        config PLLSEL = PLL3X       ; 3 x 16 MHz = 48 MHz system clock
        CONFIG  LS48MHZ = SYS48X8   ; 6 MHz USB clock
#endif
#endif
#else                               // NOT USB_CDC
        config FOSC = INTOSCCLKO      ; Internal oscillator
        config CPUDIV = NOCLKDIV    ; 48 MHZ cpu clock
        config PLLSEL = PLL3X       ; 3 x 16 MHz = 48 MHz system clock
#endif
        config CFGPLLEN = ON
        config PCLKEN = ON
        config FCMEN = OFF
        config IESO = OFF
        config nPWRTEN = ON
        config BOREN = SBORDIS
        config BORV = 250
        config WDTEN = OFF
        config WDTPS = 256
        config MCLRE = ON
        config ICPRT = OFF
        config STVREN = ON
        config LVP = OFF
        config XINST = ON

#define REG_U1RXIF PIR1
#define REG_U1RXIE PIE1
#define REG_U1RXIP IPR1
