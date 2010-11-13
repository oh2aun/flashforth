;;; FlashForth device configuration for the dsPIC30Fxxxx
config __FOSC, CSW_FSCM_OFF & FRC_PLL8
;config __FOSC, HS;FRC; HS            ;Turn off clock switching and
                              ;fail-safe clock monitoring and
                              ;use the External HS Clock as the
                              ;system clock

config __FWDT, WDT_OFF              ;Turn on Watchdog Timer

config __FBORPOR, PBOR_ON & BORV_27 & PWRT_16 & MCLR_EN
                                    ;Set Brown-out Reset voltage and
                                    ;and set Power-up Timer to 16msecs
                                            
config __FGS, CODE_PROT_OFF         ;Set Code Protection Off


;.equ FREQ_OSC, 16000000;           ; Max baudrate 19200 with this clock
.equ FREQ_OSC, (7322000*8)			; Oscillator frequency

;;; UART1 configuration
.equ BAUDRATE1, 38400 				; Serial baudrate UART1
.equ FC1_TYPE, 1                    ; 1 = XON/XOFF, 2=CTS/RTS
.equ U1RTSPORT, LATB
.equ U1RTSTRIS, TRISB
.equ U1RTSPIN, 0xc

;;; UART2 configuration
;.equ BAUDRATE2, 38400 				; Serial baudrate UART2, comment if not used
.equ FC2_TYPE, 1                    ; 1 = XON/XOFF, 2=CTS/RTS
.equ U2RTSPORT, LATB
.equ U2RTSTRIS, TRISB
.equ U2RTSPIN, 0x3

; Set to one to use the alternate UART pins. Only for dsPIC30F.
.equ USE_ALTERNATE_UART_PINS, 1

; Default number base
.equ BASE_DEFAULT, 16      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 1

; Set to 1 to see when FF writes to flash and eeprom
; 'F' is printed before each flash write.
; 'E' is printed before each eeprom write
.equ DEBUG_FLASH, 1

; Write method 1 writes to flash always before the prompt. -> Robust. More wear on the flash
; - Recommended for all chips if delays between characters is large.
; - Recommended for dsPIC30F and PIC24F16KAxxx.
; Write method 2 writes to flash when needed and after WRITE_TIMEOUT ms timeout.-> Less Robust.Less wear on the flash
; - Pretty good for dsPIC33 and for PIC24 with large flash block size.
; - NOT WORKING CASE: If a word is executed that starts on the prevoius flash page
;   and it continues on the current flash page which has not yet been written to flash,
;   a reset will occur.
.equ WRITE_METHOD, 1
.equ WRITE_TIMEOUT, 500

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

;;; Memory mapping prefixes
;;; To be set to the amount of ram + 0x800 register space.
;;; The rest if the memory space is available for flash
;;; You can increase the addressable flash range be decreasing the addressable ram.
.equ PFLASH,       0x1000    ; 2 Kbytes RAM  61 Kbytes flash
.equ PEEPROM,      0xfc00    ; 1 Kbyte EEPROM

;;; Flash memory commands. Do not modify.
.equ EEPROM_ERASE, 0x4044     
.equ EEPROM_WRITE, 0x4004
.equ FLASH_ERASE,  0x4041
.equ FLASH_WRITE,  0x4001
.equ PAIVT,        0x80
.equ IBUFSIZEL,      0x0040
.equ IBUFSIZEH,      0x0020
.equ IBUFLEN1,       0x0020
.equ IBUFLEN2,       0x0001
.equ IBUFMASK,       0xffc0

.equ FCY, (FREQ_OSC/4)              ; CPU intruction frequency
.equ MS_PR_VAL, (FCY/1000)	            ; Timer value for 1 ms ticks
.equ BAUD_DIV1, (FCY/16/BAUDRATE1)-1	; UART1 divisor
.ifdecl BAUDRATE2
.equ BAUD_DIV2, (FCY/16/BAUDRATE2)-1	; UART2 divisor
.endif
.equ write_delay, 20           ; Flash write delay


