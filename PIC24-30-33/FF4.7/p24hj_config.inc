;;; FlashForth device configuration for the PIC24HJ
.ifndecl IOL1WAY_OFF
.equ IOL1WAY_OFF, 0xffff
.endif

config __FOSCSEL, IESO_ON & FNOSC_FRCPLL
config __FOSC, FCKSM_CSECMD & IOL1WAY_OFF & OSCIOFNC_OFF & POSCMD_NONE

.equ PLL_DIV, 30                          ; Fcy is 27637 KHz
.equ PLL_FBD, PLL_DIV-2
.equ PLL_POST, 2                          ; Do not change
.equ PLL_PRE, 2                           ; Do not change
.equ FREQ_FIN, 7370000                    ; PLL input frequency.
.equ FREQ_OSC, ( FREQ_FIN*PLL_DIV/PLL_PRE/PLL_POST )  ; PLL ouput frequency. Used with FRCPLL 

;.equ  FREQ_OSC, 7370000        ; Used with FNOSC_FRC POSCMD_NONE
;.equ  FREQ_OSC, 16000000       ; Used with FNOSC_PRI POSCMD_HS/XT and external XTAL

.equ FCY, (FREQ_OSC/2)

;;; UART1 configuration
.equ BAUDRATE1, 38400 				      ; Serial baudrate UART1
.equ FC1_TYPE, 1                          ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD1, 0                         ; 0 = to use fixed baudrate
                                          ; 1 = Autobaud, First char after reset must be 'U' (0x55)

;;; UART2 configuration
;.equ BAUDRATE2, 38400 				      ; Serial baudrate UART2, comment if not used
.equ FC2_TYPE, 1                          ; ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD2, 0                         ; 0 = to use fixed baudrate
                                          ; 1 = Autobaud, First char after reset must be 'U' (0x55)

; Map UART1 pins
.ifdecl RPINR18
.equ U1RXPIN, 0xf          ; 
.equ RPINR18VAL, 0x000f    ; (U1RXPIN)
.equ U1TXPIN, 0xe
.equ U1RTSPORT, LATB
.equ U1RTSTRIS, TRISB
.equ U1RTSPIN, 0xc
.endif

; Map UART2 pins
.ifdecl BAUDRATE2
.ifdecl RPINR19
.equ U2RXPIN, 0x2          ; 
.equ RPINR19VAL, 0x0002    ; (U2RXPIN)
.equ U2TXPIN, 0x3
.equ U2RTSPORT, LATB
.equ U2RTSTRIS, TRISB
.equ U2RTSPIN, 0x3
.endif
.endif

; Default number base
.equ BASE_DEFAULT, 16      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 0

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
;;; Below is the setting for max amount of ram for dsPIC24HJ128GP502
.equ PFLASH,         0x2800  ; 10 Kbytes RAM, 54(81) KBytes FLASH

;;; Memory control defines
.equ FLASH_ERASE,    0x4042  ; Memory page erase
.equ FLASH_WRITE,    0x4001  ; Memory row write
.equ FLASH_WRITE_SINGLE, 0x4003;Memory word program
.equ PAIVT,          0x0100
.equ IBUFSIZEL,      0x0400
.equ IBUFSIZEH,      0x0200
.equ IBUFLEN1,       0x0040   ; Inner write loop
.equ IBUFLEN2,       0x0008   ; Outer write loop
.equ IBUFMASK,       0xfc00


.equ MS_PR_VAL, (FCY/1000)			      ; Timer value for 1 ms ticks
.equ BAUD_DIV1, (FCY/4/BAUDRATE1)-1		  ; UART1 divisor
.ifdecl BAUDRATE2
.equ BAUD_DIV2, (FCY/4/BAUDRATE2)-1		  ; UART2 divisor
.endif
.equ write_delay, 20                      ; Flash write delay

