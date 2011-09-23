;;; FlashForth device configuration for the PIC24F
config __CONFIG2, FCKSM_CSDCMD & POSCMOD_NONE & FNOSC_FRCPLL

.equ FREQ_OSC, (8000000*4)				  ;Clock (Crystal)frequency (Hz)

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
;;; Peripheral pin mapping
.ifdecl RPINR18
.equ U1RXPIN, 0xf          ; 
.equ RPINR18VAL, 0x0d0f    ; (U1RXPIN)
.equ U1TXPIN, 0xe
.equ U1RTSPORT, LATB
.equ U1RTSTRIS, TRISB
.equ U1RTSPIN, 0xc
.endif

; Set to one to use the alternate UART pins
.equ USE_ALTERNATE_UART_PINS, 0

; Default number base
.equ BASE_DEFAULT, 10      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for power save when CPU is idle
.equ IDLE_MODE, 1

; Set to 1 for CPU load measurement
.equ CPU_LOAD, 1

; CPU load indicator led definitions
;.equ CPU_LOAD_TRIS, TRISB
;.equ CPU_LOAD_PORT, PORTB
;.equ CPU_LOAD_BIT, 8
;.equ CPU_LOAD_LED_POLARITY, 0   ; 0 = low on port turns on led, 1 = high on port turns on led 

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 0

; Set to 1 to see when FF writes to flash and eeprom
; 'F' is printed before each flash write.
; 'E' is printed before each eeprom write
.equ DEBUG_FLASH, 0

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
;;; To be set to the amount of ram + 0x800.
;;; The rest if the memory space is available for flash
;;; You can increase the addressable flash range be decreasing the addressable ram.
;;; Below is the setting for max amount of ram for dsPIC24HJ128GP502
.equ PFLASH,       0x1000   ; 4 Kbytes RAM 60 Kbyte flash

;;; Memory control defines
.equ FLASH_ERASE,  0x4042	; Erase one row (1024/1596 bytes)
.equ FLASH_WRITE,  0x4001   ; Write one row (128/196 bytes)
.equ FLASH_WRITE_SINGLE, 0x4003;Memory word program
.equ PAIVT,        0x100
.equ IBUFSIZEL,    0x0400
.equ IBUFSIZEH,    0x0200
.equ IBUFLEN1,     0x0040   ; Inner write loop
.equ IBUFLEN2,     0x0008   ; Outer write loop
.equ IBUFMASK,     0xfc00

.equ MS_PR_VAL, (FCY/1000)			      ; Timer value for 1 ms ticks
.equ BAUD_DIV1, (FCY/4/BAUDRATE1)-1		  ; UART1 divisor
.ifdecl BAUDRATE2
.equ BAUD_DIV2, (FCY/4/BAUDRATE2)-1		  ; UART2 divisor
.endif
.equ write_delay, 20                      ; Flash write delay

