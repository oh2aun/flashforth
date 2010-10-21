;;; FlashForth device configuration for the dsPIC33
config FOSCSEL, FNOSC_PRI
config FOSC, FCKSM_CSDCMD & IOL1WAY_OFF & OSCIOFNC_OFF & POSCMD_HS
;config FWDT,
;config FPOR,


;.equ FREQ, (7322000)					  ;Clock (Crystal)frequency (Hz)
.equ FREQ, (16000000)					  ;Clock (Crystal)frequency (Hz)
.equ BAUDRATE, 9600					      ; Serial baudrate

.equ BAUD_DIV1, (FREQ/2/16/BAUDRATE)-1		; UART divisor
.equ MS_PR_VAL, (FREQ/2/1000)				; Timer value for 1 ms ticks

.equ U1RXPIN, 0xf
.equ U1TXPIN, 0xe
.equ U1CTSPIN, 0xd
.equ U1RTSPIN, 0xc
.equ RPINR18VAL, 0x0d0f; (U1RXPIN | (U1CTSPIN<<0x10))


; Set to one to use the alternate UART pins
.equ USE_ALTERNATE_UART_PINS, 0

; Flash write delay
.equ write_delay, 20

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 1

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

;;; Memory mapping prefixes
;;; To be set to the amount of ram + 0x800.
;;; The rest if the memory space is available for flash
;;; Below is the setting for max amount of ram for dsPIC33FJ128GP802
.equ PFLASH,         0x4800  ; 16 Kbytes RAM, 48 KBytes FLASH

;;; Flash memory commands. Do not modify.
.equ FLASH_ERASE,    0x4042  ; Memory page erase
.equ FLASH_WRITE,    0x4001  ; Memory row write
.equ FLASH_WRITE_SINGLE, 0x4003;Memory word program
.equ PAIVT,          0x0100
.equ IBUFSIZEL,      0x0400
.equ IBUFSIZEH,      0x0200
.equ IBUFLEN1,       0x0040   ; Inner write loop
.equ IBUFLEN2,       0x0008   ; Outer write loop
.equ IBUFMASK,       0xfc00
