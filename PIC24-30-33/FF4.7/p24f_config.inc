;;; FlashForth device configuration for the PIC24F
config __CONFIG2, FNOSC_FRCPLL

.equ FREQ, (8000000*4)					  ;Clock (Crystal)frequency (Hz)
.equ BAUDRATE, 38400					  ; Serial baudrate

.equ BAUD_DIV1, (FREQ/64/BAUDRATE)-1		; UART divisor
.equ MS_PR_VAL, (FREQ/4/1000)				; Timer value for 1 ms ticks

; Set to one to use the alternate UART pins
.equ USE_ALTERNATE_UART_PINS, 0

; Flash write delay
.equ write_delay, 20

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 1

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

.equ PFLASH,       0x4800   ; 16 Kbytes RAM 48 Kbyte flash
.equ FLASH_ERASE,  0x4042	; Erase one row (1024/1596 bytes)
.equ FLASH_WRITE,  0x4001   ; Write one row (128/196 bytes)
.equ FLASH_WRITE_SINGLE, 0x4003;Memory word program
.equ PAIVT,        0x100
.equ IBUFSIZEL,    0x0400
.equ IBUFSIZEH,    0x0200
.equ IBUFLEN1,     0x0040   ; Inner write loop
.equ IBUFLEN2,     0x0008   ; Outer write loop
.equ IBUFMASK,     0xfc00
