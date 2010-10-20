;;; FlashForth device configuration for the dsPIC33
;config FBS, BSS_OFF & BWRP_OFF
config FOSCSEL, FNOSC_FRC
;config FOSC, FCKSM_CSDCMD & SOSCSEL_SOSCLP & POSCFREQ_MS & OSCIOFNC_OFF & POSCMOD_NONE
;config FWDT, FWDTEN_ON 
;config FPOR, MCLRE_OFF & BORV_V18 & I2C1SEL_PRI & PWRTEN_ON & BOREN_BOR3
;config FDS, DSWDTEN_OFF& DSBOREN_OFF


;.equ FREQ, (7322000)					  ;Clock (Crystal)frequency (Hz)
.equ FREQ, (16000000)					  ;Clock (Crystal)frequency (Hz)
.equ BAUDRATE, 9600					  ; Serial baudrate

.equ BAUD_DIV1, (FREQ/2/16/BAUDRATE)-1		; UART divisor
.equ MS_PR_VAL, (FREQ/2/1000)				; Timer value for 1 ms ticks

; Set to one to use the alternate UART pins
.equ USE_ALTERNATE_UART_PINS, 0

; Flash write delay
.equ write_delay, 20

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 1

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

;;; Memory mapping prefixes
.equ PFLASH,         0x2800  ; 8 Kbytes RAM, 56 KBytes FLASH
.equ FLASH_ERASE,    0x4042  ; Memory page erase
.equ FLASH_WRITE,    0x4001  ; Memory row write
.equ FLASH_WRITE_SINGLE, 0x4003;Memory word program
.equ PAIVT,          0x0100
.equ IBUFSIZEL,      0x0400
.equ IBUFSIZEH,      0x0200
.equ IBUFLEN1,       0x0040   ; Inner write loop
.equ IBUFLEN2,       0x0008   ; Outer write loop
.equ IBUFMASK,       0xfc00
