;;; FlashForth device configuration for the PIC24FJ
config FBS, BSS_OFF & BWRP_OFF
config FOSCSEL, FNOSC_FRCPLL
config FOSC, FCKSM_CSDCMD & SOSCSEL_SOSCLP & POSCFREQ_MS & OSCIOFNC_OFF & POSCMOD_NONE
config FWDT, FWDTEN_ON 
config FPOR, MCLRE_OFF & BORV_V18 & I2C1SEL_PRI & PWRTEN_ON & BOREN_BOR3
config FDS, DSWDTEN_OFF& DSBOREN_OFF

.equ FREQ_OSC, (8000000*4)					  ; Clock (Crystal)frequency (Hz)

.equ FCY, (FREQ_OSC/2)

;;; UART1 configuration
.equ BAUDRATE1, 38400 				      ; Serial baudrate UART1
.equ FC1_TYPE, 1                          ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD1, 0                         ; 0 = to use fixed baudrate
                                          ; 1 = Autobaud, First char after reset must be 'U' (0x55)
.equ U1RTSPORT, LATB
.equ U1RTSTRIS, TRISB
.equ U1RTSPIN, 0xc

;;; UART2 configuration
;.equ BAUDRATE2, 38400 				      ; Serial baudrate UART2, comment if not used
.equ FC2_TYPE, 1                          ; ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD2, 0                         ; 0 = to use fixed baudrate
                                          ; 1 = Autobaud, First char after reset must be 'U' (0x55)
.equ U2RTSPORT, LATB
.equ U2RTSTRIS, TRISB
.equ U2RTSPIN, 0x3


.equ BAUD_DIV1, (FREQ/16/BAUDRATE)-1		; UART divisor
.equ MS_PR_VAL, (FREQ/4/1000)				; Timer value for 1 ms ticks

; Set to one to use the alternate UART pins
.equ USE_ALTERNATE_UART_PINS, 0

; Default number base
.equ BASE_DEFAULT, 16      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for power save when CPU is idle
.equ IDLE_MODE, 1

; Set to 1 for CPU load measurement
.equ CPU_LOAD, 1

; CPU load indicator led definitions
.equ CPU_LOAD_LED, 0       ; set to 0 to disable
.equ CPU_LOAD_TRIS, TRISB
.equ CPU_LOAD_PORT, PORTB
.equ CPU_LOAD_BIT, 8
.equ CPU_LOAD_LED_POLARITY, 0   ; 0 = low on port turns on led, 1 = high on port turns on led 

; Define the startup delay for turnkey words. Milliseconds
.equ TURNKEY_DELAY, 2000  ; milliseconds

; UART buffer sizes
.equ TX1_BUF_SIZE, 16
.equ RX1_BUF_SIZE, 32
.equ RX1_OFF_FILL, 16   ; Fill level for XOFF

.equ TX2_BUF_SIZE, 16
.equ RX2_BUF_SIZE, 32
.equ RX2_OFF_FILL, 16   ; Fill level for XOFF

;;; USER AREA sizes for the OPERATOR task
.equ RETURN_STACK_SIZE, 96           ; 48 cells return stack
.equ PARAMETER_STACK_SIZE, 96        ; 48 cells parameter stack
.equ TIB_SIZE, 74                    ; 74 chars tib size
.equ HOLD_SIZE, 10                   ; 10 chars hold area

;;; Storage space for OPERATOR uservariables 2 and upwards
;;; EXAMPLE: 8 user myUserVar  -> define UADDSIZE 8
.equ UADDSIZE, 0

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 0

; Set to 1 to see when FF writes to flash and eeprom
; 'F' is printed before each flash write.
; 'E' is printed before each eeprom write
.equ DEBUG_FLASH, 0

; Include the example for calling a C function. C4+,  C4add.c
.equ C_EXAMPLE, 0

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

;;; Memory mapping prefixes
;;; To be set to the amount of ram + 0x800 register space.
;;; The rest if the memory space is available for flash
;;; You can increase the addressable flash range be decreasing the addressable ram.
.equ PFLASH,  0x2000
.equ PEEPROM, 0xfe00
.equ EEPROM_ERASE, 0x4058   ; Erase one word
.equ EEPROM_WRITE, 0x4004	; Write one word
.equ FLASH_ERASE,  0x4058	; Erase one row (64/96 bytes)
.equ FLASH_WRITE,  0x4004   ; Write one row (64/96 bytes)
.equ PAIVT,        0x100
.equ IBUFSIZEL,    0x0040
.equ IBUFSIZEH,    0x0020
.equ IBUFLEN1,     0x0020   ; Inner write loop
.equ IBUFLEN2,     0x0001   ; Outer write loop
.equ IBUFMASK,     0xffc0


.equ MS_PR_VAL, (FCY/1000)			      ; Timer value for 1 ms ticks
.equ BAUD_DIV1, (FCY/4/BAUDRATE1)-1		  ; UART1 divisor
.ifdecl BAUDRATE2
.equ BAUD_DIV2, (FCY/4/BAUDRATE2)-1		  ; UART2 divisor
.endif
.equ write_delay, 20                      ; Flash write delay

