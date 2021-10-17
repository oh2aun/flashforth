;;; FlashForth device configuration for the PIC24F(J)

.equ FREQ_OSC, (8000000*4)				  ;Clock (Crystal)frequency (Hz)

.equ FCY, (FREQ_OSC/2)

; USB CDC support
.equ USB_CDC, 1		    ; 1 includes the USB CDC support into FlashForth
			      
; Define the UART used for the operator
.equ OPERATOR_UART, 3	        ; 1=UART1 or 2=UART2 or 3=USB interface(USB_CDC=1)

;;; UART1 configuration
.equ BAUDRATE1, 38400			; Serial baudrate UART1
.equ FC1_TYPE, 1                        ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD1, 0                       ; 0 = to use fixed baudrate
                                        ; 1 = Autobaud, First char after reset must be 'U' (0x55)

;;; UART2 configuration
;.equ BAUDRATE2, 38400 			 ; Serial baudrate UART2, comment if not used
.equ FC2_TYPE, 1                        ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD2, 0                       ; 0 = to use fixed baudrate
                                        ; 1 = Autobaud, First char after reset must be 'U' (0x55)
;;; Peripheral pin mapping
.ifdecl RPINR18
.equ U1RXPIN, 0xf          ; 
.equ RPINR18VAL, 0x0d03    ; (U1RXPIN)
.equ U1TXPIN, 0x2
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
.equ CPU_LOAD_LED, 0       ; set to 0 to disable
.equ CPU_LOAD_TRIS, TRISB
.equ CPU_LOAD_PORT, PORTB
.equ CPU_LOAD_BIT, 8
.equ CPU_LOAD_LED_POLARITY, 0   ; 0 = low on port turns on led, 1 = high on port turns on led 

; Define the startup delay for turnkey words. Milliseconds
.equ TURNKEY_DELAY, 4000  ; milliseconds

; UART buffer sizes
; Valid sizes are 7, 15, 31, 63, 127, 255, 511, 1023, etc.
.equ TX1_BUF_SIZE, 15  ; Use 0 for unbuffered TX.
.equ RX1_BUF_SIZE, 31
.equ RX1_OFF_FILL, 8   ; Fill level for XOFF

.equ TX2_BUF_SIZE, 15  ; Use 0 for unbuffered TX.
.equ RX2_BUF_SIZE, 31
.equ RX2_OFF_FILL, 8   ; Fill level for XOFF

; Set to 1 if the UARTS have ERRATA on the UTXBF status flag.
.equ ERRATA_UTXBF, 0

;;; USER AREA sizes for the OPERATOR task
.equ RETURN_STACK_SIZE, 128          ; 64 cells return stack
.equ PARAMETER_STACK_SIZE, 96        ; 48 cells parameter stack
.equ INTERRUPT_STACK_FRAME, 16       ; 8 cells interrupt stack frame  on return stack
.equ TIB_SIZE, 84                    ; 84 chars tib size
.equ HOLD_SIZE, 10                   ; 10 chars hold area

;;; Storage space for OPERATOR uservariables 2 and upwards
;;; EXAMPLE: 8 user myUserVar  -> define UADDSIZE 8
.equ UADDSIZE, 0

; Set to 1 to see when FF writes to flash and eeprom
; 'F' is printed before each flash write.
; 'E' is printed before each eeprom write
.equ DEBUG_FLASH, 0

; Include the example for calling a C function. C4+,  C4add.c
.equ C_EXAMPLE, 0

; Include the 32-bit float number support
.equ FLOATS, 0

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

; Method 1 updates flash and eeprom after each ok prompt.
; Method 2 updates flash and eeprom after no flash or eeprom
; update requests have been made for a time of WRITE_TIMEOUT
.equ WRITE_METHOD, 2
.equ WRITE_TIMEOUT, 300         ; milliseconds

;;; Memory mapping prefixes
.equ SFR_SIZE,       0x800
.equ PFLASH,         SFR_SIZE+RAM_SIZE
.equ HI_TEMP,        PFLASH+FLASH_SIZE
.if HI_TEMP > 0xffff
.equ FLASH_HI,       (0xffff)
.else
.equ FLASH_HI,       (HI_TEMP-1)
.endif

;;; Memory control defines
.equ FLASH_ERASE,  0x4042	; Erase one row
.equ FLASH_WRITE,  0x4001	; Write one row
.equ FLASH_WRITE_SINGLE, 0x4003	; Memory word program
.equ FLASH_ROWSIZE,  0x0040	; Row size, words
.equ FLASH_ROWS,     0x0008	; Rows
.equ FBUFSIZE,       FLASH_ROWSIZE*FLASH_ROWS*2
.equ FBUFMASK,       ~(FBUFSIZE-1)

.if (0x10000 - SFR_SIZE - RAM_SIZE) < FLASH_SIZE
.equ WANT_X, 1
.else
.equ WANT_X, 0
.endif

.equ MS_PR_VAL, ((FCY/1000)-1)		      ; Timer value for 1 ms ticks
.equ BAUD_DIV1, (FCY/4/BAUDRATE1)-1		  ; UART1 divisor
.ifdecl BAUDRATE2
.equ BAUD_DIV2, (FCY/4/BAUDRATE2)-1		  ; UART2 divisor
.endif
.equ write_delay, 20                      ; Flash write delay

.if FLASH_SIZE > (0x10000 - PFLASH + (FBUFSIZE*4))
.equ DPS_LOW,  0    ; EEPROM emulation after end of addressable kernel
.else
.equ DPS_LOW,  1    ; EEPROM emulation inside kernel
.endif
.if DPS_LOW == 0    ; EEPROM EMULATION after end of addressable kernel
.equ DPS_ADDR, 0x10000 - PFLASH
.equ DPS_BASE, DPS_ADDR&0xffff
.equ DPS_PAGE, DPS_ADDR>>16
.else               ; EEPROM emulation inside kernel
.equ DPS_BASE, (KERNEL_END -  (FBUFSIZE*4))  ; AND FBUFMASK in runtime
.equ DPS_PAGE, 0
.endif

; Amount of interrupt vectors
.equ IVECSIZE, 118
