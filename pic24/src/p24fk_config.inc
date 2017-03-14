;;; FlashForth device configuration for the PIC24FyyKxxx

;;; Device memory sizes. Set according to your device.
;;; You can increase the addressable flash range be decreasing the addressable ram.
;;; Below is the setting for max amount of ram for PIC24FV32KA302
.equ FLASH_SIZE,     0x5800  ; Flash size in bytes without the high byte
                             ; See program memory size in the device datasheet.
.equ RAM_SIZE,       0x0800  ; Ram size in bytes
.equ EEPROM_SIZE,    0x0200  ; Eeprom size

;; Check the configuration for your particular chip from the datasheet.
; For some reason the normal config macros did not work
           .pushsection __FOSCSEL.sec, code
           .global __FOSCSEL
__FOSCSEL: .pword FNOSC_FRCPLL
           .popsection


.equ FREQ_OSC, (8000000*4)	 ;Clock (Crystal)frequency (Hz)

; Define the UART used for the operator
.equ OPERATOR_UART, 1        ; 1 or 2

;;; UART1 configuration
.equ BAUDRATE1, 38400		 ; Serial baudrate
.equ FC1_TYPE, 1             ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD1, 0            ; 0 = to use fixed baudrate
                             ; 1 = Autobaud, First char after reset must be 'U' (0x55)
.equ U1RTSPORT, LATB
.equ U1RTSTRIS, TRISB
.equ U1RTSPIN, 0xc

;;; UART2 configuration
;.equ BAUDRATE2, 38400		 ; Serial baudrate, comment if not used
.equ FC2_TYPE, 1             ; 1 = XON/XOFF, 2=CTS/RTS
.equ AUTOBAUD2, 0            ; 0 = to use fixed baudrate
                             ; 1 = Autobaud, First char after reset must be 'U' (0x55)
.equ U2RTSPORT, LATB
.equ U2RTSTRIS, TRISB
.equ U2RTSPIN, 0xc

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
.equ TURNKEY_DELAY, 2000  ; milliseconds

; UART buffer sizes
; Valid sizes are 7, 15, 31, 63, 127, 255, 511, 1023, etc.
.equ TX1_BUF_SIZE, 0  ; Use 0 for unbuffered TX.
.equ RX1_BUF_SIZE, 31
.equ RX1_OFF_FILL, 4   ; Fill level for XOFF

.equ TX2_BUF_SIZE, 0  ; Use 0 for unbuffered TX.
.equ RX2_BUF_SIZE, 31
.equ RX2_OFF_FILL, 4   ; Fill level for XOFF

; Set to 1 if the UARTS have ERRATA on the UTXBF status flag.
.equ ERRATA_UTXBF, 1

;;; USER AREA sizes for the OPERATOR task
.equ RETURN_STACK_SIZE, 128          ; 64 cells return stack
.equ PARAMETER_STACK_SIZE, 96        ; 48 cells parameter stack
.equ INTERRUPT_STACK_FRAME, 16       ; 8 cells interrupt stack frame on return stack
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
; Needs linker parameter 
.equ FLOATS, 0

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

; Method 1 updates flash and eeprom after each ok prompt.
.equ WRITE_METHOD, 1

;;; Memory mapping prefixes
.equ SFR_SIZE,       0x800
.equ PFLASH,         SFR_SIZE+RAM_SIZE
.equ PEEPROM,        0x10000-EEPROM_SIZE
.equ HI_TEMP,        PFLASH+EEPROM_SIZE+FLASH_SIZE
.if HI_TEMP > 0xffff
.equ FLASH_HI,       (PEEPROM-1)
.else
.equ FLASH_HI,       (HI_TEMP-EEPROM_SIZE-1)
.endif

;;; Memory control defines
.equ EEPROM_WRITE, 0x4004	; Write one word automatic erase
.equ FLASH_ERASE,  0x4058	; Erase one row (64/96 bytes)
.equ FLASH_WRITE,  0x4004   ; Write one row (64/96 bytes)
.equ PAIVT,        0x100
.equ IBUFSIZEL,    0x0040
.equ IBUFSIZEH,    0x0020
.equ IBUFLEN1,     0x0020   ; Inner write loop
.equ IBUFLEN2,     0x0001   ; Outer write loop
.equ IBUFMASK,     0xffc0

.if (0x10000 - PFLASH) < FLASH_SIZE
.equ WANT_X, 1
.else
.equ WANT_X, 0
.endif

.equ FCY, (FREQ_OSC/2)
.equ BAUD_DIV1, (FCY/4/BAUDRATE1)-1		  ; UART divisor
.ifdecl BAUDRATE2
.equ BAUD_DIV2, (FCY/4/BAUDRATE2)-1		  ; UART divisor
.endif
.equ MS_PR_VAL, (FCY/1000-1)		      ; Timer value for 1 ms ticks
.equ write_delay, 20                      ; Flash write delay

; Amount of interrupt vectors
.equ IVECSIZE, 126
