;;; FlashForth device configuration for the dsPIC30Fxxxx

;;; Device memory sizes. Set according to your device.
;;; You can increase the addressable flash range be decreasing the addressable ram.
;;; Below is the setting for max amount of ram for dsPIC30F4012
.equ FLASH_SIZE,     0x8000  ; Flash size in bytes without the high byte
                             ; See program memory size in the device datasheet.
.equ RAM_SIZE,       0x0800  ; Ram size in bytes
.equ EEPROM_SIZE,    0x0400  ; Eeprom size

;; Check the configuration for your particular chip from the datasheet.
config __FOSC, CSW_FSCM_OFF & FRC;_PLL8
;config __FOSC, HS;FRC; HS            ;Turn off clock switching and
                              ;fail-safe clock monitoring and
                              ;use the External HS Clock as the
                              ;system clock

config __FWDT, WDT_OFF              ;Turn off Watchdog Timer

config __FBORPOR, PBOR_ON & PWRT_16 & MCLR_EN
                                    ;Set Brown-out Reset voltage and
                                    ;and set Power-up Timer to 16msecs
                                            
config __FGS, CODE_PROT_OFF         ;Set Code Protection Off


;.equ FREQ_OSC, 16000000;           ; Max baudrate 19200 with this clock
.equ FREQ_OSC, (7372800)			; Oscillator frequency
                                    ; NOTE: The baud calculation depends
                                    ; on this exact value for the FRC oscillator

; Define the UART used for the operator
.equ OPERATOR_UART, 1        ; 1 or 2

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

; Set to 1 for power save when CPU is idle
.equ IDLE_MODE, 1

; Set to 1 for CPU load measurement
.equ CPU_LOAD, 1

; CPU load indicator led definitions
.equ CPU_LOAD_LED, 1       ; set to 0 to disable
.equ CPU_LOAD_TRIS, TRISB
.equ CPU_LOAD_PORT, PORTB
.equ CPU_LOAD_BIT, 4
.equ CPU_LOAD_LED_POLARITY, 1   ; 0 = low on port turns on led, 1 = high on port turns on led

; Define the startup delay for turnkey words. Milliseconds
.equ TURNKEY_DELAY, 2000  ; milliseconds

; UART buffer sizes
.equ TX1_BUF_SIZE, 0  ; Use 0 for unbuffered TX.
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
.equ WRITE_METHOD,  1

;;; Memory mapping prefixes. Do not modify
.equ SFR_SIZE,       0x800
.equ PFLASH,         SFR_SIZE+RAM_SIZE
.equ PEEPROM,        0x10000-EEPROM_SIZE
.equ HI_TEMP,        PFLASH+EEPROM_SIZE+FLASH_SIZE
.if HI_TEMP > 0xffff
.equ FLASH_HI,       (PEEPROM-1)
.else
.equ FLASH_HI,       (HI_TEMP-EEPROM_SIZE-1)
.endif

;;; Flash memory commands. Do not modify.
.equ EEPROM_ERASE,   0x4044
.equ EEPROM_WRITE,   0x4004
.equ FLASH_ERASE,    0x4041
.equ FLASH_WRITE,    0x4001
.equ PAIVT,          0x80
.equ IBUFSIZEL,      0x0040
.equ IBUFSIZEH,      0x0020
.equ IBUFLEN1,       0x0020
.equ IBUFLEN2,       0x0001
.equ IBUFMASK,       0xffc0

.if (0x10000 - PFLASH) < FLASH_SIZE
.equ WANT_X, 1
.else
.equ WANT_X, 0
.endif

.equ FCY, (FREQ_OSC/4)                  ; CPU intruction frequency
.equ MS_PR_VAL, ((FCY/1000)-1)          ; Timer value for 1 ms ticks
.equ BAUD_DIV1, (FCY/16/BAUDRATE1)-1    ; UART1 divisor
.ifdecl BAUDRATE2
.equ BAUD_DIV2, (FCY/16/BAUDRATE2)-1	; UART2 divisor
.endif
.equ write_delay, 20           ; Flash write delay

; Amount of interrupt vectors
.equ IVECSIZE, 64

