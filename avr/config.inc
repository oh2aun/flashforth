;;; FlashForth device configuration for ATmega devices

; Oscillator frequency in herz
.equ FREQ_OSC = 8000000

; Define the UART used for the operator
.equ OPERATOR_UART = 0              ; 0 or 1

;;; UART0 configuration
.equ BAUDRATE0 = 38400 				; Serial baudrate UART0
;.equ FC0_TYPE = 1                    ; 1 = XON/XOFF, 2=CTS/RTS
;.equ U0RTSPORT = LATB
;.equ U0RTSTRIS = TRISB
;.equ U0RTSPIN = 0xc

;;; UART1 configuration
.equ BAUDRATE1= 38400 				; Serial baudrate UART1
;.equ FC1_TYPE = 1                    ; 1 = XON/XOFF, 2=CTS/RTS
;.equ U1RTSPORT = LATB
;.equ U1RTSTRIS = TRISB
;.equ U1RTSPIN = 0x3

; Default number base
.equ BASE_DEFAULT = 16      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for power save when CPU is idle
.equ IDLE_MODE = 0

; Set to 1 for CPU load measurement
.equ CPU_LOAD = 0

; CPU load indicator led definitions
.equ CPU_LOAD_LED = 0       ; set to 1 to enable
.equ CPU_LOAD_DDR = ddrc
.equ CPU_LOAD_PORT = portc
.equ CPU_LOAD_BIT = 5
.equ CPU_LOAD_LED_POLARITY = 0   ; 0 = low on port turns on led, 1 = high on port turns on led 

; Define the startup delay for turnkey words. Milliseconds
.equ TURNKEY_DELAY = 2000  ; milliseconds

; UART buffer sizes
.equ TX0_BUF_SIZE = 0
.equ RX0_BUF_SIZE = 32
.equ RX0_OFF_FILL = 8   ; Fill level for XOFF

.equ TX1_BUF_SIZE = 0
.equ RX1_BUF_SIZE = 32
.equ RX1_OFF_FILL = 8   ; Fill level for XOFF

;;; USER AREA sizes for the OPERATOR task
.equ RETURN_STACK_SIZE = 64          ; 32 cells return stack
.equ PARAMETER_STACK_SIZE = 64       ; 32 cells parameter stack
.equ TIB_SIZE = 84                   ; 74 chars tib size +  10 chars hold area

;;; Storage space for OPERATOR uservariables 2 and upwards
;;; EXAMPLE: 8 user myUserVar  -> define UADDSIZE 8
.equ UADDSIZE = 0

; Set to 1 to allow control-o to reset FlashForth from the operator UART
.equ CTRL_O_WARM_RESET = 1



