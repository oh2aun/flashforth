;;; FlashForth device configuration for Atmega devices

; Select the include file for your micro controller
;.include "m2561def.inc"   ; 
;.include "m2560def.inc"   ; Tested
;.include "m128def.inc"   ; Tested
;.include "m168pdef.inc"
.include "m328def.inc"    ; Tested
;.include "m32adef.inc"
;.include "m644pdef.inc"

; Oscillator frequency in herz
.equ FREQ_OSC = 8000000

; Define the UART used for the operator
.equ OPERATOR_UART = 0              ; 0 or 1

;;; UART0 configuration
;;; Note: With Arduino Uno R3 and MEGA R3 the USB to serial bridge destroys the 
;;; flow control. Best way to upload files is with TeraTerm and 1 ms intercharacter delay.
.equ BAUDRATE0 = 38400				; Serial baudrate UART0
.equ U0FC_TYPE = 2                  ; 1 = XON/XOFF, 2=CTS/RTS
.equ U0RTS_PORT = portd
.equ U0RTS_DDR = ddrd
.equ U0RTS_BIT = 3

;;; UART1 configuration
.equ BAUDRATE1= 38400 				; Serial baudrate UART1
.equ U1FC_TYPE = 1                  ; 1 = XON/XOFF, 2=CTS/RTS
.equ U1RTS_PORT = portd
.equ U1RTS_DDR = ddrd
.equ U1RTS_BIT = 4

; Default number base
.equ BASE_DEFAULT = 10      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for power save when CPU is idle
.equ IDLE_MODE = 1

; CPU load indicator led definitions. Needs IDLE_MODE
.equ CPU_LOAD_LED = 1       ; set to 1 to enable
.equ CPU_LOAD_DDR = ddrc
.equ CPU_LOAD_PORT = portc  ; avr-p28:portc arduinomega:portb arduinouno:portb
.equ CPU_LOAD_BIT = 5       ; avr-p28:pin5  arduinomega:pin7  ardinouno:pin5
.equ CPU_LOAD_LED_POLARITY = 0   ; avr-p28:  0 = low on port turns on led, 
                                 ; arduino : 1 = high on port turns on led 

; Define the startup delay for turnkey words. Milliseconds
.equ TURNKEY_DELAY = 2000  ; milliseconds

; UART buffer sizes
.equ TX0_BUF_SIZE = 0
.equ RX0_BUF_SIZE = 32  ; 8,16,32,64
.equ RX0_OFF_FILL = 4   ; Fill level for XOFF

.equ TX1_BUF_SIZE = 0
.equ RX1_BUF_SIZE = 32  ; 8,16,32,64
.equ RX1_OFF_FILL = 4   ; Fill level for XOFF

;;; USER AREA sizes for the OPERATOR task
.equ RETURN_STACK_SIZE = 96          ; 48 cells return stack
.equ PARAMETER_STACK_SIZE = 64       ; 32 cells parameter stack
.equ TIB_SIZE = 84                   ; 74 chars tib size +  10 chars hold area

; Set to 1 to allow control-o to reset FlashForth from the operator UART
.equ CTRL_O_WARM_RESET = 1

; Select which timer to use for the system millisecond ticks 0, 1, 2
.equ MS_TIMER = 2

; Enable the cpu load measurement
.equ CPU_LOAD = 1 

