;;; FlashForth device configuration for Atmega devices

; Select the include file for your micro controller
;.include "m2561def.inc"  ; 
;.include "m2560def.inc"  ; Tested     Fuses: E:0xff H:0xdc L:0xff
;.include "m128def.inc"   ; Tested     Fuses: E:0xff H:0xdc L:0xff
;.include "m168pdef.inc"
;.include "m328pdef.inc"   ; Tested    Fuses: E:0xff H:0xda L:0xff
.include "m328def.inc"   ; Tested    Fuses: E:0xff H:0xda L:0xff
;.include "m32adef.inc"
;.include "m644pdef.inc"

; Oscillator frequency in herz
.equ FREQ_OSC = 16000000

; Define the UART used for the operator
.equ OPERATOR_UART = 0              ; 0 or 1

;;; UART0 configuration
;;; Note: With Arduino Uno R3 and MEGA R3 the USB to serial bridge latency and queues 
;;; disturb the XON/XOFF flow control. 
;;; The workaround is to use XON/XOFF flow control and 1 ms intercharacter delay in the terminal program. Or use the ff-shell.py which adds CR LF flow control.
.equ BAUDRATE0 = 38400              ; Serial baudrate UART0
.equ U0FC_TYPE = 1                  ; 1 = XON/XOFF, 2=CTS/RTS
.equ U0RTS_PORT = portd
.equ U0RTS_DDR = ddrd
.equ U0RTS_BIT = 3

;;; UART1 configuration
.equ BAUDRATE1= 38400               ; Serial baudrate UART1
.equ U1FC_TYPE = 1                  ; 1 = XON/XOFF, 2=CTS/RTS
.equ U1RTS_PORT = portd
.equ U1RTS_DDR = ddrd
.equ U1RTS_BIT = 4

; Default number base
.equ BASE_DEFAULT = 10      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for power save when CPU is idle
.equ IDLE_MODE = 1

; Enable the cpu load measurement. Uses Timer 1. Needs IDLE_MODE = 1
.equ CPU_LOAD = 0

; CPU load indicator led definitions. Needs IDLE_MODE = 1
.equ CPU_LOAD_LED = 1            ; set to 1 to enable
.equ CPU_LOAD_DDR = ddrb
.equ CPU_LOAD_PORT = portb       ; avr-p28:portc arduinomega:portb arduinouno:portb
.equ CPU_LOAD_BIT = 5            ; avr-p28:pin5  arduinomega:pin7  ardinouno:pin5
.equ CPU_LOAD_LED_POLARITY = 1   ; avr-p28:  0 = low on port turns on led, 
                                 ; arduino : 1 = high on port turns on led 

; Define the startup delay for the turnkey words. Milliseconds
.equ TURNKEY_DELAY = 2000  ; milliseconds

; UART buffer sizes
.equ RX0_BUF_SIZE = 32  ; 8,16,32,64
.equ RX0_OFF_FILL = 4   ; Fill level for XOFF

.equ RX1_BUF_SIZE = 32  ; 8,16,32,64
.equ RX1_OFF_FILL = 4   ; Fill level for XOFF

;;; USER AREA sizes for the OPERATOR task
.equ RETURN_STACK_SIZE = 64          ; 48 cells return stack
.equ PARAMETER_STACK_SIZE = 96       ; 32 cells parameter stack
.equ TIB_SIZE = 90                   ; 80 chars tib size +  10 chars hold area

; Set to 1 to allow control-o to reset FlashForth from the operator UART
.equ CTRL_O_WARM_RESET = 1

; Select which timer to use for the system millisecond ticks 0, 1, 2
.equ MS_TIMER = 0

; Debug flash and eeprom writes
; Prints F=Write to FLASH E=Write to EEPROM
.equ DEBUG_FLASH = 0

#if defined(__ATmega2560__)
#define partstring "ATmega2560"
#elif defined(__ATmega328P__)
#define partstring "ATmega328P"
#elif defined(__ATmega328__)
#define partstring "ATmega328"
#elif defined(__ATmega128__)
#define partstring "ATmega128"
#elif defined(__ATmega2561__)
#define partstring "ATmega2561"
#elif defined(__ATmega644__)
#define partstring "ATmega644"
#else
#define partstring "ATmega"
#endif

.if MS_TIMER == 0
.ifdef OC0Aaddr
.equ MS_TIMER_ADDR = OC0Aaddr
.else
.equ MS_TIMER_ADDR = OC0addr
.endif
.endif

.if MS_TIMER == 1
.ifdef OC1Aaddr
.equ MS_TIMER_ADDR = OC1Aaddr
.else
.equ MS_TIMER_ADDR = OC1addr
.endif
.endif

.if MS_TIMER == 2
.ifdef OC2Aaddr
.equ MS_TIMER_ADDR = OC2Aaddr
.else
.equ MS_TIMER_ADDR = OC2addr
.endif
.endif

