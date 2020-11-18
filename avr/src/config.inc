;;; FlashForth device configuration for Atmega devices

; Select the include file for your micro controller
; Define a symbol for which file to include. 
; #define ffm328

;.include "m2561def.inc"  ; 
#ifdef ffm2560
.include "m2560def.inc"  ; Tested     Fuses: E:0xff H:0xdc L:0xff
#endif
;.include "m128def.inc"   ; Tested     Fuses: E:0xff H:0xdc L:0xff
;.include "m168pdef.inc"
;.include "m328pdef.inc"  ; Tested    Fuses: E:0xff H:0xda L:0xff
#ifdef ffm328
.include "m328def.inc"   ; Tested    Fuses: E:0xff H:0xda L:0xff
#endif
;.include "m644pdef.inc"
#ifdef ffm32u4
.include "m32U4def.inc"   ; Tested Crystal Fuses: E:0xcf H:0xda L:0xff
#endif
#ifdef ffm16u4
.include "m16U4def.inc"  ; Not tested  Fuses : E:0xff H:0xda L:0xff
#endif

; Oscillator frequency in herz
.equ FREQ_OSC = 16000000

; Define the UART used for the operator
#ifdef op0
.equ OPERATOR_UART = 0              ; 0 or 1 (0 for 32u4 ) or 3 for USB
#endif
#ifdef op3
.equ OPERATOR_UART = 3
#endif
;;; UART0 configuration
;;; Note: With Arduino Uno R3 and MEGA R3 the USB to serial bridge latency and queues 
;;; disturb the XON/XOFF flow control. 
;;; The workaround is to use XON/XOFF flow control and/or 1/2 ms character delay in the terminal program. Or use the ff-shell.py which adds CR LF flow control.
.equ BAUDRATE0 = 38400              ; Serial baudrate UART0
.equ U0FC_TYPE = 0                  ; 0 = NONE, 1 = XON/XOFF, 2=CTS/RTS
.equ U0RTS_PORT = portd
.equ U0RTS_DDR = ddrd
.equ U0RTS_BIT = 3

;;; UART1 configuration
.equ BAUDRATE1= 38400               ; Serial baudrate UART1
.equ U1FC_TYPE = 0                  ; 0 = NONE, 1 = XON/XOFF, 2=CTS/RTS
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
#ifdef ffm2560
.equ CPU_LOAD_DDR = ddrb
.equ CPU_LOAD_PORT = portb       ; avr-p28:portc arduino mega:portb uno:portb leonardo:portc
.equ CPU_LOAD_BIT = 7            ; avr-p28:5     arduino mega:7     uno:5     leonardo:7
.equ CPU_LOAD_LED_POLARITY = 1   ; avr-p28:  0 = low on port turns on led, 
                                 ; arduino : 1 = high on port turns on led 
#endif
#ifdef ffm328
.equ CPU_LOAD_DDR = ddrb
.equ CPU_LOAD_PORT = portb       ; avr-p28:portc arduino mega:portb uno:portb leonardo:portc
.equ CPU_LOAD_BIT = 5            ; avr-p28:5     arduino mega:7     uno:5     leonardo:7
.equ CPU_LOAD_LED_POLARITY = 1   ; avr-p28:  0 = low on port turns on led, 
                                 ; arduino : 1 = high on port turns on led 
#endif
#ifdef ffm32u4
.equ CPU_LOAD_DDR = ddrc
.equ CPU_LOAD_PORT = portc       ; avr-p28:portc arduino mega:portb uno:portb leonardo:portc
.equ CPU_LOAD_BIT = 7            ; avr-p28:5     arduino mega:7     uno:5     leonardo:7
.equ CPU_LOAD_LED_POLARITY = 1   ; avr-p28:  0 = low on port turns on led, 
                                 ; arduino : 1 = high on port turns on led 
#endif

; Define the startup delay for the turnkey words. Milliseconds
.if OPERATOR_UART == 3
.equ TURNKEY_DELAY = 6000  ; milliseconds
.else
.equ TURNKEY_DELAY = 2000  ; milliseconds
.endif

; UART buffer sizes
.equ RX0_BUF_SIZE = 64  ; 8,16,32,64
.equ RX0_OFF_FILL = 4   ; Fill level for XOFF

.equ RX1_BUF_SIZE = 64  ; 8,16,32,64
.equ RX1_OFF_FILL = 4   ; Fill level for XOFF

;;; USER AREA sizes for the OPERATOR task
.equ RETURN_STACK_SIZE = 64          ; 32 cells return stack
.equ PARAMETER_STACK_SIZE = 64       ; 32 cells parameter stack
.equ TIB_SIZE = 90                   ; 80 chars tib size +  10 chars hold area

; Set to 1 to allow control-o to reset FlashForth from the operator UART
.equ CTRL_O_WARM_RESET = 0

; Select which timer to use for the system millisecond ticks 0, 1, 2
.equ MS_TIMER = 0

; Debug flash and eeprom writes
; Prints F=Write to FLASH E=Write to EEPROM
.equ DEBUG_FLASH = 0

#if defined(__ATmega2560__)
#define partstring "ATmega2560 "
.equ UARTS = 2

#elif defined(__ATmega328P__)
#define partstring "ATmega328P "
.equ UARTS = 1

#elif defined(__ATmega328__)
#define partstring "ATmega328"
.equ UARTS = 1

#elif defined(__ATmega128__)
#define partstring "ATmega128"
.equ UARTS = 2

#elif defined(__ATmega2561__)
#define partstring "ATmega2561 "
.equ UARTS = 2

#elif defined(__ATmega644__)
#define partstring "ATmega644"
.equ UARTS = 1

#elif defined(__ATmega32U4__)
#define partstring "ATmega32u4 "
.equ XXU4 = 32
.equ UARTS = 1

#elif defined(__ATmega16U4__)
#define partstring "ATmega16u4 "
.equ XXU4 = 16
.equ UARTS = 1

#else
#define partstring "ATmega "
.equ UARTS = 1

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

