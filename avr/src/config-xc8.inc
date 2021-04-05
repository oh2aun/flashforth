;;; FlashForth device configuration for Atmega devices
;;; For Microchip XC8 compiler and MPLABX IDE.
;;; Tested with ATmega328 ATmega328P ATmega2560
; Oscillator frequency in herz
#define FREQ_OSC 16000000

; Define the UART used for the operator
; 0 or 1 (0 for 32u4 ) or 3 for USB
#define OPERATOR_UART 0 

;;; UART0 configuration
;;; Note: With Arduino Uno R3 and MEGA R3 the USB to serial bridge latency and queues 
;;; disturb the XON/XOFF flow control. 
;;; The workaround is to use XON/XOFF flow control and/or 1/2 ms character delay in the terminal program. Or use the ff-shell.py which adds CR LF flow control.
#define BAUDRATE0 38400              // Serial baudrate UART0
#define U0FC_TYPE 0                  // 0 = NONE, 1 = XON/XOFF, 2=CTS/RTS
#define U0RTS_PORT PORTD
#define U0RTS_DDR DDRD
#define U0RTS_BIT 3

;;; UART1 configuration
#define BAUDRATE1 38400              // Serial baudrate UART1
#define U1FC_TYPE 0                  // 0 = NONE, 1 = XON/XOFF, 2=CTS/RTS
#define U1RTS_PORT PORTD
#define U1RTS_DDR DDRD
#define U1RTS_BIT 4

; Default number base  16 = hexadecimal, 10 = decimal
#define BASE_DEFAULT 10

; Set to 1 for power save when CPU is idle
#define IDLE_MODE 1

; Enable the cpu load measurement. Uses Timer 1. Needs IDLE_MODE 1
#define CPU_LOAD 0

; CPU load indicator led definitions. Needs IDLE_MODE = 1
#define CPU_LOAD_LED 1            // set to 1 to enable

#if defined(__ATmega2560__)
#define CPU_LOAD_DDR DDRB
#define CPU_LOAD_PORT PORTB       // avr-p28:portc arduino mega:portb uno:portb leonardo:portc
#define CPU_LOAD_BIT 7            // avr-p28:5     arduino mega:7     uno:5     leonardo:7
#define CPU_LOAD_LED_POLARITY 1   // avr-p28:  0 = low on port turns on led, 
                                  // arduino : 1 = high on port turns on led 
#endif
#if defined(__ATmega328P__) || defined(__ATmega328__)
#define CPU_LOAD_DDR DDRB
#define CPU_LOAD_PORT PORTB       // avr-p28:portc arduino mega:portb uno:portb leonardo:portc
#define CPU_LOAD_BIT 5            // avr-p28:5     arduino mega:7     uno:5     leonardo:7
#define CPU_LOAD_LED_POLARITY 1   // avr-p28:  0 = low on port turns on led, 
                                  // arduino : 1 = high on port turns on led 
#endif
#ifdef __ATmega32U4__
#define CPU_LOAD_DDR DDRC
#define CPU_LOAD_PORT PORTC       // avr-p28:portc arduino mega:portb uno:portb leonardo:portc
#define CPU_LOAD_BIT 7            // avr-p28:5     arduino mega:7     uno:5     leonardo:7
#define CPU_LOAD_LED_POLARITY 1   // avr-p28:  0 = low on port turns on led, 
                                  // arduino : 1 = high on port turns on led 
#endif

; Define the startup delay for the turnkey words. Milliseconds
#if OPERATOR_UART == 3
#define TURNKEY_DELAY 6000  // milliseconds
#else
#define TURNKEY_DELAY 2000  // milliseconds
#endif

; UART buffer sizes
#define RX0_BUF_SIZE 32  // 8,16,32,64
#define RX0_OFF_FILL 4   // Fill level for XOFF

#define RX1_BUF_SIZE 32  // 8,16,32,64
#define RX1_OFF_FILL 4   // Fill level for XOFF

;;; USER AREA sizes for the OPERATOR task
#define RETURN_STACK_SIZE 64          // 32 cells return stack
#define PARAMETER_STACK_SIZE 64       // 32 cells parameter stack
#define TIB_SIZE 90                   // 80 chars tib size +  10 chars hold area

; Set to 1 to allow control-o to reset FlashForth from the operator UART
#define CTRL_O_WARM_RESET 0

; Select which timer to use for the system millisecond ticks 0, 1, 2
#define MS_TIMER 0

; Debug flash and eeprom writes
; Prints F=Write to FLASH E=Write to EEPROM
#define DEBUG_FLASH 1

#if defined(__ATmega2560__)
#define partstring "ATmega2560"
#define partlen 10
#define UARTS 2

#elif defined(__ATmega328P__)
#define partstring "ATmega328P"
#define partlen 10
#define UARTS 1

#elif defined(__ATmega328__)
#define partstring "ATmega328"
#define partlen 9
#define UARTS 1

#elif defined(__ATmega128__)
#define partstring "ATmega128"
#define partlen 9
#define UARTS 2

#elif defined(__ATmega2561__)
#define partstring "ATmega2561"
#define partlen 10
#define UARTS 2

#elif defined(__ATmega644__)
#define partstring "ATmega644"
#define partlen 9
#define UARTS 1

#elif defined(__ATmega32U4__)
#define partstring "ATmega32u4"
#define partlen 10
#define XXU4 32
#define UARTS 1

#elif defined(__ATmega16U4__)
#define partstring "ATmega16u4"
#define partlen 10
#define XXU4, 16
#define UARTS 1

#else
#define partstring "ATmega "
#define partlen 7
#define UARTS 1

#endif

#if MS_TIMER == 0
#define MS_VECTOR TIMER0_COMPA_vect_num + 1
#endif

#if MS_TIMER == 1
#ifdef OC1Aaddr
#define MS_TIMER_ADDR OC1Aaddr
#else
#define MS_TIMER_ADDR OC1addr
#endif
#endif

#if MS_TIMER == 2
#ifdef OC2Aaddr
#define MS_TIMER_ADDR OC2Aaddr
#else
#define MS_TIMER_ADDR OC2addr
#endif
#endif

