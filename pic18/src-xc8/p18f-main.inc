//**********************************************************************
//                                                                     *
//    Filename:      p18f-main.inc                                     *
//    Date:          1.1.2022                                          *
//    FF Version:    5.0                                               *
//    Copyright:     Mikael Nordman                                    *
//    Author:        Mikael Nordman                                    *
//                                                                     * 
//    Modified:                                                        *
//**********************************************************************
//    FlashForth is licensed acording to the GNU General Public License*
//**********************************************************************
// FlashForth processor main configuration file

; Use PIC-AS global options for all variants except the pic18fxxQyy chips:
;-Wa,-a -Wl,-presetVec=0h,-phi_int=08h,-plo_int=18h,-pudata_acs=000h,-pudatabig=060h -Wl,-Map=test.map

; Use PIC-AS global options for the pic18fxxQyy chips:
;-Wa,-a -Wl,-presetVec=0h,-phi_int=08h,-plo_int=18h,-pudata_acs=500h,-pudatabig=560h -Wl,-Map=test.map

#define ENABLE 1
#define DISABLE 0
#define POSITIVE 1
#define NEGATIVE 0

//  Specify the cpu clock frequency here
//  It must be aligned with the processor specific configuration bits 
clock   equ 48000000  // Hz

// Specify the baud rate for the UARTs
baud    equ 38400
baud2   equ 38400
baud3   equ 38400

// The default number base
#define DEFAULT_BASE 16

// Define which timer is stepping TICKS and MS.
// Timer 1 2 3 4 5 6 can be used
#define MS_TMR 1
// TMR 2 4 6 gives exactly 1 ms ticks only with clocks like 4096, 8192, 16384 KHz. N*256KHz.

// System tick time in milliseconds.
// TIMERS 1, 3, 5
// - 1, 2, 4 ms can be used with upto 64 MHz clock
// - 8 ms can be used with upto 32 MHz clock
// - 16 ms can be used with upto 16 MHz clock
// - etcetera
// TIMERS 2, 4, 6
// - 1 ms can be used with upto 64 MHz clock
// - 2 ms can be used with upto 32 MHz clock
// - 4 ms can be used with upto 16 MHz clock
#define TICK_TIME 1

// Allow warm reset by ctrl-O
#define CTRL_O_WARM_RESET DISABLE     // ENABLE or DISABLE

// Put CPU in idle when operator task calls PAUSE
// Saves power on CPUs that support IDLE mode.
#define IDLE_MODE ENABLE   // ENABLE or DISABLE

// Measure CPU load, uses TIMER0. Needs IDLE_MODE ENABLE
#define CPU_LOAD ENABLE    // ENABLE or DISABLE

// CPU load indicator led definitions. Needs IDLE_MODE.
#define CPU_LOAD_LED ENABLE       // ENABLE or DISABLE
#define CPU_LOAD_TRIS TRISC
#define CPU_LOAD_PORT LATC
#define CPU_LOAD_BIT 0
#define CPU_LOAD_LED_POLARITY POSITIVE   // POSITIVE or NEGATIVE

// Define the startup delay for turnkey words.
// Use 8 seconds for USB_CDC to have time to start the USB
// connection and reset the turnkey
#define TURNKEY_DELAY 2000  // Milliseconds

// Define if extra UARTs are included
#define WANT_UART2 ENABLE
#define WANT_UART3 ENABLE

// Define if USB or which UART is used for the operator task
// 0=USB 1=UART1 2=UART2 3=UART3
#define OPERATOR_UART 1

// UART buffer size. Must be a power of 2. max size 128 bytes.
// The maximum RX_BUF_SIZE is d'64'(32 for usb pic14k50)
// Larger RX buffer size may be needed for high latency flow control
#define RX_BUF_SIZE 32
#define RX2_BUF_SIZE 32
#define RX3_BUF_SIZE 32

// Flow control for the operator UART interface
// Define the RX buffer fill level for XOFF or
// Define the RX buffer fill level for CTS off
// Smaller fill levels for RX_OFF_FILL may be needed for high latency flow control
RX_OFF_FILL     equ 4

// Flow control defintions ********************************
#define NONE 0
#define XONXOFF 1
#define CTS 2

#define FC_TYPE XONXOFF    // XONXOFF or CTS or NONE

// If SW flow control is disabled, HW flow control is enabled.
#if FC_TYPE == CTS
// Clear To Send to the computer, DB9 PIN 8
#define HW_FC_CTS_PORT LATC
#define HW_FC_CTS_TRIS TRISC
#define HW_FC_CTS_PIN  5
#endif
// The PIC18 has a separate HW return stack for call/return (= not in ram).
// A ram buffer is used for R> >R and for saving the HW return stack in a task switch.
// If you nest PAUSE shallower, the return stack size save area can be decreased.
// If only operator task is used, the return stack buffer size can be decreased.
// FF with the operator task uses less than 8 cells of ram for the  return stack.
// The multitasking examples needs 16 cells of operator return stack.
// Each nesting of FOR..NEXT uses 1 cell of ram.
// Each nesting of DO..LOOP uses 2 cells of ram.
#define RETURN_STACK_SAVE_SIZE 64    // 32 cells. Can be decreased to save memory
#define PARAMETER_STACK_SIZE 56      // 28 cells parameter stack
#define TIB_SIZE 80                  // 80 chars tib size 
#define HOLD_SIZE 10                 // 10 chars hold area

// Storage space for OPERATOR uservariables 2 and upwards
// EXAMPLE: 8 user myUserVar  -> define UADDSIZE 8
#define UADDSIZE 0

// Define the parameter stack size to be used by Forth interrupt words
#define PARAMETER_STACK_SIZE_IRQ 10

// X! and X@
#define XSTORE DISABLE


// Calculate the baud rate control value
spbrgval    equ ((clock/baud)/16) - 1
spbrgvalx4  equ ((clock/baud)/4) - 1
spbrg2val    equ ((clock/baud2)/16) - 1
spbrg2valx4  equ ((clock/baud2)/4) - 1
spbrg3valx4  equ ((clock/baud3)/4) - 1

// Constant for the MS word.
tmr2prescaler   equ 4
tmr2postscaler  equ 16
tmr2ms_val      equ (((clock/4000/tmr2prescaler/tmr2postscaler)*TICK_TIME)-1)
tmr1prescaler   equ 1
tmr1ms_val      equ ((clock/4000/tmr1prescaler)*TICK_TIME)

// Calculate the program memory write delay
write_delay     equ clock / 1000000  + 1

// Calculate the scaling factor for cpu load measurement
CPU_LOAD_VAL    equ clock / 1563
