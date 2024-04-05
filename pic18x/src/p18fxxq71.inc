;**********************************************************************
;                                                                     *
;    Filename:      p18fxxq71.inc                                     *
;    Date:          05.04.2024                                        *
;    FF Version:    5X                                                *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;    Modified:                                                        *
;**********************************************************************
;    FlashForth is licensed acording to the GNU General Public License*
;**********************************************************************
;;; FlashForth processor specific configuration

; The config directive is used control the processor configuration bits
; See processor specific include file for details.

; CONFIG1
  CONFIG  FEXTOSC = OFF         ; External Oscillator Selection (Oscillator not enabled)
  CONFIG  RSTOSC = HFINTOSC_64MHZ; Reset Oscillator Selection (HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1)

; CONFIG2
  CONFIG  CLKOUTEN = OFF        ; Clock out Enable bit (CLKOUT function is disabled)
  CONFIG  PR1WAY = ON           ; PRLOCKED One-Way Set Enable bit (PRLOCKED bit can be cleared and set only once)
  CONFIG  BBEN = OFF            ; Boot Block enable bit (Boot block disabled)
  CONFIG  CSWEN = ON            ; Clock Switch Enable bit (Writing to NOSC and NDIV is allowed)
  CONFIG  FCMEN = ON            ; Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor enabled)
  CONFIG  FCMENP = ON           ; Fail-Safe Clock Monitor - Primary XTAL Enable bit (Fail-Safe Clock Monitor enabled; timer will flag FSCMP bit and OSFIF interrupt on EXTOSC failure.)
  CONFIG  FCMENS = ON           ; Fail-Safe Clock Monitor - Secondary XTAL Enable bit (Fail-Safe Clock Monitor enabled; timer will flag FSCMP bit and OSFIF interrupt on SOSC failure.)

; CONFIG3
  CONFIG  MCLRE = EXTMCLR       ; MCLR Enable bit (If LVP = 0, MCLR pin is MCLR; If LVP = 1, RE3 pin function is MCLR )
  CONFIG  PWRTS = PWRT_64       ; Power-up timer selection bits (PWRT set at 64ms)
  CONFIG  MVECEN = OFF          ; Multi-vector enable bit (Interrupt contoller does not use vector table to prioritze interrupts)
  CONFIG  IVT1WAY = ON          ; IVTLOCK bit One-way set enable bit (IVTLOCKED bit can be cleared and set only once)
  CONFIG  LPBOREN = OFF         ; Low Power BOR Enable bit (Low-Power BOR disabled)
  CONFIG  BOREN = SBORDIS       ; Brown-out Reset Enable bits (Brown-out Reset enabled , SBOREN bit is ignored)

; CONFIG4
  CONFIG  BORV = VBOR_1P9       ; Brown-out Reset Voltage Selection bits (Brown-out Reset Voltage (VBOR) set to 1.9V)
  CONFIG  ZCD = OFF             ; ZCD Disable bit (ZCD module is disabled. ZCD can be enabled by setting the ZCDSEN bit of ZCDCON)
  CONFIG  PPS1WAY = OFF         ; PPSLOCK bit One-Way Set Enable bit (PPSLOCKED bit can be set and cleared repeatedly (subject to the unlock sequence))
  CONFIG  STVREN = ON           ; Stack Full/Underflow Reset Enable bit (Stack full/underflow will cause Reset)
  CONFIG  LVP = OFF             ; Low Voltage Programming Enable bit (HV on MCLR/VPP must be used for programming)
  CONFIG  DEBUG = OFF           ; Background Debugger (Background Debugger disabled)
  CONFIG  XINST = ON            ; Extended Instruction Set Enable bit (Extended Instruction Set and Indexed Addressing Mode disabled)

; CONFIG5
  CONFIG  WDTCPS = WDTCPS_24    ; WDT Period selection bits (Divider ratio 1:32)
  CONFIG  WDTE = OFF            ; WDT operating mode (WDT enabled regardless of sleep; SWDTEN is ignored)

; CONFIG6
  CONFIG  WDTCWS = WDTCWS_7     ; WDT Window Select bits (window always open (100%); software control; keyed access not required)
  CONFIG  WDTCCS = SC           ; WDT input clock selector (Software Control)

; CONFIG7
; BBSIZE = No Setting

; CONFIG8
  CONFIG  SAFSZ = SAFSZ_NONE    ; SAF Block Size Selection bits (NONE)

; CONFIG9
  CONFIG  WRTB = OFF            ; Boot Block Write Protection bit (Boot Block not Write protected)
  CONFIG  WRTC = OFF            ; Configuration Register Write Protection bit (Configuration registers not Write protected)
  CONFIG  WRTD = OFF            ; Data EEPROM Write Protection bit (Data EEPROM not Write protected)
  CONFIG  WRTSAF = OFF          ; SAF Write protection bit (SAF not Write Protected)
  CONFIG  WRTAPP = OFF          ; Application Block write protection bit (Application Block not write protected)

; CONFIG10
  CONFIG  CPD = OFF             ; Data EEPROM Code Protection bit (Data EEPROM code protection disabled)

; CONFIG11
  CONFIG  CP = OFF              ; PFM Code Protection bit (PFM code protection disabled)

#define K42
#define Qxx

;;; Define UART 1 pins for PPS
; Define UART TX pin
#define TX_TRIS  TRISB
#define TX_ANSEL ANSELB
#define TX_BIT   4
#define TX_PPS   RB4PPS 
#define TX_LAT   LATB
#define TX_CODE  0x10

; Define UART TX pin
#define RX_TRIS  TRISB
#define RX_ANSEL ANSELB
#define RX_BIT   7
#define RX_PPS   00001111B // pin: bits0-2, port: bits3-5

;;; Define UART 2 pins for PPS

; Define UART2 TX pin
#if WANT_UART2 == ENABLE
#define TX2_TRIS  TRISB
#define TX2_ANSEL ANSELB
#define TX2_BIT   6
#define TX2_PPS   RB6PPS 
#define TX2_LAT   LATB
#define TX2_CODE  0x13

; Define UART2 RX pin
#define RX2_TRIS  TRISB
#define RX2_ANSEL ANSELB
#define RX2_BIT   5
#define RX2_PPS   00001101B  // pin: bits0-2, port: bits3-5
#endif
;;; Define UART 3 pins for PPS

; Define UART3 TX pin
#if WANT_UART3 == ENABLE
#define TX3_TRIS  TRISB
#define TX3_ANSEL ANSELB
#define TX3_BIT   6
#define TX3_PPS   RB6PPS 
#define TX3_LAT   LATB
#define TX3_CODE  0x16

; Define UART3 RX pin
#define RX3_TRIS  TRISB
#define RX3_ANSEL ANSELB
#define RX3_BIT   5
#define RX3_PPS   00001101B  // pin: bits0-2, port: bits3-5
#endif

#define REG_U1RXIF PIR4
#define REG_U1RXIE PIE4 
#define REG_U1RXIP IPR4 
#define REG_U2RXIF PIR8
#define REG_U2RXIE PIE8 
#define REG_U2RXIP IPR8
#define REG_U3RXIF PIR9
#define REG_U3RXIE PIE9 
#define REG_U3RXIP IPR9
// Timer interrupt flags are in the access bank
