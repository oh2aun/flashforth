;**********************************************************************
;                                                                     *
;    Filename:      p18fxxq10.inc                                     *
;    Date:          28.01.2022                                        *
;    FF Version:    5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;    Modified:                                                        *
;**********************************************************************
;   FlashForth is licensed according to the GNU General Public License*
;**********************************************************************
; FlashForth processor specific configuration
; The config directive is used control the processor configuration bits
; See processor specific include file for details.
; PIC18F26Q10 Configuration Bit Settings
; Assembly source line config statements
; CONFIG1L
  CONFIG  FEXTOSC = OFF         ; External Oscillator mode Selection bits (Oscillator not enabled)
  CONFIG  RSTOSC = HFINTOSC_64MHZ; Power-up default value for COSC bits (HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1)

; CONFIG1H
  CONFIG  CLKOUTEN = OFF        ; Clock Out Enable bit (CLKOUT function is disabled)
  CONFIG  CSWEN = OFF           ; Clock Switch Enable bit (The NOSC and NDIV bits cannot be changed by user software)
  CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor disabled)

; CONFIG2L
  CONFIG  MCLRE = EXTMCLR       ; Master Clear Enable bit (MCLR pin (RE3) is MCLR)
  CONFIG  PWRTE = ON            ; Power-up Timer Enable bit (Power up timer enabled)
  CONFIG  LPBOREN = OFF         ; Low-power BOR enable bit (Low power BOR is disabled)
  CONFIG  BOREN = ON            ; Brown-out Reset Enable bits (Brown-out Reset enabled according to SBOREN)

; CONFIG2H
  CONFIG  BORV = VBOR_190       ; Brown Out Reset Voltage selection bits (Brown-out Reset Voltage (VBOR) set to 1.90V)
  CONFIG  ZCD = OFF             ; ZCD Disable bit (ZCD disabled. ZCD can be enabled by setting the ZCDSEN bit of ZCDCON)
  CONFIG  PPS1WAY = OFF         ; PPSLOCK bit One-Way Set Enable bit (PPSLOCK bit can be set and cleared repeatedly (subject to the unlock sequence))
  CONFIG  STVREN = ON           ; Stack Full/Underflow Reset Enable bit (Stack full/underflow will cause Reset)
  CONFIG  XINST = OFF           ; Extended Instruction Set Enable bit (Extended Instruction Set and Indexed Addressing Mode disabled)

; CONFIG3L
  CONFIG  WDTCPS = WDTCPS_14    ; WDT Period Select bits (Divider ratio 1:524299)
  CONFIG  WDTE = ON             ; WDT operating mode (WDT always enabled)

; CONFIG3H
  CONFIG  WDTCWS = WDTCWS_7     ; WDT Window Select bits (window always open (100%); software control; keyed access not required)
  CONFIG  WDTCCS = SC           ; WDT input clock selector (Software Control)

; CONFIG4L
  CONFIG  WRT0 = OFF            ; Write Protection Block 0 (Block 0 (000800-003FFFh) not write-protected)
  CONFIG  WRT1 = OFF            ; Write Protection Block 1 (Block 1 (004000-007FFFh) not write-protected)
  CONFIG  WRT2 = OFF            ; Write Protection Block 2 (Block 2 (008000-00BFFFh) not write-protected)
  CONFIG  WRT3 = OFF            ; Write Protection Block 3 (Block 3 (00C000-00FFFFh) not write-protected)

; CONFIG4H
  CONFIG  WRTC = OFF            ; Configuration Register Write Protection bit (Configuration registers (300000-30000Bh) not write-protected)
  CONFIG  WRTB = OFF            ; Boot Block Write Protection bit (Boot Block (000000-0007FFh) not write-protected)
  CONFIG  WRTD = OFF            ; Data EEPROM Write Protection bit (Data EEPROM not write-protected)
  CONFIG  SCANE = OFF           ; Scanner Enable bit (Scanner module is NOT available for use, SCANMD bit is ignored)
  CONFIG  LVP = OFF             ; Low Voltage Programming Enable bit (HV on MCLR/VPP must be used for programming)

; CONFIG5L
  CONFIG  CP = OFF              ; UserNVM Program Memory Code Protection bit (UserNVM code protection disabled)
  CONFIG  CPD = OFF             ; DataNVM Memory Code Protection bit (DataNVM code protection disabled)

; CONFIG5H

; CONFIG6L
  CONFIG  EBTR0 = OFF           ; Table Read Protection Block 0 (Block 0 (000800-003FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR1 = OFF           ; Table Read Protection Block 1 (Block 1 (004000-007FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR2 = OFF           ; Table Read Protection Block 2 (Block 2 (008000-00BFFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR3 = OFF           ; Table Read Protection Block 3 (Block 3 (00C000-00FFFFh) not protected from table reads executed in other blocks)

; CONFIG6H
  CONFIG  EBTRB = OFF           ; Boot Block Table Read Protection bit (Boot Block (000000-0007FFh) not protected from table reads executed in other blocks)

#define K42
;#define Qxx

;;; Define UART 1 pins for PPS
; Define UART TX pin
#define TX_TRIS  TRISB
#define TX_ANSEL ANSELB
#define TX_BIT   6
#define TX_PPS   RB6PPS 
#define TX_LAT   LATB
#define TX_CODE  0x10

; Define UART RX pin
#define RX_TRIS  TRISB
#define RX_ANSEL ANSELB
#define RX_BIT   5
#define RX_PPS   00001101B // pin: bits0-2, port: bits3-5

;;; Define UART 2 pins for PPS

; Define UART2 TX pin
#if WANT_UART2 == ENABLE
#define TX2_TRIS  TRISB
#define TX2_ANSEL ANSELB
#define TX2_BIT   7
#define TX2_PPS   RB7PPS 
#define TX2_LAT   LATB
#define TX2_CODE  0x13

; Define UART2 RX pin
#define RX2_TRIS  TRISB
#define RX2_ANSEL ANSELB
#define RX2_BIT   4
#define RX2_PPS   00001100B  // pin: bits0-2, port: bits3-5
#endif
;;; Define UART 3 pins for PPS

; Define UART3 TX pin
#if WANT_UART3 == ENABLE
#define TX3_TRIS  TRISC
#define TX3_ANSEL ANSELC
#define TX3_BIT   1
#define TX3_PPS   RC1PPS 
#define TX3_LAT   LATC
#define TX3_CODE  0x16

; Define UART3 RX pin
#define RX3_TRIS  TRISC
#define RX3_ANSEL ANSELC
#define RX3_BIT   0
#define RX3_PPS   00010000B  // pin: bits0-2, port: bits3-5
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
