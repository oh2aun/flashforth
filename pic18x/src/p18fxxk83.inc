//**********************************************************************
//                                                                     *
//    Filename:      p18fxxk83.inc                                     *
//    Date:          05.04.2024                                        *
//    FF Version:    5X                                                *
//    Copyright:     Mikael Nordman                                    *
//    Author:        Mikael Nordman                                    *
//                                                                     *
//    Modified:                                                        *
//**********************************************************************
//    FlashForth is licensed acording to the GNU General Public License*
//**********************************************************************
// FlashForth processor specific configuration

// The config directive is used control the processor configuration bits
// See processor specific include file for details.
#if CONFIG_RESET == 0x0000
        CONFIG  FEXTOSC = OFF // HS
        CONFIG  RSTOSC = HFINTOSC_64MHZ
        CONFIG  CLKOUTEN = OFF
        CONFIG  PR1WAY = OFF
        CONFIG  CSWEN = OFF
        CONFIG  FCMEN = OFF
        CONFIG  MCLRE = EXTMCLR
        CONFIG  PWRTS = PWRT_64
        CONFIG  MVECEN = OFF
        CONFIG  IVT1WAY = OFF
        CONFIG  LPBOREN = OFF
        CONFIG  BOREN = OFF
        CONFIG  BORV = VBOR_2P45
        CONFIG  ZCD = OFF
        CONFIG  PPS1WAY = OFF
        CONFIG  STVREN = ON
        CONFIG  DEBUG = OFF
        CONFIG  XINST = ON
        CONFIG  WDTCPS = WDTCPS_31
        CONFIG  WDTE = OFF
        CONFIG  WDTCWS = WDTCWS_7
        CONFIG  WDTCCS = SC
        CONFIG  BBSIZE = BBSIZE_512
        CONFIG  BBEN = OFF
        CONFIG  SAFEN = OFF
        CONFIG  WRTAPP = OFF
        CONFIG  WRTB = OFF
        CONFIG  WRTC = OFF
        CONFIG  WRTD = OFF
        CONFIG  WRTSAF = OFF
        CONFIG  LVP = OFF
        CONFIG  CP = OFF
#endif

#define K42
#define K83

;;; Define UART 1 pins for PPS
; Define UART TX pin
#define TX_TRIS  TRISC
#define TX_ANSEL ANSELC
#define TX_BIT   6
#define TX_PPS   RC6PPS
#define TX_LAT   LATC
#define TX_CODE  0x13

; Define UART RX pin
#define RX_TRIS  TRISC
#define RX_ANSEL ANSELC
#define RX_BIT   7
#define RX_PPS   00010111B // pin: bits0-2, port: bits3-5

;;; Define UART 2 pins for PPS

; Define UART2 TX pin
#if WANT_UART2 == ENABLE
#define TX2_TRIS  TRISB
#define TX2_ANSEL ANSELB
#define TX2_BIT   6
#define TX2_PPS   RB6PPS
#define TX2_LAT   LATB
#define TX2_CODE  0x16

; Define UART2 RX pin
#define RX2_TRIS  TRISB
#define RX2_ANSEL ANSELB
#define RX2_BIT   5
#define RX2_PPS   00001101B  // pin: bits0-2, port: bits3-5
#endif

#define REG_U1RXIF PIR3
#define REG_U1RXIE PIE3
#define REG_U1RXIP IPR3
#define REG_U2RXIF PIR7
#define REG_U2RXIE PIE7
#define REG_U2RXIP IPR7
#define REG_TMR1IF PIR4
#define REG_TMR2IF PIR4
#define REG_TMR3IF PIR8
#define REG_TMR4IF PIR8
#define REG_TMR5IF PIR8
#define REG_TMR6IF PIR9
