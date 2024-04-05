;**********************************************************************
;                                                                     *
;    Filename:      p18fxxq40.inc                                     *
;    Date:          05.04.2024                                        *
;    FF Version:    5X                                                *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;    Modified:                                                        *
;**********************************************************************
;   FlashForth is licensed according to the GNU General Public License*
;**********************************************************************
; FlashForth processor specific configuration
; PIC-AS params 
; -Wa,-a -Wl,-presetVec=0h,-phi_int=08h,-plo_int=18h,-pudata_acs=500h,-pudatabig=560h -Wl,-Map=test.map
; The config directive is used control the processor configuration bits
; See processor specific include file for details.

        ;CONFIG  FEXTOSC = XT ; OFF         
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
        CONFIG  LVP = OFF
        CONFIG  BOREN = OFF          
        CONFIG  BORV = VBOR_2P45      
        CONFIG  ZCD = OFF             
        CONFIG  PPS1WAY = OFF         
        CONFIG  STVREN = ON           
        CONFIG  DEBUG = OFF           
        CONFIG  XINST = ON           
        CONFIG  WDTCPS = WDTCPS_31    
        CONFIG  WDTE = ON           
        CONFIG  WDTCWS = WDTCWS_7     
        CONFIG  WDTCCS = LFINTOSC
        CONFIG  BBSIZE = BBSIZE_512   
        CONFIG  BBEN = OFF            
        CONFIG  SAFEN = OFF           
        CONFIG  WRTAPP = OFF          
        CONFIG  WRTB = OFF            
        CONFIG  WRTC = OFF            
        CONFIG  WRTD = OFF            
        CONFIG  WRTSAF = OFF          
        CONFIG  CP = OFF              

#define K42
#define Qxx

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
