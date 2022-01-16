;;**********************************************************************
;;                                                                     *
;;    Filename:      ff-pic18.S                                        *
;;    Date:          16.01.2022                                        *
;;    File Version:  5.0                                               *
;;    Copyright:     Mikael Nordman                                    *
;;    Author:        Mikael Nordman                                    *
;;                                                                     *
;;**********************************************************************
;; FlashForth is a standalone Forth system for microcontrollers that
;; can flash their own flash memory.
;;
;; Copyright (C) 2022  Mikael Nordman
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3 as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY;; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http:;;www.gnu.org/licenses/>.
;;
;; Modified versions of FlashForth must be clearly marked as such,
;; in the header of this file, and in the identification
;; displayed by the word VER.
;;**********************************************************************
#include "p18fxxxx.inc"
#include <xc.inc>
#include "p18f-main.inc"

; Select the operator UART
#ifdef USB_CDC
#if OPERATOR_UART == 0
#define OPERATOR_TX  TX0
#define OPERATOR_RX  RX0
#define OPERATOR_RXQ RX0Q
#endif
#endif
#if OPERATOR_UART == 1
#define OPERATOR_TX  TX1_
#define OPERATOR_RX  RX1_
#define OPERATOR_RXQ RX1Q
#define OPERATOR_RX_IS_UART
#endif
#if OPERATOR_UART == 2
#define OPERATOR_TX  TX2_
#define OPERATOR_RX  RX2_
#define OPERATOR_RXQ RX2Q
#define OPERATOR_RX_IS_UART
#endif
#if OPERATOR_UART == 3
#define OPERATOR_TX  TX3_
#define OPERATOR_RX  RX3_
#define OPERATOR_RXQ RX3Q
#define OPERATOR_RX_IS_UART
#endif

; NVM register renaming
#ifndef K42
#define movff_ movff
#else
#define movff_ movffl
#define INTCON INTCON0
#define EEADR NVMADRL
#ifdef NVMADRH
#define EEADRH NVMADRH
#endif
#define EEDATA NVMDAT
#define EECON1 NVMCON1
#define EEPGD
#define CFGS 6
#ifdef NVMCON2
#define EECON2 NVMCON2
#endif
#ifdef NVMLOCK
#define EECON2 NVMLOCK
#endif
#endif

; UART register renaming
#ifdef TXREG1
#define TXREG TXREG1
#define SPBRG SPBRG1
#define SPBRGH SPBRGH1
#define TXSTA TXSTA1
#define RCSTA RCSTA1
#define RCREG RCREG1
#endif

#ifdef OERR1
#define OERR OERR1
#define CREN CREN1
#endif

#ifndef U1RXIF
#ifdef RCIF
#define U1RXIF RCIF
#else
#ifdef RC1IF
#define U1RXIF RC1IF
#endif
#endif
#endif
#ifndef U2RXIF
#ifdef RC2IF
#define U2RXIF RC2IF
#endif
#endif
#ifndef U3RXIF
#ifdef RC3IF
#define U3RXIF RC3IF
#endif
#endif
#ifndef U1RXB
#ifdef RCREG
#define U1RXB RCREG
#endif
#endif
#ifndef U2RXB
#ifdef RCREG2
#define U2RXB RCREG2
#endif
#endif
#ifndef U1TXB
#ifdef TXREG
#define U1TXB TXREG
#endif
#endif
#ifndef U2TXB
#ifdef TXREG2
#define U2TXB TXREG2
#endif
#endif
#ifndef U1TXIF
#ifdef TXIF
#define U1TXIF TXIF
#endif
#endif

#ifndef U2RXIE
#define WANT_UART2 DISABLE
#endif
#ifndef U3RXIE
#define WANT_UART3 DISABLE
#endif

; Status flags defintions
#ifndef CARRY
#define CARRY BANKMASK(STATUS), 0, a
#endif
#ifndef ZERO
#define ZERO BANKMASK(STATUS), 2, a
#endif

NF MACRO arg0,arg1
  db arg0|0x80
  IRPC char, arg1
  db 'char'
  ENDM
  align 2
ENDM NF
NF2 MACRO arg0,arg1
  db arg0|0x80
  IRPC char, arg1
  db 'char'
  ENDM
ENDM NF2
STRING MACRO arg0
  IRPC char, arg0
  db 'char'
  ENDM
ENDM STRING

;;   FSR0    Sp  - Parameter Stack Pointer
;;   FSR1    Tp  - Temporary Ram Pointer
;;   FSR2    Rp  - R stack pointer
;;   STKPTR      - Return Stack pointer for subroutine threading
;;               Note: The HW return stack is 31 cells deep
;;   FSR1L, FSR1H, TABLAT, TBLPTRL, TBLPTRH, PCLATH are used as interrupt
;;               safe temporary registers.
;; The parameter stack grows upwards
;; Parameter stack only contains 16 bit values. (= A cell is 16 bits)
;; When leaving a word the stack pointer points to
;; the Most Significant Byte (odd address)
;; Single precision arithmetic is 16 bit aritmetic.
;; A Character value on the stack is always 16 bits wide

Sptr    equ 0           ;; Only for lfsr
Sp      equ FSR0L       ;; current parameter stack pointer points
                        ;; to the MS byte of the 16 bit value
Sbank   equ FSR0H
Srw     equ INDF0       ;; read/write TOS
Sminus  equ POSTDEC0    ;; pop one byte
Splus   equ POSTINC0    ;;
plusS   equ PREINC0     ;; push one byte
SWrw    equ PLUSW0      ;; Offset to Sp in W

Tptr    equ 1           ;; Only for lfsr
Tp      equ FSR1L       ;; Temporary ram pointer
Tbank   equ FSR1H
Trw     equ INDF1       ;; Read/write
Tminus  equ POSTDEC1    ;; pop
Tplus   equ POSTINC1    ;; push
plusT   equ PREINC1
TWrw    equ PLUSW1      ;; Offset to Tp in W

Rptr    equ 2           ;; Only for lfsr
Rp      equ FSR2L       ;; R stack pointer
Rbank   equ FSR2H
Rrw     equ INDF2       ;; Read/write
Rminus  equ POSTDEC2    ;; pop
Rplus   equ POSTINC2    ;; push
plusR   equ PREINC2
RWrw    equ PLUSW2      ;; Offset to Rp in W

;; For Flow Control
XON_    equ 0x11
XOFF_   equ 0x13

CR_     equ 0x0d
LF_     equ 0x0a
BS_     equ 0x08

;; Flags for the Name Field
COMPILE equ 0x10
INLINE  equ 0x20
IMMED   equ 0x40
NFA     equ 0x80
NFAmask equ 0x0f

;; BANKMASK(FLAGS2)
fFC     equ 1           ;; 0=Flow Control, 1 = no Flow Control
ixoff   equ 0           ;; 1=XOFF has been sent

;; BANKMASK(FLAGS1)
noclear equ 6           ;; dont clear optimisation flags
idup    equ 5           ;; Use dupzeroequal instead of zeroequal
izeroeq equ 4           ;; Use bnz instead of bz if zeroequal
istream equ 3
fLOCK   equ 2           ;; Lock writes to flash and eeprom
fTAILC  equ 1           ;; Disable tailcall optimisation
idirty  equ 0           ;; Flash buffer dirty flag

;; Memory mapping prefixes
PRAM        equ RAM_LO      ;; h'f000' or h'c000'
PEEPROM     equ EEPROM_LO   ;; h'ec00' or h'bc00'

ussize      equ PARAMETER_STACK_SIZE
utibsize    equ TIB_SIZE + HOLD_SIZE


;;****************************************************
;; USE ACCESS BANK to R/W these registers
;; Internal variables used by asm code
PSECT   udata_acs;;,global,class=COMRAM,space=1,delta=1,lowdata,noexec
reset0:   ds 1
reset1:   ds 1
reset2:   ds 1
appdata:  ds 13      ;;
ms_count: ds 2       ;; millisecond counter 4 bytes
upcurr:   ds 2       ;; Current USER area pointer
p_lo:     ds 1       ;; P and DO LOOP INDEX
p_hi:     ds 1
ibase_lo: ds 1       ;; Memory address of ibuffer
ibase_hi: ds 1       ;; Memory address of ibuffer 0x15
#if XSTORE == ENABLE
ibase_up: ds 1       ;; Memory address of ibuffer
#endif
iaddr_lo: ds 1       ;; Instruction Memory access address
iaddr_hi: ds 1       ;; Instruction Memory access address
#if XSTORE == ENABLE
iaddr_up: ds 1       ;; Instruction Memory access address
#endif
FLAGS1:   ds 1       ;; Some flags
FLAGS2:   ds 1       ;; More flags
RXhead:   ds 1       ;; Head of serial RX interrupt buffer
RXtail:   ds 1       ;; Tail of serial RX interrupt buffer
RXcnt:    ds 1       ;; Number of characters in the RX fifo
#ifdef U2RXB
RX2head:  ds 1       ;; Head of serial RX interrupt buffer
RX2tail:  ds 1       ;; Tail of serial RX interrupt buffer
RX2cnt:   ds 1       ;; Number of characters in the RX fifo
#endif
#ifdef U3RXB
RX3head:  ds 1       ;; Head of serial RX interrupt buffer
RX3tail:  ds 1       ;; Tail of serial RX interrupt buffer
RX3cnt:   ds 1       ;; Number of characters in the RX fifo
#endif
cse:      ds 1       ;; Current data section 0=flash, 2=eeprom, 4=ram
state:    ds 1       ;; State value. Can only be changed by []
wflags:   ds 1       ;; Word flags from word header
fstatus:  ds 1       ;; if zero, cpu idle is allowed
irq_v:    ds 2       ;; Interrupt vector
areg:     ds 1       ;; A register
aregh:    ds 1
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
load_acc: ds 1       ;;
load_res: ds 3       ;; 256 ms load result
#endif
#endif
;; Interrupt low priority save variables
#ifndef K42
ilw:         ds 1
ilstatus:    ds 1
ilbsr:       ds 1
#endif

;; Interrupt high priority save variables
ihpclath:    ds 1
ihtablat:    ds 1
ihtblptrl:   ds 1
ihtblptrh:   ds 1
#ifndef K42
ihsp:        ds 1
ihsbank:     ds 1
ihprodl:     ds 1
ihprodh:     ds 1
#endif
ihtp:        ds 1
ihtbank:     ds 1

#ifdef p18fxx2xx8_fix_1
SINTCON:     ds 1       ;; Save INTCON before disabling interrupts
SPIE1:       ds 1       ;; Save PIE1 before disabling interrupts
SPIE2:       ds 1       ;; Save PIE2 before disabling interrupts
#endif

#ifdef USB_CDC
#if USB_BANK == 2
PSECT udata_bank2
#else
PSECT udata_bank4
#endif
ep0ostat:	ds 1
ep0ocnt: 	ds 1
ep0oadr:	ds 2
ep0istat: ds 1
ep0icnt: 	ds 1
ep0iadr:	ds 2

ep1ostat:	ds 1
ep1ocnt: 	ds 1
ep1oadr:	ds 2
ep1istat:	ds 1
ep1icnt: 	ds 1
ep1iadr:	ds 2

ep2ostat:	ds 1
ep2ocnt: 	ds 1
ep2oadr:	ds 2
ep2istat:	ds 1
ep2icnt: 	ds 1
ep2iadr:	ds 2

; USB Device States - To be used with usb_device_state
usb_device_state: ds 1
#define DETACHED_STATE          0
#define ATTACHED_STATE          0
#define POWERED_STATE           0
#define DEFAULT_STATE           1
#define ADR_PENDING_STATE       3
#define ADDRESS_STATE           0x07
#define ADDRESS_STATE2          0x83  ; Shifted to h'07'
#define CONFIGURED_STATE        0x0f

; BUFFERS
line_coding: ds 7
ep0buf:      ds 8
cdc_data_rx: ds 8
cdc_data_tx: ds 8

#define TIB_DEFINED
wordlenchar: ds 1
utibbuf:     ds utibsize

#define USBUF_DEFINED
usbuf:       ds ussize

#define RXBUF_DEFINED
irq_s0:      ds PARAMETER_STACK_SIZE_IRQ   ;; Interrupt parameter stack.
RXbuf:       ds RX_BUF_SIZE
RXbufmask   equ RX_BUF_SIZE - 1
#else        // NOT USB
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
#endif       // USB_CDC

;; The flash buffer area
#ifdef USB_CDC
#if USB_BANK == 4
psect udata_bank7
flash_buf:   ds flash_block_size
#else
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
flash_buf:   ds flash_block_size
#endif
#else
#ifndef Qxx
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
flash_buf:   ds flash_block_size
#else
#define flash_buf FBUFFER
#endif
#endif

#ifndef RXBUF_DEFINED
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
irq_s0:      ds PARAMETER_STACK_SIZE_IRQ   ;; Interrupt parameter stack.
RXbuf:       ds RX_BUF_SIZE
RXbufmask   equ RX_BUF_SIZE - 1
#endif

;; FORTH memory allocation cache variables
#ifdef USB_CDC
#if USB_BANK == 4
psect udata_bank4
#else
psect udata_bank2
#endif
#else
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
#endif

dpSTART:     ds 2
dpFLASH:     ds 2
dpEEPROM:    ds 2
dpRAM:       ds 2
dpLATEST:    ds 2

#ifdef USB_CDC
#if USB_BANK == 4
psect udata_bank7
#else
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
#endif
#else
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
#endif

#if WANT_UART2 == ENABLE
RX2buf:       ds RX2_BUF_SIZE
RX2bufmask   equ RX2_BUF_SIZE - 1
#endif
#if WANT_UART3 == ENABLE
RX3buf:       ds RX3_BUF_SIZE
RX3bufmask   equ RX3_BUF_SIZE - 1
#endif

;; USER AREA for the OPERATOR task
userarea:
us0         equ -26          ;; Start of parameter stack
uemit       equ -24
ukey        equ -22
ukeyq       equ -20
utask       equ -18
ubase       equ -16
utib        equ -14
uflg        equ -12           ;; ACCEPT true =  CR has been received
ustatus     equ -11
uhp         equ -10
usource     equ -8           ;; Two cells
utoin       equ -4
ulink       equ -2           ;; Link to next task
urptr       equ 0            ;; Top of the saved return stack
uvars:       ds -us0 + 2
u0:          ds 2 + UADDSIZE
urbuf:       ds RETURN_STACK_SAVE_SIZE
#ifndef USBUF_DEFINED
usbuf:       ds ussize
#endif
#ifndef TIB_DEFINED
wordlenchar: ds 1
utibbuf:     ds utibsize
#endif

;; Start of free ram
psect udatabig,class=BIGRAM,space=SPACE_DATA,noexec
dpdata:      ds 2

;; Variables in EEPROM
beeprom     equ PEEPROM
dp_start    equ beeprom + 0  ;; Deferred TURNKEY execution vector
dp_flash    equ beeprom + 2  ;; FLASH dictionary pointer
dp_eeprom   equ beeprom + 4  ;; EEPROM dictionary pointer
dp_ram      equ beeprom + 6  ;; RAM dictionary pointer
latest      equ beeprom + 8  ;; Pointer to latest dictionary word
prompt      equ beeprom + 10 ;; Deferred prompt action
dpeeprom    equ beeprom + 12

;;**************************************************
;; Ensure the EEPROM is intialized even if the
;; eeprom is not erased when programming the chip
PSECT   edata
        db  0xff, 0xff

;; Code **********************************************
;   Reset vector
    PSECT   resetVec,global,reloc=2,class=CODE,delta=1
resetVec:
        nop                     ;; 18f252/18f258 ERRATA
        goto    main
;;;;***************************************************
;   High priority interrupt vector
    PSECT   hi_int,global,reloc=2,class=CODE,delta=1
#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
#ifndef K42
        bsf     TMR0ON
#else
        banksel T0CON0
        bsf     T0EN
#endif
#endif
#endif
#endif
;; irq_v must be initialized before any high priority interrupt is enabled.
;; ' interrupt-routine 0 int!
irq_user:
        push
        movf    BANKMASK(irq_v)+1, W, A
        movwf   TOSH, A
        movf    BANKMASK(irq_v), W, A
        movwf   TOSL, A
        return              ;; Now the interrupt routine is executing
;; *******************************************
;; ****** LOW PRIORITY INTERRUPT
;; Timer 0 interrupt must be marked low priority
;   Low priority interrupt vector and handler
    PSECT   lo_int,global,reloc=2,class=CODE,delta=1
#ifndef K42
        movwf   BANKMASK(ilw), A
        movff   STATUS, ilstatus
        movff   BSR, ilbsr
#endif
#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
#ifndef K42
        bsf     TMR0ON
        btfss   TMR0IF
        bra     irq_t0_end
        bcf     TMR0IF
        incf    BANKMASK(load_acc), A
        bra     irq_end
#else
        banksel T0CON0
        bsf     T0EN
        banksel PIR3
        btfss   TMR0IF
        bra     irq_t0_end
        bcf     TMR0IF
        incf    BANKMASK(load_acc), A
        retfie  1
#endif
irq_t0_end:
#endif
#endif
#endif
;; The tick counter interrupt and UART interrupt must be marked as low priority
irq_low:
;;;; 1 millisecond tick counter
irq_ms:
#if MS_TMR == 1
#ifdef REG_TMR1IF
        banksel REG_TMR1IF
#endif
        btfss   TMR1IF
        bra     irq_ms_end
        bcf     TMR1IF
        banksel TMR1L
        movlw   low(tmr1ms_val)
        subwf   TMR1L, F, B
        movlw   high(tmr1ms_val)
        subwfb  TMR1H, F, B
#endif
#if MS_TMR == 2
#ifdef REG_TMR2IF
        banksel REG_TMR2IF
#endif
        btfss   TMR2IF
        bra     irq_ms_end
        bcf     TMR2IF
#endif
#if MS_TMR == 3
#ifdef REG_TMR3IF
        banksel REG_TMR3IF
#endif
        btfss   TMR3IF
        bra     irq_ms_end
        bcf     TMR3IF
        banksel TMR3L
        movlw   low(tmr1ms_val)
        subwf   TMR3L, F, B
        movlw   high(tmr1ms_val)
        subwfb  TMR3H, F, B
#endif
#if MS_TMR == 4
#ifdef REG_TMR4IF
        banksel REG_TMR4IF
#endif
        btfss   TMR4IF
        bra     irq_ms_end
        bcf     TMR4IF
#endif
#if MS_TMR == 5
#ifdef REG_TMR5IF
        banksel REG_TMR5IF
#endif
        btfss   TMR5IF
        bra     irq_ms_end
        bcf     TMR5IF
        banksel TMR5L
        movlw   low(tmr1ms_val)
        subwf   TMR5L, F, B
        movlw   high(tmr1ms_val)
        subwfb  TMR5H, F, B
#endif
#if MS_TMR == 6
#ifdef REG_TMR6IF
        banksel REG_TMR6IF
#endif
        btfss   TMR6IF
        bra     irq_ms_end
        bcf     TMR6IF
#endif
        movlw   TICK_TIME
        addwf   BANKMASK(ms_count), F, A
        movlw   0
        addwfc  BANKMASK(ms_count)+1, F, A
#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
        movf    BANKMASK(ms_count), W, A
        bnz     irq_load_end
        movf    TMR0L, W, A
        movwf   BANKMASK(load_res), A
        movf    TMR0H, W, A
        clrf    TMR0H, A
        clrf    TMR0L, A
        movwf   BANKMASK(load_res)+1, A
        movf    BANKMASK(load_acc), W, A
        movwf   BANKMASK(load_res)+2, A
        clrf    BANKMASK(load_acc), A
#endif
#endif
#endif
irq_load_end:
#ifndef K42
        bra     irq_end
#else // K42 K83
        retfie 1
#endif
irq_ms_end:
;; ************************************************
;; UART RX interrupt routine
;; Feeds the input buffer with characters
;; from the serial line
irq_async_rx1:
        banksel REG_U1RXIF
        btfss   U1RXIF
        bra     irq_async_rx1_end
#ifndef K42
        movf    Tp, W, A
        movwf   BANKMASK(ihtp), A
        movf    Tbank, W, A
        movwf   BANKMASK(ihtbank), A
#endif
        bsf     BANKMASK(FLAGS1), istream, A      ;; Indicate input stream activity to FLASH write routine
        movf    BANKMASK(RXcnt), W, A
#if OPERATOR_UART == 1
#if FC_TYPE == XONXOFF
        addlw   0xff - RX_OFF_FILL
        bnc     irq_async_rx1_2
        btfss   BANKMASK(FLAGS2), fFC, A
        call    XXOFF
#endif
#if FC_TYPE == CTS
        addlw   0xff - RX_OFF_FILL
        bnc     irq_async_rx1_2
        btfss   BANKMASK(FLAGS2), fFC, A
        bsf     HW_FC_CTS_PORT, HW_FC_CTS_PIN, A
#endif
#endif
irq_async_rx1_2:
        lfsr    Tptr, RXbuf
        movf    BANKMASK(RXhead), W, A
        movff_  U1RXB, TWrw
#ifndef K42
        btfsc   OERR
        bcf     CREN ;; Restart RX on case of RX overrun
        bsf     CREN
#else
        banksel U1ERRIR
        bcf     U1RXFOIF
#endif
        movf    TWrw, W, A
        sublw   0x0f                    ;; ctrl-o
#if OPERATOR_UART == 1
#if CTRL_O_WARM_RESET == ENABLE
        bnz     irq_async_rx1_3
        reset                           ;; Make a warm start
#endif
irq_async_rx1_3:
#if FC_TYPE == XONXOFF
        addlw   0x04                    ;; ctrl-s, xoff 0x13, 0xf - 0x13 + 0x4 = 0
        btfss   BANKMASK(FLAGS2), fFC, A          ;; receive xoff if FC is off
        bz      irq_async_rx1_5         ;; Do not receive  xoff
#endif
#endif
irq_async_rx1_4:
        incf    BANKMASK(RXhead), F, A
        movlw   RXbufmask               ;; Wrap the RXhead pointer.
        andwf   BANKMASK(RXhead), F, A
        incf    BANKMASK(RXcnt), F, A
irq_async_rx1_5:
#ifndef K42
        movf    BANKMASK(ihtbank), W, A
        movwf   Tbank, A
        movf    BANKMASK(ihtp), W, A
        movwf   Tp, A
#endif
        bra     irq_end
irq_async_rx1_end:

;; UART 2
#if WANT_UART2 == ENABLE
irq_async_rx2:
        banksel REG_U2RXIF
        btfss   U2RXIF
        bra     irq_async_rx2_end
#ifndef K42
        movf    Tp, W, A
        movwf   BANKMASK(ihtp), A
        movf    Tbank, W, A
        movwf   BANKMASK(ihtbank), A
#endif
        bsf     BANKMASK(FLAGS1), istream, A      ;; Indicate input stream activity to FLASH write routine
        movf    BANKMASK(RX2cnt), W, A
        addlw   0xff - RX_OFF_FILL
        bnc     irq_async_rx2_2
#if OPERATOR_UART == 2
#if FC_TYPE == XONXOFF
        btfss   BANKMASK(FLAGS2), fFC, A
        call    XXOFF
#endif
#if FC_TYPE == CTS
        btfss   BANKMASK(FLAGS2), fFC
        bsf     HW_FC_CTS_PORT, HW_FC_CTS_PIN
#endif
#endif
irq_async_rx2_2:
        lfsr    Tptr, RX2buf
        movf    BANKMASK(RX2head), W, A

        movff_   U2RXB,  TWrw
#ifndef K42
        movff   BSR, ilbsr
        banksel RCSTA2
        btfsc   RCSTA2, OERR2, B
        bcf     RCSTA2, CREN2, B ;; Restart RX on case of RX overrun
        bsf     RCSTA2, CREN2, B
        movff   ilbsr, BSR
#else
        banksel U2ERRIR
        bcf     U2RXFOIF
#endif
        movf    TWrw, W, A
        sublw   0x0f                    ;; ctrl-o
#if OPERATOR_UART == 2
#if CTRL_O_WARM_RESET == ENABLE
        bnz     irq_async_rx2_3
        reset                           ;; Make a warm start
#endif
irq_async_rx2_3:
#if FC_TYPE == XONXOFF
        addlw   0x04                     ;; ctrl-s, xoff 0x13, 0xf - 0x13 + 0x4 = 0
        btfss   BANKMASK(FLAGS2), fFC, A ;; receive xoff if FC is off
        bz      irq_async_rx2_5          ;; Do not receive  xoff
#endif
#endif
irq_async_rx2_4:
        incf    BANKMASK(RX2head), F, A
        movlw   RXbufmask               ;; Wrap the RXhead pointer.
        andwf   BANKMASK(RX2head), F, A
        incf    BANKMASK(RX2cnt), F, A
irq_async_rx2_5:
#ifndef K42
        movf    BANKMASK(ihtbank), W, A
        movwf   Tbank, A
        movf    BANKMASK(ihtp), W, A
        movwf   Tp, A
#endif
        bra     irq_end
irq_async_rx2_end:
#endif
;; UART 3
#if WANT_UART3 == ENABLE
irq_async_rx3:
        banksel REG_U3RXIF
        btfss   U3RXIF
        bra     irq_async_rx3_end
        bsf     BANKMASK(FLAGS1), istream, A      ;; Indicate input stream activity to FLASH write routine
        movf    BANKMASK(RX3cnt), W, A
        addlw   0xff - RX_OFF_FILL
        bnc     irq_async_rx3_2
#if OPERATOR_UART == 3
#if FC_TYPE == XONXOFF
        btfss   BANKMASK(FLAGS2), fFC, A
        call    XXOFF
#endif
irq_async_rx3_1:
#if FC_TYPE == CTS
        btfss   BANKMASK(FLAGS2), fFC
        bsf     HW_FC_CTS_PORT, HW_FC_CTS_PIN
#endif
#endif
irq_async_rx3_2:
        lfsr    Tptr, RX3buf
        movf    BANKMASK(RX3head), W, A

        movff_  U3RXB,  TWrw
#ifndef K42
        movff   BSR, ilbsr
        banksel RCSTA3
        btfsc   RCSTA3, OERR2, B
        bcf     RCSTA3, CREN2, B ;; Restart RX on case of RX overrun
        bsf     RCSTA3, CREN2, B
        movff   ilbsr, BSR
#else
        banksel U3ERRIR
        bcf     U3RXFOIF
#endif
        movf    TWrw, W, A
        sublw   0x0f                    ;; ctrl-o
#if OPERATOR_UART == 3
#if CTRL_O_WARM_RESET == ENABLE
        bnz     irq_async_rx3_3
        reset                           ;; Make a warm start
#endif
irq_async_rx3_3:
#if FC_TYPE == XONXOFF
        addlw   0x04                    ;; ctrl-s, xoff 0x13, 0xf - 0x13 + 0x4 = 0
        btfss   BANKMASK(FLAGS2), fFC, A          ;; receive xoff if FC is off
        bz      irq_async_rx3_end        ;; Do not receive  xoff
#endif
#endif
irq_async_rx3_4:
        incf    BANKMASK(RX3head), F, A
        movlw   RXbufmask               ;; Wrap the RXhead pointer.
        andwf   BANKMASK(RX3head), F, A
        incf    BANKMASK(RX3cnt),  F, A
irq_async_rx3_end:
#endif
;; *****************************************************************
;;;; Restore Tp and Tbank
irq_end:
#ifndef K42
        movf    BANKMASK(ilw), W, A
        movff   ilstatus, STATUS
        movff   ilbsr, BSR
        retfie  0
#else // K42 K83 Qxx
        retfie  1
#endif
;; *******************************************************************
;; WARM user area data
warmlitsize equ 28
WARMLIT:
        dw      u0+PRAM        ;; UP
        dw      usbuf+(PRAM-1) ;; S0
        dw      OPERATOR_TX    ;; EMIT vector
        dw      OPERATOR_RX    ;; KEY vector
        dw      OPERATOR_RXQ   ;; KEY? vector
        dw      OPERATOR_AREA  ;; TASK vector
        dw      DEFAULT_BASE   ;; BASE
        dw      utibbuf+(PRAM) ;; TIB
        dw      0              ;; ustatus & uflg
        dw      0,0,0,0
        dw      0;; u0+0xf000  ;; ulink
        dw      urbuf+PRAM     ;; urptr

;; **************************************

;; EMPTY dictionary data
STARTV: dw      0000
DPC:    dw      (dpcode+0x100)&0xff00
DPE:    dw      dpeeprom
DPD:    dw      dpdata+PRAM
LW:     dw      lastword
STAT:   dw      DOTSTATUS
;; *******************************************************************
    PSECT   code
;; EXIT --   Compile a return
;;        variable link
        dw      0
L_EXIT:
        NF 4,exit
EXIT:
        decf    STKPTR, F, A     ;; pop does not work on K42 and K83
        return

;;; *******************************************************
; PAUSE  --     switch task
; 188 us @ 2 MHz, 9.4 us @ 40 Mhz  7.8us at 48 Mhz  ( 93 clocks with 2 cells on return stack )
; save stack to current uarea, link -> up, restore stack
        dw      L_TURNKEY
L_PAUSE:
        NF 5,pause
PAUSE:
        clrwdt
#ifdef USB_CDC
        movf    BANKMASK(upcurr), W, A
        sublw   low(u0)           ; check usb only in operator task
        bnz     PAUSE_USB_CDC_END ; Lowers execution cpu load when many tasks are running
        btfss   USBEN
        bra     PAUSE_USB_CDC_END
        call    USBDriverService
        movf    ep2icount, W, A
        bz      PAUSE_USB_CDC_END
        movf    ep2itmo, W, A
        subwf   BANKMASK(ms_count), W, A
        bn      PAUSE_USB_CDC_END
        rcall   TX0_SEND
PAUSE_USB_CDC_END:
#endif
#ifdef IDLEN
#if IDLE_MODE == ENABLE
        movf    BANKMASK(fstatus), W, A  ; idle allowed ?
        bz      PAUSE_IDLE1
        movf    BANKMASK(upcurr), W, A
        sublw   low(u0)        ; Sleep only in operator task
        bnz     PAUSE000       ; Lowers execution delay when many tasks are running
#if CPU_LOAD_LED == ENABLE
        banksel CPU_LOAD_PORT
#if CPU_LOAD_LED_POLARITY == POSITIVE
        bcf     CPU_LOAD_PORT, CPU_LOAD_BIT, B
#else
        bsf     CPU_LOAD_PORT, CPU_LOAD_BIT, B
#endif
#endif
#if CPU_LOAD == ENABLE
#ifndef K42
        bcf     TMR0ON          ; TMR0 Restart in interrupt routine
#else
        banksel T0CON0
        bcf     T0EN            ; TMR0 Restart in interrupt routine
#endif
#endif
        sleep
PAUSE_IDLE1:
#if CPU_LOAD_LED == ENABLE
        banksel CPU_LOAD_PORT
#if CPU_LOAD_LED_POLARITY == POSITIVE
        bsf     CPU_LOAD_PORT, CPU_LOAD_BIT, B
#else
        bcf     CPU_LOAD_PORT, CPU_LOAD_BIT, B
#endif
#endif
#endif
#endif
PAUSE000:
        ; Set user pointer in Tp, Tbank (FSR1)
        movf    BANKMASK(upcurr), W, A
        movwf   Tp, A
        movf    (BANKMASK(upcurr)+1), W, A
        movwf   Tbank, A

        ;; Switch tasks only if some background tasks are running
        movf    Tminus, W, A   ; Point to high(ulink)
        movf    Tminus, W, A   ; check ulink
        bz      PAUSE_RET

        ; Save parameter stack pointer
        movf    Sp, W, A
        movwf   plusR, A
        movf    Sbank, W, A
        movwf   plusR, A

        ; Save P pointer
        movf    BANKMASK(p_lo), W, A
        movwf   plusR, A
        movf    BANKMASK(p_hi), W, A
        movwf   plusR, A

        ; Remember the return stack counter
        movf    STKPTR, W, A
        movwf   TBLPTRL, A           ; 17

        ; Save the return stack
pause1:
        movf    TOSL, W, A
        movwf   plusR, A
        movf    TOSH, W, A
        movwf   plusR, A
        decfsz  STKPTR, F, A
        bra     pause1            ; 13 2 items

        ; Save the return stack counter
        movf    TBLPTRL, W, A
        movwf   plusR, A

        ; Move to the next user area
        movf    Tplus, W, A
        movwf   BANKMASK(upcurr), A
        movf    Tplus, W, A
        movwf   (BANKMASK(upcurr)+1), A

        ; Save the return stack pointer to urptr
        movf    Rp, W, A
        movwf   Tplus, A
        movf    Rbank, W, A
        movwf   Tplus, A

        ; Put new user pointer in Tp, Tbank
        movf    BANKMASK(upcurr), W, A
        movwf   Tp, A
        movf    (BANKMASK(upcurr)+1), W, A
        movwf   Tbank, A

        ; Set the return stack restore pointer  in Rp
        movf    Tplus, W, A
        movwf   Rp, A
        movf    Tminus, W, A
        movwf   Rbank, A

        ; Set the return stack counter
        movf    Rminus, W, A
        movwf   TBLPTRL, A             ; 22

        ; Restore the return stack
pause2:
        push
        movf    Rminus, W, A
        movwf   TOSH, A
        movf    Rminus, W, A
        movwf   TOSL, A

        decfsz  TBLPTRL, F, A
        bra     pause2              ; 15 2 items

        ; Restore the P pointer
        movf    Rminus, W, A
        movwf   BANKMASK(p_hi), A
        movf    Rminus, W, A
        movwf   BANKMASK(p_lo), A

        ; Restore the parameter stack pointer
        movf    Rminus, W, A
        movwf   Sbank, A
        movf    Rminus, W, A
        movwf   Sp, A
PAUSE_RET:
        return              ; 23+2+2=27

        dw      L_IR
L_WARM:
        NF 4,warm
WARM_:
#ifndef USB_CDC
        reset                   ; Perform a reset, jumps to 0000 and resets stuff
#endif
main:
#ifndef K42
        movlw   0xf
        iorwf   ADCON1, F, A
#endif
        clrf    TBLPTRU, A
#ifdef OSCCON
        movlw   0x70
        movwf   OSCCON, A
#endif
#ifdef OSCCON1_VALUE
        banksel OSCCON1
        movlw   OSCCON1_VALUE    ; HFINTOSC SDIV = 1
        movwf   OSCCON1, B  ; K83
        movlw   OSCFRQ_VALUE     ; 4 MHz
        movwf   OSCFRQ, B
        movlw   OSCTUNE_VALUE
        movwf   OSCTUNE, B
#endif

#ifdef PLL_ENABLE
#if PLL_ENABLE == 1
        movlw   0x40
        movwf   OSCTUNE
#endif
#endif
                              ; Clear ram
WARM:
#ifndef K42
        movf    STKPTR, W, A
        movwf   BANKMASK(reset0), A ; Save return stack reset reasons
        movf    RCON, W, A
        movwf   BANKMASK(reset1), A ; Save reset reasons
        movf    BANKMASK(fstatus), W, A
        movwf   BANKMASK(reset2), A
        movlw   0x9f
        movwf   BANKMASK(RCON), A
#else
        movf    PCON0, W, A
        movwf   BANKMASK(reset0), A ; Save reset reasons
        movwf   BANKMASK(reset1), A ; Save reset reasons
        movf    BANKMASK(fstatus), W, A
        movwf   BANKMASK(reset2), A
        movlw   0x3f            ; Clearing the flags in PCON0
        banksel PCON0
        movwf   PCON0, A
#endif
        clrf    STKPTR, A       ; Clear return stack

        lfsr    Sptr, appdata   ; Zero ram from 3 upwards

#ifdef USB_CDC
        lfsr    Tptr, usb_device_state+1
WARM_ZERO_1:
        clrf    Splus, A
        movf    Sbank, W, A
        subwf   Tbank, W, A
        bnz     WARM_ZERO_1
WARM_ZERO_3:
        clrf    Tplus, A
        movf    Tbank, W, A
        sublw   0x0f
        bnz     WARM_ZERO_3
#else
WARM_ZERO_1:
        clrf    Splus, A
        movf    Sbank, W, A
        sublw   0x0f
        bnz     WARM_ZERO_1
#endif
        setf    BANKMASK(ibase_hi), A   ; Mark flash buffer empty

        lfsr    Sptr, (usbuf-1)         ; Initalise Parameter stack
        lfsr    Rptr, urbuf

        banksel PIE1
#ifdef PIE0                     // Disable all peripheral interrupts
        clrf    PIE0, B
#endif
        clrf    PIE1, B    ; Disable all peripheral interrupts
        clrf    PIE2, B
#ifdef PIE3
        clrf    PIE3, B
#endif
#ifdef PIE4
        clrf    PIE4, B
#endif
#ifdef PIE5
        clrf    PIE5, B
#endif
#ifdef PIE6
        clrf    PIE6, B
#endif
#ifdef PIE7
        clrf    PIE7, B
#endif
#ifdef PIE8
        clrf    PIE8, B
#endif
#ifdef PIE9
        clrf    PIE9, B
#endif
#ifdef PIE10
        clrf    PIE10, B
#endif
        banksel TOSU   ; SFR bank

#ifndef K42
;; UART == 1  ----------------------------------------------
#ifdef BRG16
        bsf     BRG16
        movlw   high(spbrgvalx4)
        movwf   SPBRGH, A
        movlw   low(spbrgvalx4)
#else
        movlw   spbrgval
#endif
        movwf   SPBRG, A
; TX enable
        movlw   00100100B
        movwf   TXSTA, A
#ifdef USB_CDC
        movlw   00000000B    ; Reset the UART since
        movwf   RCSTA, A     ; USB warm start does not reset the chip
#endif
; RX enable
        movlw   10010000B
        movwf   RCSTA, A
        bsf     RCIE
        bcf     RCIP
#ifdef ANSELH
#ifdef ANS11
        bcf     ANS11         ; Enable digital RB5 for RX
#endif
#endif
#ifdef ANSELC
#ifdef ANSC7
        bcf     ANSC7         ; Enable digital RC7 for RX
#endif
#endif
;; UART == 2 ---------------------------------------
#if WANT_UART2 == ENABLE
#ifdef BRG16
        bsf     BAUDCON2, BRG16
        movlw   high(spbrgvalx4)
        movwf   SPBRGH2
        movlw   spbrg2valx4
#else
        movlw   spbrg2val
#endif
        movwf   SPBRG2, B
; TX enable
        movlw   b'00100100'
        movwf   TXSTA2, B
; RX enable
        movlw   b'10010000'
        movwf   RCSTA2, B
        bsf     PIE3, RC2IE
        bcf     IPR3, RC2IP

        bcf     ANCON2, ANSEL18, B ; Configure RX pin as input
#endif
#else  // K42 K83 Qxx
;; UART == 1
; PPS configure pins for RX and TX
        banksel RX_ANSEL
        bcf     RX_ANSEL, RX_BIT, B    ; disable analogue on PORTx so RX can function
        bcf     TX_ANSEL, TX_BIT, B    ; disable analogue on PORTx so TX can function
        bcf     TX_TRIS, TX_BIT, A
        bsf     TX_LAT, TX_BIT, A
; Unlock the PPS
        call    PPS_UNLOCK
; Set the pins
        banksel U1RXPPS
        movlw   RX_PPS
        movwf   U1RXPPS, B

        clrf    U1CTSPPS, B

        movlw   TX_CODE
        movwf   TX_PPS, B

; Re-lock the PPS
        call    PPS_LOCK

; Set the Baud Rate
        movlw   low(spbrgvalx4)        ; ((clock/baud)/d'4') - 1
        banksel U1BRGL
        movwf   U1BRGL, B
        movlw   high(spbrgvalx4)
        movwf   U1BRGH, B

; TX enable
        movlw   10110000B       ; HIGH SPEED BAUD RATE / NO AUTO DETECT BOARD /
                                ; ENABLE TX / ENABLE RX / ASYNC 8 BIT MODE
        movwf   U1CON0, B
        bsf     U1RUNOVF
        bsf     U1ON            ; turn on TX

; RX enable
        banksel REG_U1RXIE
        bsf     U1RXIE              ; enable RX interupt
        banksel REG_U1RXIP
        bcf     U1RXIP              ; low priority
        bsf     RX_TRIS, RX_BIT, A  ; configure XY as an input

;; UART == 2 ---------------------------------------
#if WANT_UART2 == ENABLE
; PPS configure pins for RX and TX
        banksel RX2_ANSEL
        bcf     RX2_ANSEL, RX2_BIT, B    ; disable analogue on PORTx so RX can function
        banksel TX2_ANSEL
        bcf     TX2_ANSEL, TX2_BIT, B    ; disable analogue on PORTx so TX can function
; Unlock the PPS
        call    PPS_UNLOCK
; Set the pins
        banksel U2RXPPS
        movlw   RX2_PPS
        movwf   U2RXPPS, B        ; configure the RX pin to XY

        banksel U2CTSPPS
        clrf    U2CTSPPS, B        ; clear so always disabled

        banksel TX2_PPS          ; configure TX pin to XY
        movlw   TX2_CODE
        movwf   TX2_PPS, B

; Re-lock the PPS
        call    PPS_LOCK

; Set the Baud Rate
        movlw   low(spbrg2valx4)        ; ((clock/baud)/d'4') - 1
        banksel U2BRGL
        movwf   U2BRGL, B
        movlw   high(spbrg2valx4)
        movwf   U2BRGH, B

; TX enable
        movlw   10110000B     ; HIGH SPEED BAUD RATE / NO AUTO DETECT BOARD /
                                ; ENABLE TX / ENABLE RX / ASYNC 8 BIT MODE
        movwf   U2CON0, B
        bsf     U2RUNOVF
        bsf     U2ON            ; turn on TX
; RX enable
        banksel REG_U2RXIE
        bsf     U2RXIE              ; enable RX interupt
        banksel REG_U2RXIP
        bcf     U2RXIP              ; low priority

        banksel RX2_TRIS
        bsf     RX2_TRIS, RX2_BIT, B ; configure RX pin as an input

#endif
;; UART == 3 ---------------------------------------
#if WANT_UART3 == ENABLE
; PPS configure pins for RX and TX
        banksel RX3_ANSEL
        bcf     RX3_ANSEL, RX3_BIT, B    ; disable analogue on PORTx so RX can function
        banksel TX3_ANSEL
        bcf     TX3_ANSEL, TX3_BIT, B    ; disable analogue on PORTx so TX can function
; Unlock the PPS
        call    PPS_UNLOCK
; Set the pins
        banksel U3RXPPS
        movlw   RX3_PPS
        movwf   U3RXPPS, B        ; configure the RX pin to XY

        banksel U3CTSPPS
        clrf    U3CTSPPS, B        ; clear so always disabled

        banksel TX3_PPS          ; configure TX pin to XY
        movlw   TX3_CODE
        movwf   TX3_PPS, B

; Re-lock the PPS
        call    PPS_LOCK

; Set the Baud Rate
        movlw   low(spbrg3valx4)        ; ((clock/baud)/d'4') - 1
        banksel U3BRGL
        movwf   U3BRGL, B
        movlw   high(spbrg3valx4)
        movwf   U3BRGH, B

; TX enable
        movlw   10110000B     ; HIGH SPEED BAUD RATE / NO AUTO DETECT BOARD /
                                ; ENABLE TX / ENABLE RX / ASYNC 8 BIT MODE
        movwf   U3CON0, B
        bsf     U3RUNOVF
        bsf     U3ON            ; turn on TX
; RX enable
        banksel REG_U3RXIE
        bsf     U3RXIE              ; enable RX interupt
        banksel REG_U3RXIP
        bcf     U3RXIP              ; low priority

        banksel RX3_TRIS
        bsf     RX3_TRIS, RX3_BIT, B ; configure RX pin as an input
#endif
#endif

#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD_LED == ENABLE
        banksel CPU_LOAD_TRIS
        bcf     CPU_LOAD_TRIS, CPU_LOAD_BIT, B
#endif
#ifndef K42
        bsf     IDLEN           ; Only IDLE mode supported
#else
        banksel CPUDOZE
        bsf     IDLEN           ; Only IDLE mode supported
#endif
#endif
#endif

#if CPU_LOAD == ENABLE
#ifndef K42
        movlw   08                  ; TMR0 used for CPU_LOAD
        movwf   T0CON               ; prescale = 1
        bsf     TMR0IE
        bcf     TMR0IP
#else
        banksel T0CON0
        bsf     T0MD16              ; 16 bit timer
        movlw   0x40                ; TMR0 used for CPU_LOAD
        movwf   T0CON1, B           ; Instruction clock 1:1
        banksel PIE3
        bsf     TMR0IE
        banksel IPR3
        bcf     TMR0IP              ; Low priority
#endif
#endif

#if MS_TMR == 1                 // Timer 1 for 1 ms system tick
#ifndef K42
        movlw   01              ; Fosc/4,prescale = 1, 8-bit write
        movwf   T1CON, A
        bsf     TMR1IE
        bcf     TMR1IP
#else
        banksel T1CON
        movlw   01              ; fosc/4
        movwf   T1CLK, B
        movlw   01              ; Fosc/4,prescale = 1, 8-bit write
        movwf   T1CON, B
#ifndef Qxx
        banksel PIE4
#else
        banksel PIE3
#endif
        bsf     TMR1IE
#ifndef Qxx
        banksel IPR4
#else
        banksel IPR3
#endif
        bcf     TMR1IP
#endif
#endif
#if MS_TMR == 2                 // Timer 2 for 1 ms system tick
#ifndef K42
        movlw   0x7d            ; Prescale = 4, Postscale = 16
        movwf   T2CON
        movlw   tmr2ms_val
        movwf   PR2
        bsf     TMR2IE
        bcf     TMR2IP
#else
        movlw   01
        movwf   T2CLKCON
        movlw   tmr2ms_val
        movwf   T2PR, A
        movlw   0xaf         ; Prescale = 4, Postscale = 16
        movwf   T2CON
        banksel PIE4
        bsf     TMR2IE
        bcf     TMR2IP
#endif
#endif
#if MS_TMR == 3                 // Timer 3 for 1 ms system tick
#ifndef K42
        movlw   01              ; Fosc/4,prescale = 1, 8-bit write
        movwf   T3CON
        bsf     TMR3IE
        bcf     TMR3IP
#else
        movlw   01              ; Fosc/4,prescale = 1, 8-bit write
        movwf   T3CON
        movlw   01              ; fosc/4
        movwf   T3CLK
        banksel PIE6
        bsf     TMR3IE
        bcf     TMR3IP
#endif
#endif
#if MS_TMR == 4                 // Timer 4 for 1 ms system tick
#ifndef K42
        banksel T4CON
        movlw   0x7d            ; Prescale = 4, Postscale = 16
        movwf   T4CON, B
        movlw   tmr2ms_val
        movwf   PR4, B
        bsf     TMR4IE
        bcf     TMR4IP
#else
        movlw   01
        movwf   T4CLKCON
        movlw   tmr2ms_val
        movwf   T4PR, A
        movlw   0xaf         ; Prescale = 4, Postscale = 16
        movwf   T4CON
        banksel PIE7
        bsf     TMR4IE
        bcf     TMR4IP
#endif
#endif
#if MS_TMR == 5                 // Timer 5 for 1 ms system tick
#ifndef K42
        banksel T5CON
        movlw   01              ; Fosc/4,prescale = 1, 8-bit write
        movwf   T5CON, B
        bsf     TMR5IE
        bcf     TMR5IP
#else
        banksel T5CON
        movlw   01              ; Fosc/4,prescale = 1, 8-bit write
        movwf   T5CON, B
        movlw   01              ; fosc/4
        movwf   T5CLK
        banksel PIE8
        bsf     TMR5IE
        bcf     TMR5IP
#endif
#endif
#if MS_TMR == 6                 // Timer 6 for 1 ms system tick
#ifndef K42
        banksel T6CON
        movlw   7d              ; Prescale = 4, Postscale = 16
        movwf   T6CON, B
        movlw   tmr2ms_val
        movwf   PR6, B
        bsf     TMR6IE
        bcf     TMR6IP
#else
        movlw   01
        movwf   T6CLKCON
        movlw   tmr2ms_val
        movwf   T6PR, A
        movlw   0xaf          ; Prescale = 4, Postscale = 16
        movwf   T6CON
        banksel PIE9
        bsf     TMR6IE
        bcf     TMR6IP
#endif
#endif
        movlw   low(u0+PRAM)        ; UP
        movwf   BANKMASK(upcurr), A
        movlw   high(u0+PRAM)
        movwf   BANKMASK(upcurr)+1, A

        call    LIT
        dw      WARMLIT
        call    LIT
        dw      userarea+PRAM
        call    LIT
        dw      warmlitsize
        call    CMOVE
        call    FRAM
        clrf    INTCON, A
#ifdef K42
        bsf     INTCON, INTCON0_IPEN_POSN, A   ; Enable interrupt priority
#endif
        bsf     GIEL
        bsf     GIEH

        call    LIT
        dw      dp_start
        call    FETCH
        call    TRUE_
        call    EQUAL
        call    ZEROSENSE
        bz      WARM_2
        call    LIT
        dw      20
        call    MS        ; Write to EEPROM fails with a shorter delay, why ?
        call    EMPTY
WARM_2:
        call    DP_TO_RAM

#if FC_TYPE == XONXOFF
        bsf     BANKMASK(FLAGS2), ixoff, A ; Force sending of XON in RX1?
#endif
#ifdef HW_FC_CTS_TRIS
        bcf     HW_FC_CTS_TRIS, HW_FC_CTS_PIN
#endif
#ifdef USB_CDC
        call    USB_ON
#endif
        rcall   RQ
        rcall   VER

        rcall   TURNKEY   ; 0xca
        call    ZEROSENSE
        bz      STARTQ2
        call    XSQUOTE
        db      3
        STRING  ESC
        call    TYPE
        call    LIT
        dw      TURNKEY_DELAY
        call    MS
        call    KEYQ
        call    ZEROSENSE
        bz      STARTQ1
        call    KEY
        call    LIT
        dw      0x1b
        call    XOR ; NOTEQUAL
        call    ZEROSENSE
        bz      STARTQ2
STARTQ1:
        rcall   TURNKEY
#if OPERATOR_UART == 0
#else
        call    DUP
        call    SPACE_
        call    CFATONFA
        call    DOTID
#endif
        call    EXECUTE
STARTQ2:
        goto    ABORT

;*******************************************************
        dw      L_WARM
L_VER:
        NF 3,ver
VER:
        call    XSQUOTE
        db 38,' '                ; 1
        STRING  FlashForth       ; 10
        db      ' ','5',' '      ; 3
        STRING PICTYPE           ; 11
        db      ' '              ; 1
        STRING 16.01.2022        ; 10
        db    0xa,0xd            ; 2
        align 2
        nop
        nop
        goto    TYPE

;***************************************************
; TX1   c --    output character to the TX1 buffer
        dw      L_EXIT
L_TX1_:
        NF 3,tx1
TX1_:
        banksel REG_U1RXIF
        btfsc   U1TXIF
        bra     TX1_SEND
        rcall   PAUSE
        bra     TX1_
TX1_SEND:
        movf    Sminus, W, A
        movf    Sminus, W, A
        banksel U1TXB
        movwf   U1TXB, B
        return

;***************************************************
; RX1    -- c    get character from the serial line
        dw      L_TX1_
L_RX1_:
        NF 3,rx1
RX1_:
        rcall   PAUSE
        rcall   RX1Q
        movf    Sminus, W, A
        iorwf   Sminus, W, A
        bz      RX1_
        lfsr    Tptr, RXbuf
        movf    BANKMASK(RXtail), W, A
        movff_  TWrw, plusS    ;  Take a char from the buffer
        clrf    plusS, A
        incf    BANKMASK(RXtail), F, A
        movlw   RXbufmask
        andwf   BANKMASK(RXtail), F, A
        decf    BANKMASK(RXcnt), F, A
        return
;***************************************************
; RX1?  -- n    return the number of characters in queue
        dw      L_RX1_
L_RX1Q:
        NF 4,rx1?
RX1Q:
#ifndef K42
        btfsc   OERR
        bcf     CREN ; Restart RX on case of RX overrun
        bsf     CREN
#else
        banksel U1ERRIR
        btfsc   U1RXFOIF
        bcf     U1RXFOIF   ; Restart RX on case of RX overrun
        bsf     U1RXFOIF
#endif
        movf    BANKMASK(RXhead), W, A
        subwf   BANKMASK(RXtail), W, A
        movwf   plusS, A
        bnz     RX1Q2
#if FC_TYPE == XONXOFF
        btfss   BANKMASK(FLAGS2), fFC, A
        rcall   XXON
#endif
#if FC_TYPE == CTS
        bcf     HW_FC_CTS_PORT, HW_FC_CTS_PIN
#endif
        clrf    BANKMASK(RXcnt), A
RX1Q2:
        clrf    plusS, A
        return
#if WANT_UART2 == ENABLE
;***************************************************
; TX2   c --    output character to the TX1 buffer
        dw      L_RX1Q
L_TX2_:
        NF 3,tx2
TX2_:
        banksel REG_U2RXIF
        btfsc   U2TXIF
        bra     TX2_SEND
        rcall   PAUSE
        bra     TX2_
TX2_SEND:
        movf    Sminus, W, A
        movf    Sminus, W, A
        banksel U2TXB
        movwf   U2TXB, B
        return
;***************************************************
; RX2    -- c    get character from the serial line
        dw      L_TX2_
L_RX2_:
        NF 3,rx2
RX2_:
        rcall   PAUSE
        rcall   RX2Q
        movf    Sminus, W, A
        iorwf   Sminus, W, A
        bz      RX2_
        lfsr    Tptr, RX2buf
        movf    BANKMASK(RX2tail), W, A
        movff_  TWrw, plusS    ;  Take a char from the buffer
        clrf    plusS, A
        incf    BANKMASK(RX2tail), F, A
        movlw   RX2bufmask
        andwf   BANKMASK(RX2tail), F, A
        decf    BANKMASK(RX2cnt), F, A
        return
;***************************************************
; RX2?  -- n    return the number of characters in queue
        dw      L_RX2_
L_RX2Q:
        NF 4,rx2?
RX2Q:
#ifndef K42
        banksel RCSTA2
        btfsc   RCSTA2, OERR2, B
        bcf     RCSTA2, CREN2, B ; Restart RX on case of RX overrun
        bsf     RCSTA2, CREN2, B
#else
        banksel U2ERRIR
        btfsc   U2RXFOIF
        bcf     U2RXFOIF    ; Restart RX on case of RX overrun
        bsf     U2RXFOIF
#endif

        movf    BANKMASK(RX2head), W, A
        subwf   BANKMASK(RX2tail), W, A
        movwf   plusS, A
        bnz     RX2Q2
#if FC_TYPE == XONXOFF
        btfss   BANKMASK(FLAGS2), fFC, A
        rcall   XXON
#endif
#if FC_TYPE == CTS
        bcf     HW_FC_CTS_PORT, HW_FC_CTS_PIN, A
#endif
        clrf    BANKMASK(RX2cnt), A
RX2Q2:
        clrf    plusS, A
        return
#endif

#if WANT_UART3 == ENABLE
;***************************************************
; TX3   c --    output character to the TX1 buffer
#if WANT_UART2 == ENABLE
        dw      L_RX2Q
#else
        dw      L_RX1Q
#endif
L_TX3_:
        NF 3,tx3
TX3_:
        banksel REG_U3RXIF
        btfsc   U3TXIF
        bra     TX3_SEND
        rcall   PAUSE
        bra     TX3_
TX3_SEND:
        movf    Sminus, W, A
        movf    Sminus, W, A
        banksel U3TXB
        movwf   U3TXB, B
        return
;***************************************************
; RX3    -- c    get character from the serial line
        dw      L_TX3_
L_RX3_:
        NF 3,rx3
RX3_:
        rcall   PAUSE
        rcall   RX3Q
        movf    Sminus, W, A
        iorwf   Sminus, W, A
        bz      RX3_
        lfsr    Tptr, RX3buf
        movf    BANKMASK(RX3tail), W, A
        movff_  TWrw, plusS    ;  Take a char from the buffer
        clrf    plusS, A
        incf    BANKMASK(RX3tail), F, A
        movlw   RX3bufmask
        andwf   BANKMASK(RX3tail), F, A
        decf    BANKMASK(RX3cnt), F, A
        return
;***************************************************
; RX3?  -- n    return the number of characters in queue
        dw      L_RX3_
L_RX3Q:
        NF 4,rx3?
RX3Q:
        banksel U3ERRIR
        btfsc   U3RXFOIF
        bcf     U3RXFOIF    ; Restart RX on case of RX overrun
        bsf     U3RXFOIF

        movf    BANKMASK(RX3head), W, A
        subwf   BANKMASK(RX3tail), W, A
        movwf   plusS, A
        bnz     RX3Q2
#if FC_TYPE == XONXOFF
        btfss   BANKMASK(FLAGS2), fFC, A
        rcall   XXON
#endif
#if FC_TYPE == CTS
        bcf     HW_FC_CTS_PORT, HW_FC_CTS_PIN, A
#endif
        clrf    BANKMASK(RX3cnt), A
RX3Q2:
        clrf    plusS, A
        return
#endif

; idle
#if WANT_UART3 == ENABLE
        dw      L_RX3Q
#else
#if WANT_UART2 == ENABLE
        dw      L_RX2Q
#else
        dw      L_RX1Q
#endif
#endif
#if IDLE_MODE == ENABLE
L_IDLE:
        NF 4,idle
IDLE:
        bsf    BANKMASK(fstatus), 7, A
        return

; busy
        dw      L_IDLE
L_BUSY:
        NF 4,busy
BUSY:
        clrf    BANKMASK(fstatus), A
        return

; load -- n
        dw      L_BUSY
#if CPU_LOAD == ENABLE
L_LOAD:
        NF 4,load
LOAD:
        movf    BANKMASK(load_res), W, A
        movwf   plusS, A
        movf    BANKMASK(load_res)+1, W, A
        movwf   plusS, A
        movf    BANKMASK(load_res)+2, W, A
        movwf   plusS, A
        clrf    plusS, A
        call    LIT
        dw      CPU_LOAD_VAL
        call    UMSLASHMOD
        goto    NIP

; a, ( -- 0 ) Force Access bank
        dw      L_LOAD
#endif
#endif
L_A_:
        NF2     2,a
        db      ','
        align   2
A_:
        goto    FALSE_

; w, ( -- 0 ) Destination W register
        dw      L_A_
L_W_:
        NF2     2,w
        db      ','
        align   2
W_:
FALSE__:
        goto    FALSE_

; movf, ( f d a -- )
        dw      L_W_
L_MOVF_:
        NF2     5,movf
        db      ','
        align   2
        rcall   AS3_DOES
        dw      0x0050

; movwf, ( f a -- )
        dw      L_MOVF_
L_MOVWF_:
        NF2     5,movwf
        db      ','
        align   2
        rcall   AS2_DOES
        dw      0x006e

; movlw, ( k -- )
        dw      L_MOVWF_
L_MOVLW_:
        NF2     6,movlw
        db      ','
        align   2
        rcall   AS1_DOES
        dw      0x0e00

; movlb, ( k -- )
        dw      L_MOVLW_
L_MOVLB_:
        NF2     6,movlb
        db      ','
        align   2
        rcall   AS1_DOES
        dw      0x0100

; andlw, ( k -- )
        dw      L_MOVLB_
L_ANDLW_:
        NF2     6,andlw
        db      ','
        align   2
        rcall   AS1_DOES
        dw      0x0b00

; [i   --    Save registers for the Forth interrupt context
;;;     16 instruction cycles
        dw      L_ANDLW_
L_LI:
        NF INLINE|COMPILE|2,[i
#ifndef K42
        movf    PRODL, W, A
        movwf   BANKMASK(ihprodl), A
        movf    PRODH, W, A
        movwf   BANKMASK(ihprodh), A
        movf    Sp, W, A
        movwf   BANKMASK(ihsp), A
        movf    Sbank, W, A
        movwf   BANKMASK(ihsbank), A
        movf    Tp, W, A
        movwf   BANKMASK(ihtp), A
        movf    Tbank, W, A
        movwf   BANKMASK(ihtbank), A
 #endif
        movf    TBLPTRL, W, A
        movwf   BANKMASK(ihtblptrl), A
        movf    TBLPTRH, W, A
        movwf   BANKMASK(ihtblptrh), A
        movf    TABLAT, W, A
        movwf   BANKMASK(ihtablat), A
        lfsr    Sptr, irq_s0 - 1
        return

; i]   --    Restore registers for the Forth interrupt context
;;;     14 instruction cycles
        dw      L_LI
L_IR:
        NF INLINE|COMPILE|2,i]
#ifndef K42
        movf    BANKMASK(ihtbank), W, A
        movwf   Tbank, A
        movf    BANKMASK(ihtp), W, A
        movwf   Tp, A
        movf    BANKMASK(ihsbank), W, A
        movwf   Sbank, A
        movf    BANKMASK(ihsp), W, A
        movwf   Sp, A
        movf    BANKMASK(ihprodl), W, A
        movwf   PRODL, A
        movf    BANKMASK(ihprodh), W, A
        movwf   PRODH, A
#endif
        movf    BANKMASK(ihtablat), W, A
        movwf   TABLAT, A
        movf    BANKMASK(ihtblptrl), W, A
        movwf   TBLPTRL, A
        movf    BANKMASK(ihtblptrh), W, A
        movwf   TBLPTRH, A
        return

;*******************************************************
;;; Multiplication routine from the PIC datasheet adapted to FORTH.
;;; Uses the registers:
;;; TBLPTRH TBLPTRL PCLATH TABLAT TOSL TOSH PRODL PRODH
;;; 42 clock cycles
umstar0:
        rcall   TOTBLP
        movf    Sminus, W, A
        movwf   PCLATH, A
        movf    Sminus, W, A
        movwf   TABLAT, A
        movf    TBLPTRL, W, A

        push
        mulwf   TABLAT, A       ; ARG1L * ARG2L ->  PRODH:PRODL
        movf    PRODL, W, A
        movwf   plusS, A
        movf    PRODH, W, A
        movwf   plusS, A

        movf    TBLPTRH, W, A
        mulwf   PCLATH, A       ; ARG1H * ARG2H -> PRODH:PRODL
        movf    PRODL, W, A
        movwf   TOSL, A
        movf    PRODH, W, A
        movwf   TOSH, A

        movf    TBLPTRL, W, A
        mulwf   PCLATH, A       ; ARG1L * ARG2H -> PRODH:PRODL
        movf    PRODL, W, A
        addwf   Srw, F, A
        movf    PRODH, W, A
        addwfc  TOSL, F, A
        clrf    WREG, A
        addwfc  TOSH, F, A

        movf    TBLPTRH, W, A
        mulwf   TABLAT, A       ; ARG1H * ARG2L -> PRODH:PRODL
        movf    PRODL, W, A     ;
        addwf   Srw, F, A       ; Add cross
        movf    PRODH, W, A     ; products
        addwfc  TOSL, F, A
        clrf    WREG, A
        addwfc  TOSH, F, A

        movf    TOSL, W, A
        movwf   plusS, A
        movf    TOSH, W, A
        movwf   plusS, A
        decf    STKPTR, F, A
        return

;***********************************************************
;;; 320 - 384 cycles for 16 or 32 bit division
;;; TBLPTRH TBLPTRL PCLATH TABLAT Tp Tbank PRODL PRODH

#define DIVIDEND_0      PRODL
#define DIVIDEND_1      PRODH
#define DIVIDEND_2      TABLAT
#define DIVIDEND_3      PCLATH
#define DIVISOR_0       TBLPTRL
#define DIVISOR_1       TBLPTRH
#define DCNT            Tbank

umslashmod0:
        rcall   TOTBLP     ; DIVISOR_1, DIVISOR_0
        tstfsz  DIVISOR_0, A
        bra     umslashmod3
        tstfsz  DIVISOR_1, A
        bra     umslashmod3
        bsf     BANKMASK(fstatus), 0, A  ; Signal divide by zero error
        bra     WARM
umslashmod3:
        movf    Sminus, W, A
        movwf   DIVIDEND_3, A
        movf    Sminus, W, A
        movwf   DIVIDEND_2, A
        movf    Sminus, W, A
        movwf   DIVIDEND_1, A
        movf    Sminus, W, A
        movwf   DIVIDEND_0, A
        movlw   16
        movwf   DCNT, A             ; count to 16; 19
UMSLASHMOD1:
        clrf    Tp, A
        bcf     CARRY
        rlcf    DIVIDEND_0, F, A
        rlcf    DIVIDEND_1, F, A
        rlcf    DIVIDEND_2, F, A
        rlcf    DIVIDEND_3, F, A
        rlcf    Tp, F, A

        movf    DIVISOR_0, W, A
        subwf   DIVIDEND_2, W , A
        movf    DIVISOR_1, W, A
        subwfb  DIVIDEND_3, W, A
        movlw   0
        subwfb  Tp, W, A
        bnc     UMSLASHMOD2

        movf    DIVISOR_0, W, A
        subwf   DIVIDEND_2, F, A
        movf    DIVISOR_1, W, A
        subwfb  DIVIDEND_3, F, A
        bsf     DIVIDEND_0, 0, A
UMSLASHMOD2:
        decfsz  DCNT, F, A
        bra     UMSLASHMOD1         ; 16*(18-22) = ~320
        movf    DIVIDEND_2, W, A
        movwf   plusS, A            ; remainder
        movf    DIVIDEND_3, W, A
        movwf   plusS, A
        movf    DIVIDEND_0, W, A
        movwf   plusS , A           ; quotient
        movf    DIVIDEND_1, W, A
        movwf   plusS, A
        return                  ; 11 cycles
; *******************************************************************
;if (ibaselo != (iaddrlo&0xc0))&& (ibasehi != iaddrhi)
;   if (idirty)
;       writebuffer_to_imem
;   endif
;   fillbuffer_from_imem
;   ibaselo = iaddrlo&0xc0
;   ibasehi = iaddrhi
;endif
iupdatebuf:
#if XSTORE == ENABLE
        movf    BANKMASK(iaddr_up), W, A
        cpfseq  BANKMASK(ibase_up), A
        bra     iupdatebuf0
#endif
        movf    BANKMASK(iaddr_hi), W, A
        cpfseq  BANKMASK(ibase_hi), A
        bra     iupdatebuf0
        movlw   flash_pointer_mask
        andwf   BANKMASK(iaddr_lo), W, A
        cpfseq  BANKMASK(ibase_lo), A
        bra     iupdatebuf0
        return

iupdatebuf0:
        rcall   IFLUSH

        movlw   flash_pointer_mask
        andwf   BANKMASK(iaddr_lo), W, A
        movwf   BANKMASK(ibase_lo), A
        movf    BANKMASK(iaddr_hi), W, A
        movwf   BANKMASK(ibase_hi), A
#if XSTORE == ENABLE
        movf    BANKMASK(iaddr_up), W, A
        movwf   BANKMASK(ibase_up), A
#endif
fill_buffer_from_imem:
#ifndef Qxx
        movlw   flash_block_size ; d'64' or d'128'
        movwf   PCLATH, A
        rcall   init_ptrs        ; Init TBLPTR and ram pointer
fill_buffer_from_imem_1:
        tblrd*+
        movf    TABLAT, W, A
        movwf   Tplus, A
        decfsz  PCLATH, F, A
        bra     fill_buffer_from_imem_1
#if XSTORE == ENABLE
        clrf    TBLPTRU, A
#endif
#else   // Qxx
        rcall   init_ptrs
        movlw   2
        movwf   NVMCON1, B
        bsf     NVMGO
        btfsc   NVMGO
        bra     $-2
#endif
        return
;***********************************************************
write_buffer_to_imem:
;; Loop here until there are no more characters has been received for a while
;; from the operator UART.
;; The assumption is that the serial line is silent then.
#ifdef OPERATOR_RX_IS_UART
#if FC_TYPE == XONXOFF
        btfss   BANKMASK(FLAGS2), fFC, A
        rcall   XXXOFF
#endif
#if FC_TYPE == CTS
        btfss   BANKMASK(FLAGS2), fFC
        bsf     HW_FC_CTS_PORT, HW_FC_CTS_PIN, A
#endif

wbtil:
        bcf     BANKMASK(FLAGS1), istream, A
        rcall   LIT
        dw      (19200/baud)+2
        call    MS
        btfsc   BANKMASK(FLAGS1), istream, A
        bra     wbtil
#endif
        bcf     GIE             ; Disable Interrupts

#ifdef p18fxx2xx8_fix_1
        movff   PIE1, SPIE1
        movff   PIE2, SPIE2
        movff   INTCON, SINTCON ; TMR0IF, INT0IF, RBIF
        clrf    INTCON          ; may be lost
        clrf    PIE1
        clrf    PIE2
#endif
#ifndef Qxx
        rcall   init_ptrs             ; Init TBLPTR, ram pointer, banksel
                                      ; Erase the flash block
        bsf     BANKMASK(EECON1), 7, B    ; EEPGD
        bcf     BANKMASK(EECON1), 6, B    ; CFGS
        bsf     BANKMASK(EECON1), 2, B    ; WREN
        bsf     BANKMASK(EECON1), 4, B    ; FREE
        rcall   magic
        bcf     BANKMASK(EECON1), 4, B    ; FREE

        TBLRD*-
        movlw   flash_write_outer_loop
        movwf   PRODH, A
write_buffer_to_imem_1:
        movlw   flash_write_inner_loop
        movwf   PRODL, A
write_buffer_to_imem_2:
        movf    Tplus, W, A
        movwf   TABLAT, A
        tblwt+*
        decfsz  PRODL, F, A
        bra     write_buffer_to_imem_2
        rcall   magic
        decfsz  PRODH, F, A
        bra     write_buffer_to_imem_1
        bcf     BANKMASK(EECON1), 2, B    ; WREN
#else
        ; Erase the flash block
        rcall   init_ptrs
        movlw   6
        movwf   NVMCON1, B
        rcall   magic
        ; Write the buffer to flash
        movlw   5
        movwf   NVMCON1, B
        rcall   magic
#endif

#ifdef p18fxx2xx8_fix_1
        movff   SPIE2, PIE2
        movff   SPIE1, PIE1
        movff   SINTCON, INTCON
#endif
        bsf     GIE

verify_imem:
#ifndef Qxx
        movlw   flash_block_size     ; d'64' or d'128'
        movwf   PCLATH, A
#else
        clrf    PCLATH, A
#endif
        rcall   init_ptrs
verify_imem_1:
        tblrd*+
        movf    TABLAT, W, A
        cpfseq  Tplus, A
        reset
        decfsz  PCLATH, F, A
        bra     verify_imem_1

        bcf     BANKMASK(FLAGS1), idirty, A ; Mark flash buffer clean
        setf    BANKMASK(ibase_hi), A       ; Mark flash buffer empty
#if XSTORE == ENABLE
        setf    BANKMASK(ibase_up), A       ; Mark flash buffer empty
        clrf    TBLPTRU
#endif
        return

init_ptrs:
        banksel EECON1
        lfsr    Tptr, flash_buf
        movf    BANKMASK(ibase_lo), W, A
        movwf   TBLPTRL, A
        movf    BANKMASK(ibase_hi), W, A
        movwf   TBLPTRH, A
#if XSTORE == ENABLE
        movf    BANKMASK(ibase_up), W, A
        movwf   TBLPTRU, A
#endif
#ifdef Qxx
        movf    BANKMASK(ibase_lo), W, A
        movwf   NVMADRL, B
        movf    BANKMASK(ibase_hi), W, A
        movwf   NVMADRH, B
#if XSTORE == ENABLE
        movf    BANKMASK(ibase_up), W, A
        movwf   NVMADRU, B
#endif
#endif
        return

magic:
        movlw   0x55
        movwf   EECON2, B
        movlw   0xaa
        movwf   EECON2, B
#ifndef Qxx
        bsf     EECON1, 1, B    ; WR
#else
        bsf     NVMGO
        btfsc   NVMGO
        bra     $-2
#endif
        return

;***************************************************
asmemit:
#if 0
        banksel REG_U1RXIF
        btfss   U1TXIF
        bra     asmemit
        banksel U1TXB
        movwf   U1TXB, B
        return
#endif
#if FC_TYPE == XONXOFF
#if OPERATOR_UART == 1
        banksel REG_U1RXIF
        btfss   U1TXIF
        bra     asmemit
        banksel U1TXB
        movwf   U1TXB, B
#endif
#if WANT_UART2 == ENABLE
#if OPERATOR_UART == 2
        banksel REG_U2RXIF
        btfss   U2TXIF
        bra     asmemit
        banksel U2TXB
        movwf   U2TXB, B
#endif
#endif
#if WANT_UART3 == ENABLE
#if OPERATOR_UART == 3
        banksel REG_U3RXIF
        btfss   U3TXIF
        bra     asmemit
        banksel U3TXB
        movwf   U3TXB, B
#endif
#endif
#endif
        return
;***************************************************
; N=    c-addr nfa -- flag
; N= is only used for finding dictionary entries
        dw      L_VER
L_NEQUAL:
        NF 2,n=
NEQUAL:
        movff_  Sminus, TBLPTRH
        movff_  Sminus, TBLPTRL       ; nfa in flash
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A               ; String in ram
        tblrd*+
        movf    TABLAT, W, A
        andlw   NFAmask             ; MASK NFA, IMMED, INLINE, COMPILE BITS
        movwf   PCLATH, A           ; Length
        xorwf   Tplus, W, A
        bnz     NEQUAL2             ; NO MATCH
NEQUAL0:
        tblrd*+
        movf    TABLAT, W, A
        xorwf   Tplus, W, A
        bnz     NEQUAL2
        decfsz  PCLATH, F, A
        bra     NEQUAL0
NEQUAL2:
        movwf   plusS, A
        clrf   plusS, A
        return

; SKIP   c-addr u c -- c-addr' u'
;                          skip matching chars
; u (count) must be smaller than 256
        dw      L_NEQUAL
L_SKIP:
        NF 4,skip
SKIP:
        rcall   TOTBLP          ; c character
        movf    Sminus, W, A    ; skip count_hi
        movf    Sminus, W, A
        movwf   PCLATH, A       ; count_lo
        bz      SKIP5           ; zero flag comes from the previous movf
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A           ; c-addr
SKIP0:
        movlw   0x9             ; SKIP TAB
        subwf   Trw, W, A
        bz      SKIP1

        movf    Trw, W, A
        subwf   TBLPTRL, W, A
        bnz     SKIP4
SKIP1:                          ; check next character
        movf    Tplus, W, A

        decfsz  PCLATH, F, A
        bra     SKIP0
        swapf   Tminus, W, A
SKIP4:
                                ; found start of word
                                ; restore the stack
        movf    Tp, W, A
        movwf   plusS, A
        movf    Tbank, W, A
        iorlw   high(PRAM)
        movwf   plusS, A
SKIP5:
        movf    PCLATH, W, A
        movwf   plusS, A
        clrf    plusS, A
        return

; SCAN   c-addr u c -- c-addr' u'
;                          find matching chars
; u(count) must be smaller than 256
; TAB will always give a match. This works OK
; with tabbed source files that have space as a delimiter.
; When using scan with other delimiters there may be
; problems, because TAB will always terminate the scan.

        dw      L_SKIP
L_SCAN:
        NF 4,scan
SCAN:
        rcall   TOTBLP              ; c character
        movf    Sminus, W, A        ; count_hi
        movf    Sminus, W , A       ; count_lo
        movwf   PCLATH, A
        bz      SCAN4
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A               ; c-addr
SCAN0:
        movf    Trw, W, A
        subwf   TBLPTRL, W, A
        bz      SCAN3               ; Found a match
SCAN1:
        movf    Trw, W, A
        sublw   9                   ; Check for TAB
        bz      SCAN3               ; TAB is handled as the delimiter c .
SCAN2:
        movf    Tplus, W, A         ; check next character
        decfsz  PCLATH, F, A
        bra     SCAN0
SCAN3:                              ; found start of word
                                    ; restore the stack
        movf    Tp, W, A
        movwf   plusS, A
        movf    Tbank, W, A
        iorlw   high(PRAM)
        movwf   plusS, A
SCAN4:
        movf    PCLATH, W, A
        movwf   plusS, A
        clrf    plusS, A
        return

; ei  ( -- )    Enable interrupts
        dw      L_SCAN
L_EI:
        NF INLINE|2,ei
        bsf     GIE
        return

; di  ( -- )    Disable interrupts
        dw      L_EI
L_DI:
        NF INLINE|2,di
        bcf     GIE
        return

; ;i  ( -- )    End definition of user interrupt routine
        dw      L_DI
L_IRQ_SEMI:
        db      NFA|IMMED|2,';','i'
        align   2
IRQ_SEMI:
        rcall   LIT
        dw      0x11      ; retfie 1
        rcall   ICOMMA
        goto    LEFTBRACKET

; INT!   addr vector --     Store the interrupt vector
        dw      L_IRQ_SEMI
L_INT:
        NF2     4,int
        db      '!'
        align   2
INT:
        call    DROP
        movf    Sminus, W, A
        movff_	Sminus, irq_v
        movwf   BANKMASK(irq_v)+1, A
        return

; LITERAL  x --           compile literal x as native code
        dw      L_INT
L_LITERAL:
        NF IMMED|7,literal
LITERAL:
        movf    Sminus, W, A
        movff_   Srw, Tp
        movwf   Srw, A
        movf    Tp, W, A
        movwf   plusS, A
        rcall   LITER0
LITER0:
        movf    Srw, W, A
        bz      LITER2
        movlw   0xff
        cpfslt  Srw, A
        bra     LITER3
LITER1:
        movlw   0x0e              ; movlw literal
        movwf   plusS, A
        rcall   ICOMMA
        rcall   LIT
        dw      0x6eec            ; movwf plusS
        rcall   ICOMMA
        bra     LITER5
LITER2:
        rcall   LIT
        dw      0x6aec            ; clrf plusS
        bra     LITER4
LITER3:
        rcall   LIT
        dw      0x68ec            ; setf plusS
LITER4:
        rcall   ICOMMA
        movf    Sminus, W, A
LITER5:
        return

;**************************************************
;   INSTRUCTION MEMORY INSTRUCTIONS
FETCHLIT:
        movwf   TBLPTRH, A
pfetch0:
        tblrd*+
        movf    TABLAT, W, A
        movwf   plusS, A
        tblrd*+
        movf    TABLAT, W, A
        movwf   plusS, A
        return
pcfetch0:
        tblrd*+
        movf    TABLAT, W, A
        movwf   plusS, A
        clrf    plusS, A
        return

ISTORE_SETUP:
        rcall   LOCKED
; check that writes are not to the kernel code
ISTORECHK:
#if XSTORE == ENABLE
        tstfsz  BANKMASK(iaddr_up), A
        bra     ISTOREOK
#else
        movlw   HIGH FLASH_HI+1
        cpfslt  Srw, A
        bra     ISTORERR
#endif
        movlw   HIGH((dpcode+0x100)&0xff00)
        cpfslt  Srw, A
        bra     ISTOREOK
        bra     ISTORERR
ISTOREOK:
;check if program memory row is already in buffer
        movf    Sminus, W, A
        movwf   BANKMASK(iaddr_hi), A
        movf    Sminus, W, A
        movwf   BANKMASK(iaddr_lo), A
        rcall   iupdatebuf
; set write address
        movf    BANKMASK(iaddr_lo), W, A
#ifndef Qxx
        andlw   flash_block_mask
#endif
        lfsr    Tptr, flash_buf
        addwf   Tp, F, A
        return

; I!       x a-addr --    store cell in Code mem
ISTORE:
        rcall   ISTORE_SETUP
        movf    Tplus, W, A
        movf    Sminus, W, A
        movwf   Tminus, A
        bra     ICSTORE1

; IC!       x addr --    store byte in Code mem
ICSTORE:
        rcall   ISTORE_SETUP
        swapf   Sminus, W, A
ICSTORE1:
        movf    Sminus, W, A
        movwf   Trw, A
;mark_buffer_dirty
        bsf     BANKMASK(FLAGS1), idirty, A
        return

; >TBLP  x --
        dw      L_LITERAL
L_TOTBLP:
        NF 5,>tblp
TOTBLP:
        movf    Sminus, W, A
        movwf   TBLPTRH, A
        movff_  Sminus, TBLPTRL
        return
; I@       a-addr -- x  fetch cell from Code mem
; 25 cycles when fetching from buffer
; 18-22 cycles when pfetching directly from flash
IFETCH:
        rcall   TOTBLP
        cpfseq  BANKMASK(ibase_hi), A
        bra     pfetch0
#ifndef Qxx
        movlw   flash_pointer_mask
        andwf   TBLPTRL, W, A
        cpfseq  BANKMASK(ibase_lo), A
        bra     pfetch0
#endif
;read_cell_from_buffer
        movf    TBLPTRL, W, A
#ifndef Qxx
        andlw   flash_block_mask
#endif
        lfsr    Tptr, flash_buf
        addwf   Tp, F, A
        goto    FETCH2

;  IC@      addr -- x  fetch char from Code mem
ICFETCH:
        rcall   TOTBLP
ICFETCH1:
        movf    TBLPTRH, W, A
        cpfseq  BANKMASK(ibase_hi), A
        bra     pcfetch0
#ifndef Qxx
        movlw   flash_pointer_mask
        andwf   TBLPTRL, W, A
        cpfseq  BANKMASK(ibase_lo), A
        bra     pcfetch0
#endif
;read_byte_from_buffer
        movf    TBLPTRL, W, A
#ifndef Qxx
        andlw   flash_block_mask
#endif
        lfsr    Tptr, flash_buf
        addwf   Tp, F, A
        goto    CFETCH2


; E!      x a-addr --    store cell in data EEPROM
ESTORE:
        rcall   LOCKED
        movf    Sminus, W, A
        banksel EEADR
#ifdef EEADRH
#ifdef Qxx
        andlw   EEPROM_MASKH
#endif
        movwf   EEADRH, B
#endif
        movf    Sminus, W, A
        movwf   EEADR, B
        incf    EEADR, F, B
        movf    Sminus, W, A
        movwf   EEDATA, B
        rcall   ECSTORE1
        decf    EEADR, F, B
        movf    Sminus, W, A
        movwf   EEDATA, B
        bra     ECSTORE1

; EC!       c addr --    store char in data EEPROM
ECSTORE:
        rcall   LOCKED
        movf    Sminus, W, A
        banksel EEADR
#ifdef EEADRH
#ifdef Qxx
        andlw   EEPROM_MASKH
#endif
        movwf   EEADRH, B
#endif
        movf    Sminus, W, A
        movwf   EEADR, B
        movf    Sminus, W, A
        movf    Sminus, W, A
        movwf   EEDATA, B
ECSTORE1:
#ifndef Qxx
        bcf     EECON1, 7, B        ; EEPGD
        bcf     EECON1, 6, B        ; EEFS
        bsf     EECON1, 2, B        ; WREN
#else
        movlw   EEPROM_ADDRU
        movwf   NVMADRU, B
        movlw   3
        movwf   NVMCON1, B
#endif
        bcf     GIE
        call    magic
        bsf     GIE
ECSTORE2:
#ifndef Qxx
        btfsc   EECON1, 1, B        ; WR
        bra     ECSTORE2
        bcf     EECON1, 2, B        ; WREN
#else
        clrf    NVMCON1, B
        clrf    NVMADRU, B
#endif
        return


; E@       a-addr -- x  fetch cell from data EEPROM
EFETCH:
        movf    Sminus, W, A
        banksel EEADR
#ifdef EEADRH
#ifdef Qxx
        andlw   EEPROM_MASKH
#endif
        movwf   EEADRH, B
#endif
        movf    Sminus, W, A
        movwf   EEADR, B
        rcall   asmecfetch
        incf    EEADR, F, B
        rcall   asmecfetch
        return

; EC@      addr -- c  fetch char from data EEPROM
ECFETCH:
        movf    Sminus, W, A
        banksel EEADR
#ifdef EEADRH
#ifdef Qxx
        andlw   EEPROM_MASKH
#endif
        movwf   EEADRH, B
#endif
        movf    Sminus, W, A
        movwf   EEADR, B
        rcall   asmecfetch
        clrf    plusS, A
        return
asmecfetch:
#ifndef Qxx
#ifdef p18fxx2xx8_fix_1
        bcf     GIE                 ; 18f252 ERRATA
#endif
        bcf     EECON1, 7, B        ; EEPGD
        bcf     EECON1, 6, B        ; CFGS
        bsf     EECON1, 0, B        ; RD
        movf    EEDATA, W, B
        movwf   plusS, A
#ifdef p18fxx2xx8_fix_1
        bsf     GIE                 ; 18f252 ERRATA
#endif
#else
        movlw   EEPROM_ADDRU
        movwf   NVMADRU, B
        clrf    NVMCON1, B
        bsf     NVMGO
        btfsc   NVMGO
        bra     $-2
        movf    EEDATA, W, B
        movwf   plusS, A
        clrf    NVMADRU, B
#endif
        return

;;; Disable writes to flash and eeprom
        dw      L_TOTBLP
L_FLOCK:
        NF 3,fl-
        bsf     BANKMASK(FLAGS1), fLOCK, A
        return

;;; Enable writes to flash and eeprom
        dw      L_FLOCK
L_FUNLOCK:
        NF 3,fl+
        bcf     BANKMASK(FLAGS1), fLOCK, A
        return

;;; Enable flow control
        dw      L_FUNLOCK
L_FCON:
        NF 3,u1+
        bcf     BANKMASK(FLAGS2), fFC, A
        return

;;; Disable flow control
        dw      L_FCON
L_FCOFF:
        NF 3,u1-
        bsf     BANKMASK(FLAGS2), fFC, A
        return

;;; Clear watchdog timer
        dw      L_FCOFF
L_CWD:
        NF INLINE|3,cwd
        clrwdt
        return

; VALUE
        dw      L_CWD
L_VALUE:
        NF 5,value
VALUE:
        call    CREATE
        rcall   COMMA
        call    XDOES
VALUE_DOES:
        rcall   DODOES
        goto    FETCH

; DEFER
        dw      L_VALUE
L_DEFER:
        NF 5,defer
;DEFER:
        call    CREATE
        rcall   LIT
        dw      ABORT
        rcall   COMMA
        call    XDOES
DEFER_DOES:
        rcall   DODOES
        goto    FEXECUTE

; TO
        dw      L_DEFER
L_TO:
        NF IMMED|2,to
TO_:
        goto    IS

; IS
        dw      L_TO
L_IS:
        NF IMMED|2,is
IS:
        call    TICK
        call    TOBODY
        movf    BANKMASK(state), W, A
        bz      IS1
        rcall   LITERAL
        rcall   DOCOMMAXT
        dw      STORE
        bra     IS2
IS1:
        rcall   STORE
IS2:
        return

        dw      L_IS
L_TURNKEY:
        NF 7,turnkey
TURNKEY:
        call    VALUE_DOES      ; Must be call for IS to work.
        dw      dpSTART+PRAM

#ifdef USB_CDC
;***************************************************
; TX0  c --    output character to the USB serial emulation
; Buffer Descriptor Status Register bits
#define UOWN      7
#define DTS       6
        dw      L_PAUSE
L_TX0:
        NF 3,txu
TX0:
        rcall   PAUSE
        banksel ep2istat
        btfss   BANKMASK(usb_device_state), 3, B
        bra     TX0_DROP
        btfsc   BANKMASK(ep2istat), UOWN, B
        bra     TX0
        movf    BANKMASK(ms_count), W, A
        addlw   2+TICK_TIME
        movwf   ep2itmo, A
        lfsr    Tptr, cdc_data_tx
        movf    Sminus, W, A
        movf    ep2icount, W, A
        movff_  Sminus, TWrw
        addlw   1
        movwf   ep2icount, A
        sublw   6                       ; l - w
        bnn     TX0_END
TX0_SEND:
        movf    ep2icount, W, A
TX0_SEND2:
        movwf   BANKMASK(ep2icnt), B
        movlw   0x48
        btfsc   BANKMASK(ep2istat), DTS, B
        movlw   0x8
        movwf   BANKMASK(ep2istat), B
        bsf     BANKMASK(ep2istat), UOWN, B
        clrf    ep2icount, A
        bra     TX0_END
TX0_DROP:
        call    DROP
TX0_END:
        return
;***************************************************
; KEY   -- c    get character from the USB line
        dw      L_TX0
L_RX0:
        NF 3,rxu
RX0:
        call    PAUSE
        banksel ep2ostat
        btfsc   BANKMASK(usb_device_state), 3, B
        btfsc   BANKMASK(ep2ostat), UOWN, B
        bra     RX0
        lfsr    Tptr, cdc_data_rx
        movf    ep2optr, W, A
        movff   TWrw, plusS
        incf    ep2optr, F, A
        decf    BANKMASK(ep2ocnt), F, B
        bnz     RX0_END
        movlw   8
        movwf   BANKMASK(ep2ocnt), B
        movlw   0x48
        btfsc   BANKMASK(ep2ostat), DTS, B
        movlw   8
        movwf   BANKMASK(ep2ostat), B
        bsf     BANKMASK(ep2ostat), UOWN, B
        clrf    ep2optr, A
RX0_END:
        clrf    plusS, A
        return
;***************************************************
; KEY?  -- f    return true if a char is waiting
        dw      L_RX0
L_RX0Q:
        NF 4,rxu?
RX0Q:
        banksel ep2ostat
        btfsc   BANKMASK(usb_device_state), 3, B
        btfsc   BANKMASK(ep2ostat), 7, B
RX0Q1:
        goto    FALSE_
        goto    TRUE_
#endif
; ***************************************************
#if FC_TYPE == XONXOFF
XXOFF:
        btfsc   BANKMASK(FLAGS2), ixoff, A
        return
XXXOFF:
        bsf     BANKMASK(FLAGS2), ixoff, A
        movlw   XOFF_
        bra     asmemit
XXON:
        btfss   BANKMASK(FLAGS2), ixoff, A
        return
XXXON:
        bcf     BANKMASK(FLAGS2), ixoff, A
        movlw   XON_
        bra     asmemit
#endif
;****************************************************
;if (idirty)
;   writebuffer_to_imem
;endif
#ifdef USB_CDC
        dw      L_RX0Q
#else
        dw      L_PAUSE
#endif
L_IFLUSH:
        NF 6,iflush
IFLUSH:
        btfsc   BANKMASK(FLAGS1), idirty, A
        bra     write_buffer_to_imem
        return

; *********************************************
; Bit masking 8 bits, only for ram addresses !
; : mset ( mask addr -- )
;   dup >r c@ swap or r> c!
; ;
        dw      L_IFLUSH
L_MSET:
        NF 4,mset
MSET:
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A
        movf    Sminus, W, A
        movf    Sminus, W, A
        iorwf   Trw, F, A
        return

; : mclr  ( mask addr -- )
;  dup >r c@ swap invert and r> c!
; ;
        dw      L_MSET
L_MCLR:
        NF 4,mclr
MCLR_:
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A
        movf    Sminus, W, A
        comf    Sminus, W, A
        andwf   Trw, F, A
        return

; Print restart reason
RQ:
        call    CR
RQ_DIVZERO:
        btfss   BANKMASK(reset2), 0, A
        bra     RQ_STKFUL
        rcall   XSQUOTE
        db      1,'M'
        rcall   TYPE
RQ_STKFUL:
        btfss   BANKMASK(reset0), 7, A
        bra     RQ_STKUNF
        rcall   XSQUOTE
        db      1,"O"
        rcall   TYPE
RQ_STKUNF:
        btfss   BANKMASK(reset0), 6, A
        bra     RQ_BOR
        rcall   XSQUOTE
        db      1,"U"
        rcall   TYPE
RQ_BOR:
        btfsc   BANKMASK(reset1), 0, A       ; BOR
        bra     RQ_POR
        rcall   XSQUOTE
        db      1,"B"
        rcall   TYPE
RQ_POR:
        btfsc   BANKMASK(reset1), 1, A       ; POR
        bra     RQ_TO
        rcall   XSQUOTE
        db      1,"P"
        rcall   TYPE
RQ_TO:
#ifndef K42
        btfsc   BANKMASK(reset1), 3, A
#else
        btfsc   BANKMASK(reset1), 4, A         ; RWDT
#endif
        bra     RQ_RI
        rcall   XSQUOTE
        db      1,"W"
        rcall   TYPE
RQ_RI:
        btfsc   BANKMASK(reset1), 2, A         ; RI
        bra     RQ_END
        rcall   XSQUOTE
        db      1,"S"
        rcall   TYPE
RQ_END:
        return

; : mtst ( mask addr -- flag )
;   c@ and
; ;
        dw      L_MCLR
L_MTST:
        NF 4,mtst
MTST:
        rcall   CFETCH
        goto    AND_

;;; Fcy returns the cpu clock Fcy in KHz. Unsigned value.
        dw      L_MTST
L_CPUCLK:
        NF 3,Fcy
        rcall   DOCREATE
        dw      clock / 4000

        dw      L_CPUCLK
L_OPERATOR:
        NF 8,operator
OPERATOR:
        call    DOCREATE        ; Must be a call !
        dw      OPERATOR_AREA
OPERATOR_AREA:
        dw      u0+PRAM         ; User pointer
        dw      UADDSIZE, RETURN_STACK_SAVE_SIZE
        dw      ussize, utibsize

; I,   x --             append cell to Flash
;   IHERE ! 1 CELLS IALLOT ;
        dw      L_OPERATOR
L_ICOMMA:
        NF2     2,i
        db      ','
        align   2
ICOMMA:
        rcall   IHERE_P
        rcall   STORE
        rcall   CELL
        goto    IALLOT

;   IHERE ! 1 CHARS IALLOT ;
        dw      L_ICOMMA
L_ICCOMMA:
        NF2     3,ic
        db      ','
        align   2
ICCOMMA:
        rcall   IHERE_P
        rcall   CSTORE
        rcall   ONE
        goto    IALLOT

;   LSHIFT      x1 u -- x2
        dw      L_ICCOMMA
L_LSHIFT:
        NF 6,lshift
LSHIFT:
        swapf   Sminus, W, A
        movf    Sminus, W, A
        bz      LSHIFT2
        movwf   Tp, A
        swapf   Sminus, W, A
LSHIFT1:
        bcf     CARRY
        rlcf    Splus, F, A
        rlcf    Sminus, F, A

        decfsz  Tp, F, A
        bra     LSHIFT1
        swapf   plusS, W, A
LSHIFT2:
        return

;   RSHIFT      x1 u -- x2
        dw      L_LSHIFT
L_RSHIFT:
        NF 6,rshift
RSHIFT:
        swapf   Sminus, W, A
        movf    Sminus, W, A
        bz      RSHIFT2
        movwf   Tp, A
RSHIFT1:
        bcf     CARRY
        rrcf    Sminus, F, A
        rrcf    Splus, F, A

        decfsz  Tp, F, A
        bra     RSHIFT1
RSHIFT2:
        return
;*******************************************************
; Assembler
;*******************************************************
;: as0 ( opcode "name" -- ) ( -- )
;  co:
;  does> i, ;
;
;: as2 ( opcode "name" -- ) ( f a -- )
;  co:
;  does> rot ic, or ic, ;

;       as1 ( opcode "name" -- ) ( k -- )
        dw      L_RSHIFT
L_AS1:
        NF 3,as1
AS1:
        rcall   CONSTANT_
        call    XDOES
AS1_DOES:
        rcall   DODOES
AS1_1:
        rcall   OR_A
        bra     ICOMMA

;       as2 ( opcode "name" -- ) ( f a -- )
        dw      L_AS1
L_AS2:
        NF 3,as2
AS2:
        rcall   CONSTANT_
        call    XDOES
AS2_DOES:
        rcall   DODOES
        rcall   ROT
        rcall   ICCOMMA
        bra     AS3_2

;       as3 ( opcode "name" --) ( f d/b a -- )
;       write a 3 operand asm intruction to flash
        dw      L_AS2
L_AS3:
        NF IMMED|3,as3
AS3:
        rcall   CONSTANT_
        call    XDOES
AS3_DOES:
        rcall   DODOES          ;  f d/b a opcode
        rcall   TOR             ;  f d/b A
        rcall   ROT             ;  d/b a f
        rcall   ICCOMMA         ;  d/b a
        rcall   SWOP            ;  a d/b
        call    TWOSTAR
        rcall   OR_A
        rcall   RFROM
AS3_2:
        rcall   OR_A
        bra     ICCOMMA

;       br2 ( opcode "name" -- ) ( rel-addr -- ) \ bra and rcall
        dw      L_AS3
L_BR2:
        NF 3,br2
BR2:
        rcall   CONSTANT_
        call    XDOES
BR2_DOES:
        rcall   DODOES
        rcall   SWOP            ; opcode rel-addr
        rcall   LIT
        dw      0x0fff          ; opcode rel-addr limit
        call    BRQ             ; opcode clipped-rel-addr
        bra     AS1_1

;       br3 ( opcode "name" -- ) ( abs-addr -- ) \ goto and call
        dw      L_BR2
L_BR3:
        NF 3,br3
BR3:
        rcall   CONSTANT_
        call    XDOES
BR3_DOES:
        rcall   DODOES          ; abs-addr opcode
        rcall   TOR             ; abs-addr
        rcall   LIT
        dw      01
        rcall   RSHIFT          ; abs-addr
        rcall   DUP
        rcall   LIT             ; abs-addr abs-addr ff
        dw      0xff
        rcall   AND_
        rcall   RFROM
        rcall   OR_A
        rcall   ICOMMA
        rcall   LIT
        dw      08
        rcall   RSHIFT
        rcall   LIT
        dw      0xf000
        bra     AS1_1

;       goto, ( abs-addr -- )
        dw      L_BR3
L_GOTO:
        NF2     5,goto
        db      ','
        align   2
GOTO_:
        rcall   BR3_DOES
        dw      0xef00

;       call, ( abs-addr -- )
        dw      L_GOTO
L_CALL:
        NF2     5,call
        db      ','
        align   2
CALL_:
        rcall   BR3_DOES
        dw      0xec00

;       rcall, ( rel-addr -- )
        dw      L_CALL
L_RCALL:
        NF2     6,rcall
        db      ','
        align   2
RCALL_:
        rcall   BR2_DOES
        dw      0xd800

;       bra, ( rel-addr -- )
        dw      L_RCALL
L_BRA:
        NF2     4,bra
        db      ','
        align   2
BRA_:
        rcall   BR2_DOES
        dw      0xd000

;       bcf, ( f b a -- )
        dw      L_BRA
L_BCF:
        NF2     4,bcf
        db      ','
        align   2
BCF_:
        rcall   AS3_DOES
        dw      0x0090

;       bsf, ( f b a -- )
        dw      L_BCF
L_BSF:
        NF2     4,bsf
        db      ','
        align   2
BSF_:
        rcall   AS3_DOES
        dw      0x0080

;       btfsc, ( f b a -- )
        dw      L_BSF
L_BTFSC:
        NF2     6,btfsc
        db      ','
        align   2
BTFSC_:
        rcall   AS3_DOES
        dw      0x00b0

;       btfss, ( f b a -- )
        dw      L_BTFSC
L_BTFSS:
        NF2     6,btfss
        db      ','
        align   2
BTFSS_:
        rcall   AS3_DOES
        dw      0x00a0

;;;
LOCKED:
        btfss   BANKMASK(FLAGS1), fLOCK, A
        return
        bra     ISTORERR
;******************************************************
#ifdef  PPSLOCK
        dw      L_BTFSS

L_PPS_UNLOCK:
        NF 4,pps+
PPS_UNLOCK:
        banksel PPSLOCK         ; required sequence
        movlw   0x55
        movwf   PPSLOCK, B
        movlw   0xAA
        movwf   PPSLOCK, B
        bcf     PPSLOCKED       ; disable the pps lock
        return
        dw      L_PPS_UNLOCK
L_PPS_LOCK:
        NF 4,pps-
PPS_LOCK:
        movlw   0x55
        movwf   PPSLOCK, B
        movlw   0xAA
        movwf   PPSLOCK, B
        bsf     PPSLOCKED         ; enable the pps lock
        return

        dw      L_PPS_LOCK
#else
        dw      L_BTFSS
#endif
L_EMPTY:
        NF 5,empty
EMPTY:
        rcall   LIT
        dw      STARTV
        rcall   LIT
        dw      dp_start
        rcall   LIT
        dw      12
        call    CMOVE
        goto    DP_TO_RAM

#ifdef USB_CDC
        dw      L_EMPTY
L_USB_ON:
        NF 4,usb+
USB_ON:
        btfsc   USBEN
        bra     USB_ON_RET
#ifdef USB_SPEED
#if USB_SPEED == 0
        movlw   0x10
#else
        movlw   0x14
#endif
#endif
        movwf   UCFG, A
        clrf    UCON, A
        bsf     USBEN
        clrf    UIR, A
        banksel usb_device_state
        clrf    BANKMASK(usb_device_state), B
        clrf    ep2icount, A
#ifdef USB_CDC
#if USB_SPEED == 1
#ifdef ACTCON
        bsf     ACTSRC
        bsf     ACTEN
#endif
#endif
#endif
USB_ON_RET:
        return
;*******************************************************
        dw      L_USB_ON
L_USB_OFF:
        NF 4,usb-
USB_OFF:
        bsf     SUSPND
        clrf    UCON, A
        banksel usb_device_state
        clrf    BANKMASK(usb_device_state), B
#ifdef ACTCON
        bcf     ACTEN
#endif

        return
;*******************************************************
        dw      L_USB_OFF
#endif
;*******************************************************
;**********************************************************
        NF 2,or
OR_A:
        bra     OR
;************************************************************
;;; Check parameter stack pointer
        NF 3,sp?
check_sp:
        rcall   SPFETCH
        call    S0
        rcall   FETCH
        call    TIB
        rcall   WITHIN
        rcall   XSQUOTE
        db      3
        STRING  SP?
        align   2
        call    QABORT
        return
;***************************************************
; EMIT  c --    output character to the emit vector
#ifdef USB_CDC
        dw      L_USB_OFF
#else
        dw      L_EMPTY
#endif
L_EMIT:
        NF 4,emit
EMIT:
        call    UEMIT
        goto    FEXECUTE

;***************************************************
; KEY   -- c    get char from UKEY vector
        dw      L_EMIT
L_KEY:
        NF 3,key
KEY:
        call    UKEY
        goto    FEXECUTE

;***************************************************
; KEY   -- c    get char from UKEY vector
        dw      L_KEY
L_KEYQ:
        NF 4,key?
KEYQ:
        call    UKEYQ
        goto    FEXECUTE

;***************************************************
; LIT   -- x    fetch inline 16 bit literal to the stack
; 17 clock cycles
;       dw      link
;link    set     $
        NF 3,lit
LIT:
        movf    TOSL, W, A
        movwf   TBLPTRL, A
        movf    TOSH, W, A
        rcall	  FETCHLIT
        movf    TBLPTRH, W, A
        movwf   TOSH, A
        movf    TBLPTRL, W, A
        movwf   TOSL, A
        return

;****************************************************
; EXECUTE  xt -- execute word at xt
; 6 clock cycles
        dw      L_KEYQ
L_EXECUTE:
        NF 7,execute
EXECUTE:
        movf    Sminus, W, A
        movwf   PCLATH, A
        movf    Sminus, W, A
        movwf   PCL, A           ;  after this, xt is executing

; @EX  addr -- execute xt from addr
; 6 clock cycles
        dw      L_EXECUTE
L_FEXECUTE:
        NF 3,@ex
FEXECUTE:
        rcall   FETCH
        goto    EXECUTE

;****************************************************
;****************************************************
; VARIABLE name --            define a Forth 16 bit VARIABLE
; The data of this variable is stored in data space (PIC ram, eeprom, rom).
;   CREATE CELL ALLOT ;
        dw      L_FEXECUTE
L_VARIABLE:
        NF 8,variable
VARIABLE_:
        rcall   HERE            ; Make space at here
        rcall   CELL            ; for a cell
        rcall   ALLOT           ; in current data space
        goto    CON_            ; Constant as inline literal

        dw      L_VARIABLE
L_2VARIABLE:
        NF 9,2variable
TWOVARIABLE_:
        rcall   HERE            ; Make space at here
        rcall   LIT             ; for a two cells
        dw      4
        rcall   ALLOT           ; in current data space
        goto    CON_            ; Constant as inline literal

;******************************************************
; CONSTANT x name --      define a Forth constant
;  : CO: CREATE  CELL NEGATE IALLOT I, ;
; Note that the constant is stored in flash.
        dw      L_2VARIABLE
L_CONSTANT:
        NF 3,co:
CONSTANT_:
        call    CREATE      ; Create a word that in runtime leaves the current DP on the stack
        rcall   CELL
        rcall   NEGATE
        call    IALLOT
        goto    ICOMMA      ; Append the constant value

;;; CON is a faster version of CONSTANT for numeric constants
;;; CON does not work in conjuction with DOES> . Use CONSTANT together with DOES>
;;; Execution time for a CON word is 7-8 cycles, 4 more cycles than a inline literal
;;; : con create -6 iallot postpone literal postpone  ; ;
        dw      L_CONSTANT
L_CON:
        NF 8,constant
CON_:
        call    COLON         ; Create a word header
        rcall   LITERAL       ; Append the constant value  as inline literal
        goto    SEMICOLON     ; Compile return

        dw      L_CON
L_2CON:
        NF 9,2constant
TWOCON:
        rcall   SWOP
        call    COLON         ; Create a word header
        rcall   LITERAL       ; Append the constant value  as inline literal
        rcall   LITERAL       ; Append the constant value  as inline literal
        goto    SEMICOLON     ; Compile return

; DOCREATE, code action of CREATE
; Fetch the next cell from program memory to the parameter stack
; 15 clock cycles
;       dw      link
;link    set     $
        NF 3,(c)
DOCREATE: ; -- addr  exec action of CREATE
        movf    TOSL, W, A
        movwf   TBLPTRL, A
        movf    TOSH, W, A
        rcall   FETCHLIT
        decf    STKPTR, F, A
RETURN2:
        return

;;; Resolve the runtime action of the word created by using does>
; 20 clock cycles 6,3 us@12MHz
;       dw      link
;link   set     $
        NF 3,(d)
DODOES:
        movf    TOSL, W, A
        movwf   Tp, A
        movf    TOSH, W, A
        movwf   PCLATH, A
        decf    STKPTR, F, A
        movf    TOSL, W, A
        movwf   TBLPTRL, A
        movf    TOSH, W, A
        call    FETCHLIT
        movf    PCLATH, W, A
        movwf   TOSH, A
        movf    Tp, W, A
        movwf   TOSL, A
        return

;;; Compile inline address as subroutine call
        db      NFA|3,'(',',',')'
        align   2
DOCOMMAXT:
        movf    TOSL, W, A
        movwf   TBLPTRL, A
        movf    TOSH, W, A
        call    FETCHLIT
        movf    TBLPTRH, W, A
        movwf   TOSH, A
        movf    TBLPTRL, W, A
        movwf   TOSL, A
        goto    COMMAXT

;   SP@     -- addr         get parameter stack pointer
        dw      L_2CON
L_SPFETCH:
        NF 3,sp@
SPFETCH:
        movf    Sp, W, A
        movwf   Tp, A
        movf    Sbank, W, A
        movff_   Tp, plusS
        iorlw   high(PRAM)
        movwf   plusS, A
        return


; !     x addrl addru  --   store x at addr in memory
; 17 clock cycles for ram. 3.5 us @ 12 Mhz
        dw      L_SPSTORE
#if XSTORE == ENABLE
L_XSTORE:
        db      NFA|2,'x','!'
        align   2
        movf    Sminus, W
        movf    Sminus, W
        movwf   BANKMASK(iaddr_up), A
        call    ISTORE
        clrf    BANKMASK(iaddr_up), A
        return

; MEMORY OPERATIONS =============================
; !     x addr --   store x at addr in memory
; 17 clock cycles for ram. 3.5 us @ 12 Mhz
        dw      L_XSTORE
#endif
L_STORE:
        db      NFA|1,'!'
        align   2
STORE:
        movlw   high(PRAM)
        cpfslt  Srw, A
        bra     STORE1
        movlw   high(PEEPROM)
        cpfslt  Srw, A
        bra     ESTORE
        bra     ISTORE_
STORE1:
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A
        movf    Tplus, W, A
        movf    Sminus, W, A
        movwf   Tminus, A
        movf    Sminus, W, A
        movwf   Trw, A
return1:
        return
ISTORE_:
        goto    ISTORE

;   C!      x addr -- store lower byte of cell x in memory
;;; 15 cycles + chkramaddr (6-10 cycles) for ram
        dw      L_STORE
L_CSTORE:
        db      NFA|2,'c','!'
        align   2
CSTORE:
        movlw   high(PRAM)
        cpfslt  Srw, A
        bra     CSTORE1
        movlw   high(PEEPROM)
        cpfslt  Srw, A
        bra     ECSTORE
        bra     ICSTORE_
CSTORE1:
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A
        movf    Sminus, W, A
        movf    Sminus, W, A
        movwf   Trw, A
        return
ICSTORE_:
        goto    ICSTORE

;   @       addr -- x    fetch cell from memory
; 16 cycles for ram.
; 26-33 cycles for rom
        dw      L_CSTORE
L_FETCH:
        NF 1,@
FETCH:
        movlw   high(PRAM)
        cpfslt  Srw, A
        bra     FETCH1
        movlw   high(PEEPROM)
        cpfslt  Srw, A
        bra     EFETCH
        bra     IFETCH_
FETCH1:
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A
FETCH2:
        movf    Tplus, W, A
        movwf   plusS, A
        movf    Tplus, W, A
        movwf   plusS, A
        return
IFETCH_:
        goto    IFETCH


;   C@      addr -- x fetch char from memory
;;; 15 cycles for ram.
        dw      L_FETCH
L_CFETCH:
        NF 2,c@
CFETCH:
        movlw   high(PRAM)
        cpfslt  Srw, A
        bra     CFETCH1
        movlw   high(PEEPROM)
        cpfslt  Srw, A
        bra     ECFETCH_
        bra     ICFETCH_
CFETCH1:
        movf    Sminus, W, A
        movwf   Tbank, A
        movf    Sminus, W, A
        movwf   Tp, A
CFETCH2:
        movf    Trw, W, A
        movwf   plusS, A
        clrf    plusS, A
        return
ICFETCH_:
        goto    ICFETCH
ECFETCH_:
        goto    ECFETCH

;   RPEMPTY     -- EMPTY THE RETURN STACK
;   empty the return stack and jump to the caller
;       dw      link
;link   set     $
        NF 3,rp0
RPEMPTY:
        lfsr    Rptr, urbuf
        movf    TOSH, W, A
        movwf   PCLATH, A    ; Save the return address
        movf    TOSL, W, A
        clrf    STKPTR, A
        movwf   PCL, A

ISTORERR:
ISTORERR2:
        call    DOTS
        rcall   XSQUOTE
        db      4
        STRING  AD?
        db      0x17
        align   2
        rcall   TYPE
        goto    STARTQ2        ; goto    ABORT

;   x@       addrl addru -- x    fetch cell from flash
        dw      L_CFETCH
#if XSTORE == ENABLE
L_XFETCH:
        NF 2,x@
XFETCH:
        movf    Sminus, W
        movf    Sminus, W
        movwf   TBLPTRU
        movf    Sminus, W
        movwf   TBLPTRH
        movf    Sminus, W
        movwf   TBLPTRL
        call    pfetch0
        clrf    TBLPTRU
        return

; DICTIONARY POINTER FOR the current section
; Flash -- sets the data section to flash
        dw      L_XFETCH
#endif
L_FLASH:
ROM_N:
        NF 5,flash
ROM:
        clrf    BANKMASK(cse), A
        return

; EEPROM -- sets the data section to EEPROM data memory
        dw      L_FLASH
L_EEPROM:
EROM_N:
        NF 6,eeprom
EROM:
        movlw   2
        movwf   BANKMASK(cse), A
        return

; RAM -- sets the data section to RAM memory
        dw      L_EEPROM
L_RAM:
FRAM_N:
        NF 3,ram
FRAM:
        movlw   4
        movwf   BANKMASK(cse), A
        return

; DP    -- a-addr
; Fetched from EEPROM
        dw      L_RAM
L_DP:
        NF 2,dp
DP:
        call    IDP
        rcall   CSE
        goto    PLUS


;;;
        NF 3,BANKMASK(cse)
CSE:
        movf    BANKMASK(cse), W, A
        movwf   plusS, A
        clrf    plusS, A
        return

; HERE    -- addr    get current data space ptr
;   DP @ ;
        dw      L_DP
L_HERE:
        NF 4,here
HERE:
        rcall   DP
        goto    FETCH

        NF 5,ihere
IHERE_P:
        goto   IHERE
; ,   x --             append cell to current data space
;   HERE ! CELL ALLOT ;
        dw      L_HERE
L_COMMA:
        db      NFA|1,','
        align   2
COMMA:
        rcall   HERE
        rcall   STORE
        rcall   CELL
        goto    ALLOT

; C,  c --             append char to current data space
;   HERE C! 1 ALLOT ;
        dw      L_COMMA
L_CCOMMA:
        db      NFA|2,'c',','
        align   2
CCOMMA:
        rcall   HERE
        rcall   CSTORE
        rcall   ONE
        goto    ALLOT


; CELL     -- n                 size of one cell
        dw      L_CCOMMA
L_CELL:
        NF INLINE|4,cell
CELL:
        movlw   2
        movwf   plusS,A
        clrf    plusS,A
        return

; ALIGN    --                         align DP
        dw      L_CELL
L_ALIGN:
        NF 5,align
ALIGN_:
        rcall   HERE
        rcall   ALIGNED
        rcall   DP
        goto    STORE

; ALIGNED  addr -- a-addr       align given addr
        dw      L_ALIGN
L_ALIGNED:
        NF 7,aligned
ALIGNED:
        swapf   Sminus, W, A
        incf    Srw, F, A
        bcf     Splus, 0, A
        bnc     ALIGNED1
        incf    Srw, F, A
ALIGNED1:
        return

; CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
        dw      L_ALIGNED
L_CELLPLUS:
        NF 5,cell+
CELLPLUS:
        goto    TWOPLUS

; CELLS    n1 -- n2            cells->adrs units
        dw      L_CELLPLUS
L_CELLS:
        NF 5,cells
CELLS:
        goto    TWOSTAR

; CHAR+    c-addr1 -- c-addr2   add char size
        dw      L_CELLS
L_CHARPLUS:
        NF 5,char+
CHARPLUS:
        goto    ONEPLUS

; CHARS    n1 -- n2            chars->adrs units
        dw      L_CHARPLUS
L_CHARS:
        NF INLINE|5,chars
CHARS:  return



; cf,    xt --  append codefield
        dw      L_CHARS
L_COMMAXT:
        db      NFA|3,'c','f',','
        align   2
COMMAXT:
        rcall   DUP
        rcall   IHERE_P
        rcall   MINUS
        rcall   ABS
        rcall   LIT
        dw      0x7f0
        rcall   GREATER
        rcall   ZEROSENSE
        bz      STORECF1
STORECFF1:
        rcall   CALL_
        bra     STORECF2
STORECF1:
        rcall   IHERE_P
        rcall   MINUS
        call    TWOMINUS
        rcall   RCALL_
STORECF2:
        return

; !COLON   --       change code field to docolon
;   -6 IALLOT ;
;       dw      link
;link   set     $
        NF 6,!colon
STORCOLON:
        rcall   LIT
        dw      -6
        goto    IALLOT


; 2@    a-addr -- x1 x2            fetch 2 cells
;   DUP @ SWAP CELL+ @ ;
;   the lower address will appear on top of stack
        dw      L_COMMAXT
L_TWOFETCH:
        NF 2,2@
TWOFETCH:
        rcall   DUP
        rcall   FETCH
        rcall   SWOP
        rcall   CELLPLUS
        goto    FETCH

; 2!    x1 x2 a-addr --            store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
        dw      L_TWOFETCH
L_TWOSTORE:
        db      NFA|2,'2','!'
        align   2
TWOSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   CELLPLUS
        rcall   STORE
        goto    STORE

; 2DROP  x1 x2 --                   drop 2 cells
;   DROP DROP ;
        dw      L_TWOSTORE
L_TWODROP:
#ifndef K42
        NF 5,2drop
#else
        NF INLINE|5,2drop
#endif
TWODROP:
#ifndef K42
        rcall   DROP
        goto    DROP
#else
        subfsr  Sptr, 4
        return
#endif

; 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        dw      L_TWODROP
L_TWODUP:
        NF 4,2dup
TWODUP:
        movlw   -3
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        return


; 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        dw      L_TWODUP
L_TWOOVER:
        NF 5,2over
TWOOVER:
        movlw   -7
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        return

; 2SWAP   x1 x2 x3 x4 -- x3 x4 x1 x2    dup top 2 cells
        dw      L_TWOOVER
L_TWOSWAP:
        NF 5,2swap
TWOSWAP:
        rcall   ROT
        rcall   TOR
        rcall   ROT
        rcall   RFROM
        return

; INPUT/OUTPUT ==================================

; SPACE   --                      output a space
;   BL EMIT ;
        dw      L_TWOSWAP
L_SPACE:
        NF 5,space
SPACE_:
        call    BL
        goto    EMIT

; SPACES   n --                  output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        dw      L_SPACE
L_SPACES:
        NF 6,spaces
SPACES:
SPCS1:
        rcall   DUPZEROSENSE
        bz      SPCS2
        rcall   SPACE_
        rcall   ONEMINUS
        bra     SPCS1
SPCS2:  goto    DROP


; umin     u1 u2 -- u           unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
        dw      L_SPACES
L_UMIN:
        NF 4,umin
UMIN:
        rcall   TWODUP
        rcall   UGREATER
        bra     MINMAX

; umax    u1 u2 -- u            unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
        dw      L_UMIN
L_UMAX:
        NF 4,umax
UMAX:
        rcall   TWODUP
        rcall   ULESS
        bra     MINMAX

        dw      L_UMAX
L_ONE:
        NF INLINE|1,1
ONE:
        movlw   1
WTOS:
        movwf   plusS, A
        clrf    plusS, A
        return

; ACCEPT  c-addr +n -- +n'  get line from terminal
        dw      L_ONE
L_ACCEPT:
        NF 6,accept
ACCEPT:
        rcall   OVER
        rcall   PLUS
        rcall   TOR
        rcall   DUP
ACC_1:
        rcall   KEY
        movf    Sminus, W, A
        movf    Splus, W, A
        sublw   8             ; L - W
        bz      ACC_BS_DEL
        sublw   -2            ;
        bz      ACC_LF
        sublw   3             ;
        bz      ACC_CR
        sublw   0x8e
        bz      ACC_BS_DEL
        bra     ACC_3
ACC_CR:
        rcall   FCR             ; Mark CR received
        rcall   CSTORE
        bra     ACC_6           ; CR END OF LINE
ACC_LF:
        rcall   DROP
        rcall   FCR
        rcall   CFETCH
        rcall   ZEROSENSE
        bz      ACC_6           ; LF END OF LINE, CR has not been received
        call    FALSE_
        rcall   FCR
        rcall   CSTORE
        bra     ACC_1           ; CR has been received
ACC_BS_DEL:
        call    FALSE_
        rcall   FCR
        rcall   CSTORE
        rcall   DROP
        rcall   TWODUP
        rcall   EQUAL
        rcall   ZEROSENSE
        bnz     ACC_1
        rcall   ONEMINUS
        rcall   XSQUOTE
        db      3,8,0x20,8
        rcall   TYPE
        bra     ACC_1
ACC_3:
        rcall   DUP
        rcall   EMIT
        rcall   OVER
        rcall   CSTORE
        rcall   ONEPLUS
        rcall   RFETCH
        rcall   OVER
        rcall   EQUAL
        rcall   ZEROSENSE
        bz      ACC_1
ACC_6:
        call    RDROP
        rcall   SWOP
        goto    MINUS

        NF 3,fcr
FCR:
        rcall   DOUSER
        dw      uflg
; TYPE    c-addr u --   type line to terminal u < $100
; : type for c@+ emit next drop ;

        dw      L_ACCEPT
L_TYPE:
        NF 4,type
TYPE:
        rcall   TOR             ; XFOR
        bra     TYPE2           ; XFOR
TYPE1:
        rcall   CFETCHPP
        rcall   EMIT
TYPE2:
        decf    Rrw , F, A      ; XNEXT
        bc      TYPE1           ; XNEXT
        movwf   Rminus, A
        movwf   Rminus, A    ; UNNEXT
        goto    DROP

; (S"    -- c-addr u      run-time code for S"
        dw      L_TYPE
L_XSQUOTE:
        NF 3,(s"
XSQUOTE:
        movf    TOSL, W, A
        movwf   plusS, A
        movf    TOSH, W, A
        movwf   plusS, A
        rcall   CFETCHPP
        rcall   TWODUP
        rcall   PLUS
        rcall   ALIGNED
        movf    Sminus, W, A
        movwf   TOSH, A
        movf    Sminus, W, A
        movwf   TOSL, A
        return


; S"      --            compile in-line string to flash
        dw      L_XSQUOTE
L_SQUOTE:
        NF IMMED|COMPILE|2,s"
SQUOTE:
        rcall   DOCOMMAXT
        dw      XSQUOTE
        rcall   ROM
        rcall   CQUOTE
        goto    FRAM

; ,"      --           store a string to current data space
        dw      L_SQUOTE
L_CQUOTE:
        db      NFA|2,',','"'
        align   2
CQUOTE:
        rcall   LIT
        dw      0x22
        rcall   PARSE
        rcall   HERE
        rcall   OVER
        rcall   ONEPLUS
        rcall   ALLOT
        rcall   PLACE
        goto    ALIGN_


; ."       --            compile string to print into flash
        dw      L_CQUOTE
L_DOTQUOTE:
        NF IMMED|COMPILE|2,."
DOTQUOTE:
        rcall   SQUOTE
        rcall   DOCOMMAXT
        dw      TYPE
        return


; ALLOT   n --    allocate n bytes in current data section
        dw      L_DOTQUOTE
L_ALLOT:
        NF 5,allot
ALLOT:
        rcall   DP
        goto    PLUSSTORE

;************************************************
; DROP  x --                    DROP top of stack
; 2 cycles.
        dw      L_ALLOT
L_DROP:
        NF INLINE|4,drop
DROP:
#ifndef K42
        movwf   Sminus, A       ; no status change
        movwf   Sminus, A
#else
        subfsr  Sptr, 2
#endif
        return


; SWAP  x1 x2 -- x2 x1          SWAP two top items
; 17 cycles (23 movffl)
        dw      L_DROP
L_SWOP:
        NF 4,swap
SWOP:
        movlw   -2
        movff_   SWrw, Tp
        movff_   Srw, SWrw
        movff_   Tp, Sminus
        movff_   SWrw, Tp
        movff_   Srw, SWrw
        movf    Tp, W, A
        movwf   Splus, A
        return

; OVER  x1 x2 -- x1 x2 x1           OVER
; 9 cycles
        dw      L_SWOP
L_OVER:
        NF 4,over
OVER:
        movlw   -3
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        return

;   ROT x1 x2 x3 -- x2 x3 x1        ROT
; 34+24+4=62  cycles
        dw      L_OVER
L_ROT:
        NF 3,rot
ROT:
        rcall   TOR
        rcall   SWOP
        rcall   RFROM
        goto    SWOP
;   >R       x --   R: -- x   push to return stack
; 12 cycles
        dw      L_ROT
L_TOR:
        NF 2,>r
TOR:
        movf    Sminus, W, A
        movwf   plusR, A
        movf    Sminus, W, A
        movwf   plusR, A
        return

;   R> -- x R: x --             pop from R stack
; 12 cycles
        dw      L_TOR
L_RFROM:
        NF 2,r>
RFROM:
        movf    Rminus, W, A
        movwf   plusS, A
        movf    Rminus, W, A
        movwf   plusS, A
        return
;  R@  -- x  R: x -- x         fetch from R stack
;  4 cycles
        dw      L_RFROM
L_RFETCH:
        NF 2,r@
RFETCH:
        movf    Rminus, W, A
        movwf   plusS, A
        movf    Rplus,  W, A
        movwf   plusS, A
        return

;   DUP x -- x x    duplicate top of stack cell
; 9 cycles including call and return
        dw      L_RFETCH
L_DUP:
        NF 3,dup
DUP:
        movlw   -1
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        return

;***********************************************************
;   ABS     n   --- n1      absolute value of n
        dw      L_DUP
L_ABS:
        NF 3,abs
ABS:
        rcall   DUP
        goto    QNEGATE

;   +
        dw      L_ABS
L_PLUS:
        NF 1,+
PLUS:
        movf    Sminus, W, A
        movwf   Tp, A
        movf    Sminus, W, A
        movf    Sminus, F, A
        addwf   Srw, F, A
        movf    Tp, W, A
        addwfc  plusS, F, A
        return

; M+       d n -- d         add single to double
        dw      L_PLUS
L_MPLUS:
        NF 2,m+
MPLUS:
        call    STOD
        goto    DPLUS

;   -   n1/u1 n2/u2 -- n3/u3 n3 = n1 - n2
        dw      L_MPLUS
L_MINUS:
        NF 1,-
MINUS:
        swapf   Sminus, W, A
        movwf   Tp, A
        movf    Sminus, W, A
        movf    Sminus, F, A
        subwf   Srw, F, A
        swapf   Tp, W, A
        subwfb  plusS, F, A
        return

;   AND
        dw      L_MINUS
L_AND:
        NF 3,and
AND_:
        movf    Sminus, W, A
        movff_   Sminus, Tp
        andwf   Sminus, F, A
        movf    Tp, W, A
        andwf   Splus, F, A
        return

;   OR ( n n -- )
        dw      L_AND
L_OR:
        NF 2,or
OR:
        movf    Sminus, W, A
        movff_   Sminus, Tp
        iorwf   Sminus, F, A
        movf    Tp, W, A
        iorwf   Splus, F, A
        return

;   XOR   ( n n -- )
        dw      L_OR
L_XOR:
        NF 3,xor
XOR:
        movf    Sminus, W, A
        movff_   Sminus, Tp
        xorwf   Sminus, F, A
        movf    Tp, W, A
        xorwf   Splus, F, A
        return

;   INVERT
        dw      L_XOR
L_INVERT:
        NF 6,invert
INVERT:
        comf   Sminus, F, A
        comf   Splus, F, A
        return

;   NEGATE
        dw      L_INVERT
L_NEGATE:
        NF 6,negate
NEGATE:
        rcall   INVERT
        goto    ONEPLUS

;   1+
        dw      L_NEGATE
L_ONEPLUS:
        NF 2,1+
ONEPLUS:
        swapf   Sminus, W, A
        infsnz  Splus, F, A
        incf    Srw, F, A
        return

;   1-
        dw      L_ONEPLUS
L_ONEMINUS:
        NF 2,1-
ONEMINUS:
        swapf   Sminus, W, A
        decf    Splus, F, A
        movlw   0
        subwfb  Srw, F, A
        return

; 2+    n -- n-2      2 + CELL+
        dw      L_ONEMINUS
L_TWOPLUS:
        NF 2,2+
TWOPLUS:
        swapf   Sminus, W, A
        movlw   2
        addwf   Splus, F, A
        movlw   0
        addwfc  Srw, F, A
        return

; >body xt -- a-addr transform a created words XT to it's data field address
; : >body 2+ 2+ @ ;
        dw      L_TWOPLUS
L_TOBODY:
        NF 5,>body
TOBODY:
        rcall   TWOPLUS
        rcall   TWOPLUS
        goto    FETCH


;   2*
        dw      L_TOBODY
L_TWOSTAR:
        NF 2,2*
TWOSTAR:
        swapf   Sminus, W, A
        bcf     CARRY
        rlcf    Splus, F, A
        rlcf    Srw, F, A
        return

;   2/
        dw      L_TWOSTAR
L_TWOSLASH:
        NF 2,2/
TWOSLASH:
        bcf     CARRY
        btfsc   Srw, 7, A
        bsf     CARRY
        rrcf    Sminus, F, A
        rrcf    Splus, F, A
        return

;   +!      n/u addr --     add cell to data memory
        dw      L_TWOSLASH
L_PLUSSTORE:
        db      NFA|2,'+','!'
        align   2
PLUSSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   FETCH
        rcall   PLUS
        rcall   SWOP
        goto    STORE

;***************************************************
;   WITHIN      ( u ul uh -- t )
;               Return true if u is within the range of ul and uh. ( ul <= u < uh )
        dw      L_PLUSSTORE
L_WITHIN:
        NF 6,within
WITHIN:
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   MINUS
        rcall   RFROM
        goto    ULESS

;***************************************************
;   <>      x1 x2 -- flag       return true if not equal
        dw      L_WITHIN
L_NOTEQUAL:
        db      NFA|2,'<','>'
        align   2
NOTEQUAL:
        rcall   MINUS
        movf    Sminus, W, A
        iorwf   Srw, A
        bnz     test_true               ; x1 not equal to x2
        bra     test_false              ; x1 equal to x2

;***************************************************
;   0=      n/u -- flag         return true if TOS=0
        dw      L_NOTEQUAL
L_ZEROEQUAL:
        NF 2,0=
ZEROEQUAL:
        movf    Sminus, W, A
        iorwf   Srw, W, A
        bnz     test_false
test_true:                      ; TOS is ffff (TRUE)
        setf    Srw, A
        setf    plusS, A
        return

;***************************************************
;   0<      n -- flag           return true if TOS is negative
        dw      L_ZEROEQUAL
L_ZEROLESS:
        db      NFA|2,'0','<'
        align   2
ZEROLESS:
        btfsc   Sminus, 7, A
        bra     test_true
test_false:                    ; TOS is 0000 (FALSE)
        clrf    Srw, A         ; TOS_LO = 00
        clrf    plusS, A       ; TOS_HI = 00
        return

;***************************************************
;   =       x1 x2 -- flag       return true if x1 = x2
        dw      L_ZEROLESS
L_EQUAL:
        NF 1,=
EQUAL:
        rcall   MINUS
        goto    ZEROEQUAL

;***************************************************
;   <       n1 n2 -- flag       return true if n1 < n2
        dw      L_EQUAL
L_LESS:
        db      NFA|1,'<'
        align   2
LESS:
        rcall   MINUS               ; n1 - n2 in TOS
LESS2:
        swapf   Sminus, W, A
        movf    STATUS, W, A
        andlw   0x18
        xorlw   0x10
        bz      test_true
        xorlw   0x18
        bz      test_true
        bra     test_false

;***************************************************
;   >       n1 n2 -- flag       return true if n1 > n2
        dw      L_LESS
L_GREATER:
        NF 1,>
GREATER:
        rcall   SWOP
        goto    LESS

;***************************************************
;   U<      u1 u2 -- flag       test unsigned less
        dw      L_GREATER
L_ULESS:
        db      NFA|2,'u','<'
        align   2
ULESS:
        rcall   MINUS
        swapf   Sminus, W, A
        bnc     test_true
        bra     test_false

;***************************************************
;   U>      u1 u2 -- flag       test unsigned greater than
        dw      L_ULESS
L_UGREATER:
        NF 2,u>
UGREATER:
        rcall   SWOP
        goto    ULESS

;***************************************************
        dw      L_UGREATER
L_STORE_P:
        db      NFA|2,'!','p'
        align   2
STORE_P:
        movf    Sminus, W, A
        movwf   BANKMASK(p_hi), A
        movf    Sminus, W, A
        movwf   BANKMASK(p_lo), A
        return

;***************************************************
        dw      L_STORE_P
L_STORE_P_TO_R:
        db      NFA|4,'!'
        STRING  p>r
        align   2
STORE_P_TO_R:
        movf    BANKMASK(p_hi), W, A
        movwf   plusR, A
        movf    BANKMASK(p_lo), W, A
        movwf   plusR, A
        goto    STORE_P           ; Set the new pointer
;***************************************************
        dw      L_STORE_P_TO_R
L_R_TO_P:
        NF 3,r>p
R_TO_P:
        movf    Rminus, W, A
        movwf   BANKMASK(p_lo), A
        movf    Rminus, W, A
        movwf   BANKMASK(p_hi), A
        return
;***************************************************
        dw      L_R_TO_P
L_PFETCH:
        NF 2,p@ ; ( -- u ) Fetch cell from pointer
PFETCH:
        movf    BANKMASK(p_lo), W, A
        movwf   plusS, A
        movf    BANKMASK(p_hi), W, A
        movwf   plusS, A
        goto    FETCH
;***************************************************
        dw      L_PFETCH
L_PSTORE:
        db      NFA|2,'p','!'   ; store cell to pointer
        align   2
PSTORE:
        movf    BANKMASK(p_lo), W, A
        movwf   plusS, A
        movf    BANKMASK(p_hi), W, A
        movwf   plusS, A
        goto    STORE
;***************************************************
        dw      L_PSTORE
L_PCSTORE:
        db      NFA|3
        STRING  pc    ; store char to pointer
        db      '!'
        align   2
PCSTORE:
        movf    BANKMASK(p_lo), W, A
        movwf   plusS, A
        movf    BANKMASK(p_hi), W, A
        movwf   plusS, A
        goto    CSTORE
;***************************************************
        dw      L_PCSTORE
L_PPLUS:
        NF INLINE|2,p+  ; Increment p by one
PPLUS:
        infsnz  BANKMASK(p_lo), F, A
        incf    BANKMASK(p_hi), F, A
        return
;***************************************************
        dw      L_PNPLUS
L_PTWOPLUS:
kernellink:
        NF 3,p2+ ; ( n -- ) Add 2 to p
PTWOPLUS:
        movlw   2
        addwf   BANKMASK(p_lo), F, A
        movlw   0
        addwfc  BANKMASK(p_hi), F, A
        return
;***************************************************
; 'EMIT  -- addr         Address of EMIT user vector
        dw      L_PPLUS
L_UEMIT:
        db      NFA|5,"'"
        STRING  emit
        align   2
UEMIT:
        rcall   DOUSER
        dw      uemit&0xffff

;***************************************************
; UKEY  -- addr         Address of KEY user vector
        dw      L_UEMIT
L_UKEY:
        db      NFA|4,"'"
        STRING  key
        align   2
UKEY:
        rcall   DOUSER
        dw      ukey&0xffff

;***************************************************
; UKEYQ  -- addr         Address of KEYQ user vector
        dw      L_UKEY
L_UKEYQ:
        db      NFA|5,"'"
        STRING  key?
        align   2
UKEYQ:
        rcall   DOUSER
        dw      ukeyq&0xffff

;***************************************************

;  n --    Set the Zero STATUS bit if TOS is zero.
;          ALWAYS inlined by the compiler.
;       dw      link
;link   set     $
        NF 3,?0=
ZEROSENSE:
        movf    Sminus, W, A
        iorwf   Sminus, W, A
        return

;  n -- n  Set the Zero STATUS bit if TOS is zero.
;;; DupZerosense that does not destroy the top of stack.
        NF 3,d0=
DUPZEROSENSE:
        movf    Sminus, W, A
        iorwf   Splus, W, A
        return


; MULTIPLY AND DIVIDE ===========================
; UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
        dw      L_UKEYQ
L_UMSTAR:
        NF 3,um*
UMSTAR:
        goto    umstar0

; UM/MOD   ud u1 -- u2(rem) u3(quot)     unsigned 32/16->16
        dw      L_UMSTAR
L_UMSLASHMOD:
        NF 6,um/mod
UMSLASHMOD:
        goto    umslashmod0

; U/MOD   u1 u2 -- u3(rem) u4(quot)     16/16->16 divide
;   0 swap um/mod
;
        dw      L_UMSLASHMOD
L_USLASHMOD:
        NF 5,u/mod
USLASHMOD:
        rcall   FALSE_
        rcall   SWOP
        goto    umslashmod0

; *      n1|u1 n2|u2 -- n3|u3      16*16->16 multiply
;   um* drop ;
        dw      L_USLASHMOD
L_STAR:
        NF 1,*
STAR:
        rcall   UMSTAR
        goto    DROP

; U/      u1 u2 -- u3      16/16-> divide
        dw      L_STAR
L_USLASH:
        NF 2,u/
USLASH:
        rcall   USLASHMOD
        goto    NIP


; U*/MOD  u1 u2 u3 -- u4 u5    u1*u2/u3, rem&quot
;   >R UM* R> UM/MOD ;
        dw      L_USLASH
L_USSMOD:
        NF 6,u*/mod
USSMOD:
        rcall   TOR
        rcall   UMSTAR
        rcall   RFROM
        goto    UMSLASHMOD



; / n1 n2 -- n3  signed 16/16->16 divide
        dw      L_USSMOD
L_SLASH:
        NF 1,/
SLASH:
        rcall   TWODUP
        rcall   XOR
        rcall   TOR
        rcall   ABS
        rcall   SWOP
        rcall   ABS
        rcall   SWOP
        rcall   USLASH
        rcall   RFROM
        goto    QNEGATE

;   NIP x1 x2 -- x2         NIP
        dw      L_SLASH
L_NIP:
        NF 3,nip
NIP:
        movlw   -1
        movff_   Sminus, SWrw
        movff_   Sminus, SWrw
        return

;   TUCK    x1 x2 -- x2 x1 x2
        dw      L_NIP
L_TUCK:
        NF 4,tuck
TUCK:
        rcall   SWOP
        goto    OVER

;***************************************************
; ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;
        dw      L_TUCK
L_QNEGATE:
        NF 7,?negate
QNEGATE:
        rcall   ZEROLESS
        rcall   ZEROSENSE
        bz      QNEGATE1
        rcall   NEGATE
QNEGATE1:
        return

; MAX    n1 n2 -- n3              signed maximum
;   2DUP < IF SWAP THEN DROP ;
        dw      L_QNEGATE
L_MAX:
        NF 3,max
MAX:
        rcall   TWODUP
        rcall   LESS
MINMAX:
        rcall   ZEROSENSE
        bz      max1
        rcall   SWOP
max1:   goto    DROP


; MIN    n1 n2 -- n3              signed minimum
;   2DUP > IF SWAP THEN DROP ;
        dw      L_MAX
L_MIN:
        NF 3,min
MIN:    rcall   TWODUP
        rcall   GREATER
        bra     MINMAX

        NF 2,c@
CFETCH_A:
        bra     CFETCH


; UP    -- a-addr       Current User Area pointer
        dw      L_MIN
L_UPTR:
        NF 2,up
UPTR:
        rcall   DOCREATE_A
        dw      BANKMASK(upcurr)+PRAM

; NUMERIC OUTPUT ================================
; HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
        dw      L_UPTR
L_HOLD:
        NF 4,hold
HOLD:   rcall   TRUE_
        rcall   HP
        rcall   PLUSSTORE
        rcall   HP
        rcall   FETCH
        goto    CSTORE

; <#    --              begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
        dw      L_HOLD
L_LESSNUM:
        db      NFA|2,'<','#'
        align   2
LESSNUM:
        rcall   PAD
        rcall   HP
        goto    STORE

; digit   n -- c            convert to 0..9a..z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
        dw      L_LESSNUM
L_TODIGIT:
        NF 5,digit
TODIGIT:
        movf    Sminus, W, A
        movf    Srw, W, A
        addlw   0xf6
        bn      TODIGIT1
        addlw   0x27
TODIGIT1:
        addlw   0x3a
        movwf   Srw, A
        clrf    plusS, A
        return


; #     ud1 -- ud2     convert 1 digit of output
;   base @ ud/mod rot digit hold ;
        dw      L_TODIGIT
L_NUM:
        NF 1,#
NUM:
        rcall   BASE
        rcall   FETCH
        rcall   UDSLASHMOD
        rcall   ROT
        rcall   TODIGIT
        goto    HOLD

; #S    ud1 -- ud2      convert remaining digits
;   begin # 2dup or 0= until ;
        dw      L_NUM
L_NUMS:
        NF 2,#s
NUMS:
        rcall   NUM
        rcall   TWODUP
        rcall   OR
        rcall   ZEROSENSE
        bnz     NUMS
        return

; #>    ud1 -- c-addr u    end conv., get string
;   2drop hp @ pad over - ;
        dw      L_NUMS
L_NUMGREATER:
        NF 2,#>
NUMGREATER:
        rcall   TWODROP
        rcall   HP
        rcall   FETCH
        rcall   PAD
        rcall   OVER
        goto    MINUS


; SIGN  n --               add minus sign if n<0
;   0< IF 2D HOLD THEN ;
        dw      L_NUMGREATER
L_SIGN:
        NF 4,sign
SIGN:
        rcall   ZEROLESS
        rcall   ZEROSENSE
        bz      SIGN1
        rcall   LIT_A
        dw      2Dh
        rcall   HOLD
SIGN1:
        return

; U.    u --                  display u unsigned
;   <# 0 #S #> TYPE SPACE ;
        dw      L_SIGN
L_UDOT:
        NF 2,u.
UDOT:
        rcall   LESSNUM
        rcall   FALSE_
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; U.R    u +n --      display u unsigned in field of n. 1<n<=255
;    0 swap <# 1- for # next #s #> type space ;
        dw      L_UDOT
L_UDOTR:
        NF 3,u.r
UDOTR:
        rcall   LESSNUM
        rcall   ONEMINUS
        rcall   TOR
        rcall   FALSE_
        bra     UDOTR2
UDOTR1:
        rcall   NUM
UDOTR2:
        decf    Rrw, F, A      ;  XNEXT
        bc      UDOTR1
        movwf   Rminus, A
        movwf   Rminus, A      ; UNNEXT
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        rcall   SPACE_
        return

; .     n --                    display n signed
;   <# DUP ABS #S SWAP SIGN #> TYPE SPACE ;
        dw      L_UDOTR
L_DOT:
        NF 1,.
DOT:    rcall   LESSNUM
        rcall   DUP
        rcall   ABS
        rcall   FALSE_
        rcall   NUMS
        rcall   ROT
        rcall   SIGN
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; DECIMAL  --         set number base to decimal
;   #10 BASE ! ;
        dw      L_DOT
L_DECIMAL:
        NF 7,decimal
DECIMAL:
        rcall   TEN
        rcall   BASE
        goto    STORE

; HEX     --              set number base to hex
;   #16 BASE ! ;
        dw      L_DECIMAL
L_HEX:
        NF 3,hex
HEX_:
        rcall   LIT_A
        dw      16
        rcall   BASE
        goto    STORE

; BIN     --              set number base to binary
;   #2 BASE ! ;
        dw      L_HEX
L_BIN:
        NF 3,bin
BIN:    rcall   CELL
        rcall   BASE
        goto    STORE

; ULINK   -- a-addr     link to next task
        dw      L_BIN
L_ULINK:
        NF 5,ulink
ULINK:  rcall   DOUSER
        dw      ulink&0xffff


; TASK       -- a-addr              TASK pointer
        dw      L_ULINK
L_TASK:
        NF 4,task
TASK:   rcall   DOUSER
        dw      utask&0xffff


; HP       -- a-addr                HOLD pointer
        dw      L_TASK
L_HP:
        NF 2,hp
HP:     rcall   DOUSER
        dw      uhp&0xffff

; PAD     -- a-addr        User Pad buffer
        dw      L_HP
L_PAD:
        NF 3,pad
PAD:
        rcall   TIB
        rcall   TIBSIZE
        goto    PLUS

; BASE    -- a-addr       holds conversion radix
        dw      L_PAD
L_BASE:
        NF 4,base
BASE:
        rcall   DOUSER
        dw      ubase&0xffff

; USER   n --        holds conversion radix
; 18 cycles
        dw      L_BASE
L_USER:
        NF 4,user
USER:
        call    CONSTANT_
        rcall   XDOES
DOUSER:
        movf    TOSL, W, A
        movwf   TBLPTRL, A
        movf    TOSH, W, A
        movwf   TBLPTRH, A
        tblrd*+
        movf    TABLAT, W, A
        addwf   BANKMASK(upcurr), W, A
        movwf   plusS, A
        tblrd*+
        movf    TABLAT, W, A
        addwfc  BANKMASK(upcurr)+1, W, A
        movwf   plusS, A
        decf    STKPTR, F, A         ; return to the callers caller
        return

; SOURCE   -- adr n         current input buffer
;   'SOURCE 2@ ;        length is at higher adrs
        dw      L_USER
L_SOURCE:
        NF 6,source
SOURCE:
        rcall   TICKSOURCE
        goto    TWOFETCH

; /STRING  a u n -- a+n u-n          trim string
;   swap over - >r + r>
        dw      L_SOURCE
L_SLASHSTRING:
        NF 7,/string
SLASHSTRING:
        rcall   SWOP
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   PLUS
        rcall   RFROM
        return

;      Skip the rest of the line
        dw      L_SLASHSTRING
L_BSLASH:
        db      NFA|IMMED|1,0x5c
        align   2
BSLASH:
        rcall   SOURCE
        rcall   TOIN
        rcall   STORE_A
        bsf     BANKMASK(FLAGS1), noclear, A
        goto    DROP

; PARSE  char -- c-addr u
        dw      L_BSLASH
L_PARSE:
        NF 5,parse
PARSE:
        rcall   DUP             ; c c
        rcall   SOURCE          ; c c a u
        rcall   TOIN            ; c c a u a
        rcall   FETCH_A         ; c c a u n
        rcall   SLASHSTRING     ; c c a u   new tib addr/len
        rcall   DUP             ; c c a u u
        rcall   TOR             ; c c a u                  R: u (new tib len
        rcall   ROT             ; c a u c
        call    SKIP            ; c a u
        rcall   OVER            ; c a u a
        rcall   TOR             ; c a u                    R: u a (start of word
        rcall   ROT             ; a u c
        call    SCAN            ; a u      end of word, tib left
        rcall   DUPZEROSENSE
        bz      PARSE1
        rcall   ONEMINUS
PARSE1: rcall   RFROM           ; a u a
        rcall   RFROM           ; a u a u
        rcall   ROT             ; a a u u
        rcall   MINUS           ; a a n  ( addition to toin
        rcall   TOIN
        rcall   PLUSSTORE       ; aend astart
        rcall   TUCK            ; astart aend astart
        goto    MINUS           ; astart wlen


; WORD   char -- c-addr        word delimited by char and/or TAB
        dw      L_PARSE
L_WORD:
        NF 4,word
WORD:
        rcall   PARSE           ; c-addr wlen
        rcall   SWOP
        rcall   ONEMINUS
        rcall   TUCK
        goto    CSTORE          ; Write the length into the TIB !

; CMOVE  src dst u --  copy u bytes from src to dst
; cmove swap !p for c@+ pc! p+ next drop ;
        dw      L_WORD
L_CMOVE:
        NF 5,cmove
CMOVE:
        rcall   SWOP
        rcall   STORE_P_TO_R
        rcall   TOR             ; Count to return stack
        bra     CMOVE2
CMOVE1:
        rcall   CFETCHPP
        rcall   PCSTORE
        rcall   PPLUS
CMOVE2:
        decf    Rminus, F, A      ;  XNEXT
        movlw   0
        subwfb  Rplus, F, A
        bc      CMOVE1
        movwf   Rminus, A
        movwf   Rminus, A   ; UNNEXT
        rcall   R_TO_P
        goto    DROP


; place  src n dst --     place as counted str
        dw      L_CMOVE
L_PLACE:
        NF 5,place
PLACE:
        rcall   TWODUP
        call    CSTORE
        call    CHARPLUS
        rcall   SWOP
        goto    CMOVE

; :     c@+ ( addr -- addr+1 n ) dup 1+ swap c@ ;
        dw      L_PLACE
L_CFETCHPP:
        NF 3,c@+
CFETCHPP:
        rcall   DUP
        rcall   CFETCH_A
        movlw   -3
        incf    SWrw, F, A
        bnc     CFETCHPP1
        movlw   -2
        incf    SWrw, F, A
CFETCHPP1:
        return

; :     @+ ( addr -- addr+2 n ) dup 2+ swap @ ;
        dw      L_CFETCHPP
L_FETCHPP:
        NF 2,@+
FETCHPP:
        rcall   DUP
        rcall   TWOPLUS
        rcall   SWOP
        goto    FETCH
;;; ******************************************************
        db      NFA|1,'!'
STORE_A:
        goto    STORE

; N>C   nfa -- cfa    name adr -> code field
        dw      L_FETCHPP
L_NTOC:
        NF 3,n>c
NFATOCFA:
        rcall   CFETCHPP
        rcall   LIT_A
        dw      0x0f
        rcall   AND_
        rcall   PLUS
        goto    ALIGNED

; C>N   cfa -- nfa    code field addr -> name field addr
        dw      L_NTOC
L_CTON:
        NF 3,c>n
CFATONFA:
        rcall   MINUS_FETCH
        movf    Sminus, W, A
        movf    Sminus, W, A
        bnn     CFATONFA
        return

; findi   c-addr nfa -- c-addr 0   if not found
;                          xt  1      if immediate
;                          xt -1      if "normal"
        dw      L_CTON
L_BRACFIND:
        NF 3,(f)
findi:
findi1:
        movlw   -3
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS    ; c-addr nfa c-addr nfa
        call    NEQUAL          ; c-addr nfa flag
        movf    Sminus, W, A
        iorwf   Sminus, W, A       ; c-addr nfa
        bz      findi2
        swapf   Sminus, W, A
        movlw   2
        subwf   Splus, F, A
        movlw   0
        subwfb  Srw, F, A          ; c-addr lfa
        call    IFETCH          ; c-addr nfa
        movf    Sminus, W, A
        iorwf   Splus, W, A
        bnz     findi1
        bra     findi3
findi2:
        rcall   NIP
        rcall   DUP
        rcall   NFATOCFA
        rcall   SWOP
        rcall   IMMEDQ
        rcall   ZEROEQUAL
        rcall   ONE
        rcall   OR
findi3:
        return

; IMMED?    nfa -- f        fetch immediate flag
        dw      L_BRACFIND
L_IMMEDQ:
        NF 6,immed?
IMMEDQ:
        rcall   CFETCH_A
        movf    Sminus, W, A
        movf    Splus, W, A
        movwf   BANKMASK(wflags), A          ; COMPILE and INLINE flags for the compiler
        rcall   LIT_A
        dw      IMMED
        goto    AND_

; FIND   c-addr -- c-addr 0   if not found
;                  xt  1      if immediate
;                  xt -1      if normal
        dw      L_IMMEDQ
L_FIND:
        NF 4,find
FIND:
        rcall   LIT_A
        dw      kernellink
        rcall   findi
        rcall   DUPZEROSENSE
        bnz     FIND1
        rcall   DROP
        rcall   LAST
        rcall   findi
FIND1:
        return

; DIGIT?   c -- n -1   if c is a valid digit
        dw      L_FIND
L_DIGITQ:
        NF 6,digit?
DIGITQ:
        movf    Sminus, W, A
        movf    Srw, W, A
        addlw   -0x3a
        bn      DIGITQ1
        addlw   -0x27
DIGITQ1:
        addlw   0xa
        movwf   Srw, A
        clrf    plusS, A
        bnn     DIGITQ2
        bra     FALSE_
DIGITQ2:
        rcall   DUP             ; 1 1
        rcall   BASE            ; 1 1 base
        rcall   FETCH_A         ; 1 1 10
        goto    LESS            ; 1 ffff

SLASHONE:
        call   ONE
        goto   SLASHSTRING


; SIGN?   adr n -- adr' n' f   get optional sign
; + leaves $0000 flag
; - leaves $ffff flag
        dw      L_DIGITQ
L_SIGNQ:
        NF 5,sign?
SIGNQ:
        rcall   OVER              ; a n a
        rcall   CFETCH_A          ; a n c
        movf    Sminus, W, A
        movf    Sminus, W, A
        addlw   -'-'
        bz      SIGNQMINUS
        addlw   2
        bz      SIGNQPLUS
        bra     SIGNQEND
SIGNQMINUS:
        rcall   SLASHONE
        bra     TRUE_
SIGNQPLUS:
        rcall   SLASHONE
SIGNQEND:
        goto   FALSE_

; UD*  ud u -- ud
        dw      L_SIGNQ
L_UDSTAR:
        NF 3,ud*
UDSTAR:
        rcall   DUP
        rcall   TOR
        rcall   UMSTAR
        rcall   DROP
        rcall   SWOP
        rcall   RFROM
        rcall   UMSTAR
        rcall   ROT
        goto    PLUS

; UD/MOD  ud u --u(rem) ud(quot)
        dw      L_UDSTAR
L_UDSLASHMOD:
        NF 6,ud/mod
UDSLASHMOD:
        rcall   TOR             ; ud.l ud.h
        rcall   FALSE_          ; ud.l ud.h 0
        rcall   RFETCH          ; ud.l ud.h 0 u
        rcall   UMSLASHMOD      ; ud.l r.h q.h
        rcall   ROT             ; r.h q.h ud.l
        rcall   ROT             ; q.h ud.l r.h
        rcall   RFROM           ; q.h ud.l r.h u
        rcall   UMSLASHMOD      ; q.h r.l q.l
ROT_A:
        goto    ROT             ; r.l q.l q.h

        dw      L_UDSLASHMOD
L_TO_A:
        NF 2,>a
TO_A:
        movf    Sminus, W, A
        movwf   BANKMASK(areg)+1, A
        movf    Sminus, W, A
        movwf   BANKMASK(areg)+0, A
        return

        dw      L_TO_A
L_A_FROM:
        NF 2,a>
A_FROM:
        movf    BANKMASK(areg)+0, W, A
        movwf   plusS, A
        movf    BANKMASK(areg)+1, W, A
        movwf   plusS, A
        return


; >NUMBER  0 0 adr u -- ud.l ud.h adr' u'
;                       convert string to number
        dw      L_A_FROM
L_TONUMBER:
        NF 7,>number
TONUMBER:
        clrf    BANKMASK(areg), A
        clrf    BANKMASK(areg)+1, A
        incf    BANKMASK(areg), F, A
TONUM1:
        rcall   DUPZEROSENSE      ; ud.l ud.h adr u
        bz      TONUM3
        rcall   TOR               ; ud.l ud.h adr
        rcall   DUP
        rcall   TOR               ; ud.l ud.h adr
        rcall   CFETCH_A          ; ud.l ud.h c
        movf    Sminus, W, A
        movf    Splus, W, A
        sublw   '.'
        bz      TONUM_SKIP
        rcall   DIGITQ          ; ud.l ud.h digit flag
        rcall   ZEROSENSE
        bnz     TONUM2
        rcall   DROP
        rcall   RFROM           ; ud.l ud.h adr
        rcall   RFROM           ; ud.l ud.h adr u
        bra     TONUM3
TONUM2:
        rcall   TOR             ; ud.l ud.h
        rcall   BASE
        rcall   FETCH_A
        rcall   UDSTAR
        rcall   RFROM           ; ud.l ud.h digit
        rcall   MPLUS           ; ud.l ud.h
        clrf    BANKMASK(areg), A
        bra     TONUM_CONT
TONUM_SKIP:
        rcall   DROP
TONUM_CONT:
        rcall   RFROM           ; ud.l ud.h adr
        rcall   RFROM           ; ud.l ud.h adr u
        rcall   SLASHONE
        bra     TONUM1
TONUM3:
        rcall   A_FROM
        goto    PLUS

BASEQV:
        dw      DECIMAL
        dw      HEX_
        dw      BIN


; NUMBER?  c-addr -- n 1
;                 -- dl dh 2
;                 -- c-addr 0  if convert error
        dw      L_TONUMBER
L_NUMBERQ:
        NF 7,number?
NUMBERQ:
        rcall   DUP             ; a a
        rcall   FALSE_          ; a a 0 0
        rcall   FALSE_          ; a a 0 0
        rcall   ROT             ; a 0 0 a
        rcall   CFETCHPP        ; a 0 0 a' u
        rcall   SIGNQ           ; a 0 0 a' u f
        rcall   TOR             ; a 0 0 a' u

        rcall   BASE
        rcall   FETCH_A
        rcall   TOR             ; a 0 0 a' u

        rcall   OVER
        rcall   CFETCH_A

        movf    Sminus, W, A
        movlw   '#'
        subwf   Srw, F, A
        bn      BASEQ1
        movlw   3
        subwf   Srw, W, A
        bnn     BASEQ1
        rlncf   Srw, F, A
        movlw   low(BASEQV)
        addwf   Splus, F, A
        movlw   high(BASEQV)
        addwfc  Srw, F, A
        call    FEXECUTE

        rcall   SLASHONE
        bra     BASEQ2
BASEQ1:
        movf    Sminus, W, A
BASEQ2:                         ; a 0 0 a' u
        rcall   TONUMBER        ; a ud.l ud.h  a' u

        rcall   RFROM           ; a ud.l ud.h  a' u oldbase
        rcall   BASE            ; a ud.l ud.h  a' u oldbase addr
        rcall   STORE_A         ; a ud.l ud.h  a' u
                                ; u > 0 -> error
        rcall   ZEROSENSE       ; a ud.l ud.h  a'
        bz      QNUMD           ; u = 0 -> single or double number
QNUM_ERR:                       ; Not a number
        rcall   RFROM           ; a ud.l ud.h a' sign
        call    TWODROP
        call    TWODROP
        bra     FALSE_          ; a 0           Not a number
QNUMD:                          ; Single or double Double number
                                ; a ud.l ud.h a'
        rcall   ONEMINUS
        call    CFETCH          ; a ud.l ud.h c
        rcall   TO_A            ; a ud.l ud.h
        rcall   RFROM           ; a ud.l ud.d sign
        call    QDNEGATE
QNUMD1:
        rcall   ROT_A           ; d.l d.h a
        call    DROP            ; d.l d.h
        call    ONE             ; d.l d.h 1
        movlw   '.'             ; d.l d.h
        subwf   BANKMASK(areg), W, A
        bz      QNUM1
        bra     NIP             ; n 1           Single number
QNUM1:
        goto    ONEPLUS         ; d.l d.h 2     Double number

; TI#  -- n                      size of TIB
; : ti# task @ 8 + @ ;
        dw      L_NUMBERQ
L_TIBSIZE:
        NF 3,ti#
TIBSIZE:
        rcall   TASK
        rcall   FETCH_A
        movlw   8
        call    WTOS
        rcall   PLUS
        goto    FETCH

; TIB     -- a-addr        Terminal Input Buffer
        dw      L_TIBSIZE
L_TIB:
        NF 3,tib
TIB:
        rcall   TIU
        goto    FETCH

; TIU     -- a-addr        Terminal Input Buffer user variable
        dw      L_TIB
L_TIU:
        NF 3,tiu
TIU:
        rcall   DOUSER
        dw      utib&0xffff     ; pointer to Terminal input buffer

; >IN     -- a-addr        holds offset into TIB
; In RAM
        dw      L_TIU
L_TOIN:
        NF 3,>in
TOIN:
        rcall   DOUSER
        dw      utoin&0xffff

; 'SOURCE  -- a-addr        two cells: len, adrs
; In RAM ?
        dw      L_TOIN
L_TICKSOURCE:
        NF 7,source
TICKSOURCE:
        rcall   DOUSER
        dw      usource&0xffff    ; two cells !!!!!!

        NF 3,dup
DUP_A:  goto    DUP

;  INTERPRET  c-addr u --    interpret given buffer
        dw      L_TICKSOURCE
L_INTERPRET:
        NF 9,interpret
INTERPRET:
        rcall   TICKSOURCE
        call    TWOSTORE
        rcall   FALSE_
        rcall   TOIN
        rcall   STORE_A
IPARSEWORD:
        rcall   BL
        rcall   WORD

        rcall   DUP_A
        rcall   CFETCH_A
        rcall   ZEROSENSE
        bz      INOWORD

        rcall   FIND            ; sets also BANKMASK(wflags)
        movf    Sminus, W, A
        movf    Sminus, W, A    ; 0 = not found, -1 = normal, 1 = immediate
        bz      INUMBER
        addlw   1
        bnz     IEXEC           ; Immediate word
        addwf   BANKMASK(state), W, A     ; 0 or ff
        bn      ICOMPILE_1
IEXEC:                          ; Execute a word
                                ; immediate&compiling or interpreting
        btfss   BANKMASK(wflags), 4, A    ; Compile only check
        bra     IEXECUTE        ; Not a compile only word
        rcall   STATE_          ; Compile only word check
        call    XSQUOTE
        db      13
        STRING  COMPILE ONLY
        db      0x17
        align   2
        rcall   QABORT
IEXECUTE:
        bcf     BANKMASK(FLAGS1), noclear, A
        call    EXECUTE
        btfsc   BANKMASK(FLAGS1), noclear, A ;  set by \ and by (
        bra     IPARSEWORD
        bcf     BANKMASK(FLAGS1), izeroeq, A ; Clear 0= encountered in compilation
        bcf     BANKMASK(FLAGS1), idup, A    ; Clear DUP encountered in compilation
        bra     IPARSEWORD
ICOMPILE_1:
        bcf     BANKMASK(FLAGS1), izeroeq, A ; Clear 0= encountered in compilation
        rcall   DUP_A
        rcall   LIT_A
        dw      ZEROEQUAL       ; Check for 0=, modifies IF and UNTIL to use bnz
        rcall   EQUAL
        rcall   ZEROSENSE
        bz      ICOMPILE_2
        bsf     BANKMASK(FLAGS1), izeroeq, A ; Mark 0= encountered in compilation
        bra     ICOMMAXT
ICOMPILE_2:
        bcf     BANKMASK(FLAGS1), idup, A    ; Clear DUP encountered in compilation
        rcall   DUP_A
        rcall   LIT_A
        dw      DUP             ; Check for DUP, modies IF and UNTIl to use DUPZEROSENSE
        rcall   EQUAL
        rcall   ZEROSENSE
        bz      ICOMPILE
        bsf     BANKMASK(FLAGS1), idup, A    ; Mark DUP encountered during compilation
ICOMMAXT:
        rcall   COMMAXT_A
        bcf     BANKMASK(FLAGS1), fTAILC, A  ; Allow tailgoto optimisation
        btfsc   BANKMASK(wflags), 4, A          ; Compile only ?
        bsf     BANKMASK(FLAGS1), fTAILC, A  ; Prevent tailgoto optimisation
        bra     IPARSEWORD
ICOMPILE:
        btfss   BANKMASK(wflags), 5, A    ; Inline check
        bra     ICOMMAXT
        rcall   INLINE0
        bra     IPARSEWORD
INUMBER:
        bcf     BANKMASK(FLAGS1), izeroeq, A ; Clear 0= encountered in compilation
        bcf     BANKMASK(FLAGS1), idup, A    ; Clear DUP encountered in compilation
        rcall   NUMBERQ
        movf    Sminus, W, A
        movf    Sminus, W, A
        bz      IUNKNOWN
        movf    BANKMASK(state), F, A
        bz      INUMBER1
        sublw   1
        bz      ISINGLE
IDOUBLE:
        rcall   SWOP_A
        call    LITERAL
ISINGLE:
        call    LITERAL
INUMBER1:
        bra     IPARSEWORD
IUNKNOWN:
        rcall   DP_TO_RAM
        rcall   CFETCHPP
        call    TYPE
        rcall   FALSE_
        rcall   QABORTQ         ; Never returns & resets the stacks
INOWORD:
        goto    DROP

        NF 1,@
FETCH_A:
        goto    FETCH

;;;    bitmask --
        dw      L_INTERPRET
L_SHB:
        NF 3,shb     ; Set header bit
SHB:
        rcall   LAST
        rcall   DUP_A
        rcall   CFETCH_A
        rcall   ROT_A
        call    OR
        rcall   SWOP_A
        goto    CSTORE

        dw      L_SHB
L_IMMEDIATE:
        NF 9,immediate ;
IMMEDIATE:
        rcall   LIT_A
        dw      IMMED
        bra     SHB

        NF 4,swap
SWOP_A:
        goto    SWOP

;***************************************************************
        dw      L_IMMEDIATE
L_INLINED:
        NF 7,inlined ;
INLINED:
        rcall   LIT_A
        dw      INLINE
        bra     SHB
EMIT_A: goto    EMIT
;; .st ( -- ) output a string with current data section and current base info
;;; : .st base @ dup decimal <#  [char] , hold #s  [char] < hold #> type
;;;     <# [char] > hold BANKMASK(cse) @ #s #> type base ! ;
        dw      L_INLINED
L_DOTSTATUS:
        NF 3,.st
DOTSTATUS:
        rcall   LIT_A
        dw      0x003c
        rcall    EMIT_A
        call    DOTBASE
        rcall    EMIT_A
        rcall   LIT_A
        dw      0x002c
        rcall    EMIT_A
        call    MEMQ
        call    TYPE
        rcall   LIT_A
        dw      0x003e
        rcall    EMIT_A
        goto    DOTS

        NF 3,lit
LIT_A:
        goto    LIT


        NF 2,>r
TOR_A:  goto    TOR


;;; TEN ( -- n ) Leave decimal 10 on the stack
        NF 1,a
TEN:
        movlw   10
        goto    WTOS

; dp> ( -- ) Copy ini, dps and latest from eeprom to ram
;        dw      link
; link    set     $
        NF 3,dp>
DP_TO_RAM:
        rcall   LIT_A
        dw      dp_start
        rcall   INI
        rcall   TEN
        goto    CMOVE

; >dp ( -- ) Copy only changed turnkey, dp's and latest from ram to eeprom
;        dw      link
; link    set     $
        NF 3,>dp
DP_TO_EEPROM:
        rcall   LIT_A
        dw      dp_start
        rcall   STORE_P_TO_R
        rcall   INI
        movlw   5
        movwf   plusR, A
        bra     DP_TO_EEPROM_3
DP_TO_EEPROM_0:
        rcall   FETCHPP
        call    DUP
        rcall   PFETCH
        call    XOR; NOTEQUAL
        movf    Sminus, W, A
        iorwf   Sminus, W, A
        bz      DP_TO_EEPROM_1
        rcall   PSTORE
        bra     DP_TO_EEPROM_2
DP_TO_EEPROM_1:
        call    DROP
DP_TO_EEPROM_2:
        rcall   PTWOPLUS
DP_TO_EEPROM_3:
        decf    Rrw, F, A
        bc      DP_TO_EEPROM_0
        movf    Rminus, W, A
        rcall   R_TO_P
        goto    DROP

;***************************************************************
        dw      L_DOTSTATUS
L_FALSE:
        NF INLINE|5,false
FALSE_:                     ; TOS is 0000 (FALSE)
        clrf    plusS, A         ; TOS_LO = 00
        clrf    plusS, A         ; TOS_HI = 00
        return

        dw      L_FALSE
L_TRUE:
        NF INLINE|4,true
TRUE_:                      ; TOS is ffff (TRUE)
        setf    plusS, A
        setf    plusS, A
        return

; QUIT     --    R: i*x --    interpret from kbd
        dw      L_TRUE
L_QUIT:
        NF 4,quit
QUIT:
        call    RPEMPTY
        rcall   LEFTBRACKET
        call    FRAM
QUIT0:
        ;; Copy INI and DP's from eeprom to ram
        rcall   DP_TO_RAM
QUIT1:
        call    check_sp
        rcall   CR
        rcall   TIB
        rcall   DUP_A
        rcall   TIBSIZE
        rcall   TEN                 ; Reserve 10 bytes for hold buffer
        call    MINUS
        call    ACCEPT
        call    SPACE_
        rcall   INTERPRET
        movf    BANKMASK(state), W, A
        bnz     QUIT1
        call    IFLUSH
        rcall   DP_TO_EEPROM
        call    XSQUOTE
        db      3,' '
        STRING  ok
        call    TYPE
        rcall   PROMPT
        bra     QUIT0
        return

        dw      L_QUIT
L_PROMPT:
        NF 6,prompt
PROMPT:
        call    DEFER_DOES
        dw      prompt

; ABORT    i*x --   R: j*x --   clear stk & QUIT
        dw      L_PROMPT
L_ABORT:
        NF 5,abort
ABORT:
        rcall   S0
        rcall   FETCH_A
        rcall   SPSTORE
        goto    QUIT

        dw      L_SPFETCH
L_SPSTORE:
        NF 3,sp!
SPSTORE:
        movf    Sminus, W, A
        movwf   Tp, A
        movf    Sminus, W, A
        movwf   Sp, A
        movf    Tp, W, A
        movwf   Sbank, A
        return

; ?ABORT?   f --       abort & print ?
        dw      L_ABORT
L_QABORTQ:
        NF 7,?abort?
QABORTQ:
        call    XSQUOTE
        db      2,'?',0x17
        align   2
        goto    QABORT


; ?ABORT   f c-addr u --       abort & print msg
        dw      L_QABORTQ
L_QABORT:
        NF 6,?abort
QABORT:
        rcall   ROT_A
        call    ZEROSENSE
        bnz     QABO1
QABORT1:
        call    SPACE_
        call    TYPE
        rcall   ABORT  ; ABORT never returns
QABO1:  goto    TWODROP

        NF 3,(,)
DOCOMMAXT_A: goto DOCOMMAXT

; ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;         i*x x1 --       R: j*x --      x1<>0
        dw      L_QABORT
L_ABORTQUOTE:
        NF IMMED|COMPILE|6,abort"
ABORTQUOTE:
        call    SQUOTE
        rcall   DOCOMMAXT_A
        dw      QABORT
        return

; '    -- xt             find word in dictionary
        dw      L_ABORTQUOTE
L_TICK:
        db      NFA|1,"'"
TICK:
        rcall   BL
        rcall   WORD
        rcall   FIND
        goto    QABORTQ

; CHAR   -- char           parse ASCII character
        dw      L_TICK
L_CHAR:
        NF 4,char
CHAR:
        rcall   BL
        rcall   PARSE
        call    DROP
        goto    CFETCH

; (    --                     skip input until )
        dw      L_CHAR
L_PAREN:
        NF IMMED|1,(
PAREN:
        rcall   LIT_A
        dw      0x29
        rcall   PARSE
        bsf     BANKMASK(FLAGS1), noclear, A ; dont clear flags in case of (
        goto    TWODROP

        NF 3,(c)
DOCREATE_A:
        goto    DOCREATE

; IHERE    -- a-addr    return Code dictionary ptr
;   IDP @ ;
;;;         dw      link
;;; link    set     $
        NF 5,ihere
IHERE:
        rcall   IDP
        bra     FETCH_A

; [CHAR]   --          compile character literal
        dw      L_PAREN
L_BRACCHAR:
        NF IMMED|COMPILE|6,[char]
BRACCHAR:
        rcall   CHAR
        goto    LITERAL

; COMPILE,  xt --         append codefield
        NF 3,cf,
COMMAXT_A:
        goto    COMMAXT

; CR      --                      output newline
        dw      L_BRACCHAR
L_CR:
        NF 2,cr
CR:
        call    XSQUOTE
        db      2,10,13,0
        goto    TYPE

; CREATE   --         create an empty definition
; Create a definition header and append
; doCREATE and the current data space dictionary pointer
; in FLASH.
;  Examples :
; : table create 10 cells allot does> swap cells + ;
; ram table table_a     flash table table_b    eeprom table table_c
; ram variable  qqq
; eeprom variable www ram
; flash variable  rrr ram
; eeprom create calibrationtable 30 allot ram
;
        dw      L_CR
L_CREATE:
        NF 6,create
CREATE:
        rcall   BL
        rcall   WORD            ; Parse a word

        rcall   DUP_A           ; Remember parsed word at rhere
        rcall   FIND
        call    NIP
        call    ZEROEQUAL
        call    XSQUOTE
        db      15
        STRING  ALREADY_DEFINED
        rcall   QABORT         ; ABORT if word has already been defined

        rcall   DUP_A           ; Check the word length
        call    CFETCH_A
        call    ONE
        rcall   LIT_A
        dw      0x10
        call    WITHIN
        rcall   QABORTQ          ; Abort if there is no name for create

        rcall   IHERE
        call    ALIGNED
        rcall   IDP             ; Align the flash DP.
        rcall   STORE_A

        rcall   LAST
        call    ICOMMA          ; Link field
        rcall   CFETCHPP        ; str len
        rcall   IHERE
        rcall   DUP_A
        rcall   LATEST          ; new 'latest' link
        rcall   STORE_A         ; str len ihere
        rcall   PLACE           ;
        rcall   IHERE           ; ihere
        call    CFETCH_A
        rcall   LIT_A
        dw      NFA
        rcall   SHB
        call    ONEPLUS
        call    ALIGNED
        rcall   IALLOT          ; The header has now been created
        rcall   DOCOMMAXT_A     ; Append an exeution token
        dw      DOCREATE        ; compiles the runtime routine to fetch the next dictionary cell to the parameter stack
        call    ALIGN_
        call    HERE            ; compiles the current dataspace dp into the dictionary
        movf    BANKMASK(cse), W, A
        bnz     CREATE2
        call    TWOPLUS
CREATE2:
        goto    ICOMMA          ; dp now points to a free cell

;***************************************************************
; POSTPONE
        dw     L_CREATE
L_POSTPONE:
        NF IMMED|COMPILE|8,postpone
POSTPONE:
        rcall   BL
        rcall   WORD
        rcall   FIND
        rcall   DUP_A
        rcall   QABORTQ
        call    ZEROLESS
        call    ZEROSENSE
        bz      POSTPONE1
        rcall   DOCOMMAXT_A
        dw      DOCOMMAXT
        goto    ICOMMA
POSTPONE1:
        goto   COMMAXT
;***************************************************************
; IDP    -- a-addr  Dictonary pointer storage
; Stored in EEPROM
        ;;      dw link
        ;; link set $
        NF 3,idp
IDP:
        rcall   DOCREATE_A
        dw      dpFLASH+PRAM


;***************************************************************
; (DOES>)  --      compile the run-time action of DOES>
;        dw     link
;link   set     $
        NF 7,(does>)
XDOES:
        movf    TOSL, W, A
        movwf   plusS, A
        movf    TOSH, W, A
        movwf   plusS, A
        decf    STKPTR, F, A
        rcall   LAST
        rcall   NFATOCFA
        rcall   IDP
        rcall   FETCH_A
        rcall   TOR_A
        rcall   IDP
        rcall   STORE_A
        call    CALL_      ; Always stores a 4 byte call
        call    RFROM
        rcall   IDP
        goto    STORE


; DOES>    --      change action of latest def'n
        dw      L_POSTPONE
L_DOES:
        NF IMMED|COMPILE|5,does>
DOES:   rcall   DOCOMMAXT_A
        dw      XDOES
        rcall   DOCOMMAXT_A
        dw      DODOES
        return

        dw      L_DOES
L_LAST:
        NF 3,lst
LAST:
        rcall   LATEST
        goto    FETCH

;*****************************************************************
; [        --      enter interpretive BANKMASK(state)
        dw      L_LAST
L_LEFTBRACKET:
        NF IMMED|1,[
LEFTBRACKET:
        clrf    BANKMASK(state), A
        return


; ]        --      enter compiling BANKMASK(state)
        dw      L_LEFTBRACKET
L_RIGHTBRACKET:
        NF 1,]
RIGHTBRACKET:
        setf    BANKMASK(state), A
        return

; :        --           begin a colon definition
        dw      L_RIGHTBRACKET
L_COLON:
        NF 1,:
COLON:
        rcall   CREATE
        rcall   RIGHTBRACKET
        goto    STORCOLON

; :noname        -- a          define headerless forth code
        dw      L_COLON
L_NONAME:
        NF 7,:noname
NONAME:
        rcall   IHERE
        goto    RIGHTBRACKET

; ;        --             end a colon definition
        dw      L_NONAME
L_SEMICOLON:
        db      IMMED|COMPILE|1,';'
SEMICOLON:
        rcall   LEFTBRACKET
        btfsc   BANKMASK(FLAGS1), fTAILC, A
        bra     ADD_RETURN_1
        rcall   IHERE
        rcall   MINUS_FETCH
        movf    Sminus, W, A
        movf    Sminus, F, A
        andlw   0xf8
        sublw   0xd8
        bz      RCALL_TO_GOTO
        rcall   MINUS_FETCH
        movf    Sminus, W, A
        movf    Sminus, F, A
        sublw   0xec
        bnz     ADD_RETURN
CALL_TO_GOTO:
        rcall   LIT_A
        dw      0x300
        rcall   SWOP_A
        goto    PLUSSTORE
RCALL_TO_GOTO:
        rcall   FETCH_A
        movlw   0xf8           ; These rcalls are always negative.
        iorwf   Srw, F, A
        call    TWOSTAR
        rcall   IHERE
        call    PLUS
        rcall   LIT_A
        dw      0xfffe          ; -2
        rcall   IALLOT
        goto    GOTO_
ADD_RETURN:
        call    DROP
ADD_RETURN_1:
        rcall   LIT_A   ; Compile a return
        dw      0x0012
        goto    ICOMMA

        dw      L_SEMICOLON
L_MINUS_FETCH:
        NF 2,-@
MINUS_FETCH:
        rcall   TWOMINUS
        rcall   DUP_A
        goto    FETCH

; [']  --         find word & compile as literal
        dw      L_MINUS_FETCH
L_BRACTICK:
        db      IMMED|COMPILE|3,'[',"'",']'
BRACTICK:
        rcall   TICK       ; get xt of 'xxx'
        goto    LITERAL

; 2-    n -- n-2
        dw      L_BRACTICK
L_TWOMINUS:
        NF 2,2-
TWOMINUS:
        swapf   Sminus, W, A
        movlw   2
        subwf   Splus, F, A
        movlw   0
        subwfb  Srw, F, A
        return


; BL      -- char                 an ASCII space
        dw      L_TWOMINUS
L_BL:
        NF 2,bl
BL:
        movlw   0x20
        goto    WTOS

; STATE   -- flag                 holds compiler BANKMASK(state)
        dw      L_BL
L_STATE:
        NF 5,state
STATE_:
        movf    BANKMASK(state), W, A
        movwf   plusS, A
        movwf   plusS, A
        return

; LATEST    -- a-addr
        dw      L_STATE
L_LATEST:
        NF 6,latest
LATEST:
        rcall   DOCREATE_A
        dw      dpLATEST+PRAM

; S0       -- a-addr      start of parameter stack
        dw      L_LATEST
L_S0:
        NF 2,s0
S0:
        call    DOUSER
        dw      us0&0xffff

; ini -- a-addr       ini variable contains the user-start xt
; In RAM
;        dw      link
;link    set     $
;        NF 3,ini
INI:
        rcall   DOCREATE_A
        dw      dpSTART+PRAM

; ticks  -- u      system ticks (0-ffff) in milliseconds
        dw      L_S0
L_TICKS:
        NF 5,ticks
TICKS:
        bcf     GIE
        movf    BANKMASK(ms_count), W, A
        movwf   plusS, A
        movf    BANKMASK(ms_count)+1, W, A
        movwf   plusS, A
        bsf     GIE
        return


; ms  +n --      Pause for n millisconds
; : ms ( +n -- )
;   ticks +
;   begin
;     pause dup ticks - 0<
;   until drop ;
;
        dw      L_TICKS
L_MS:
        NF 2,ms
MS:
        rcall   TICKS
        call    PLUS
MS_1:
        call    PAUSE
        swapf   Sminus, W, A    ; timeout_hi
        movwf   Tp, A
        movf    Splus, W, A     ; timeout_lo
        bcf     GIE
        subwf   BANKMASK(ms_count), W, A  ; ticks_lo - timeout_lo
        swapf   Tp, W, A
        subwfb  BANKMASK(ms_count)+1, W, A ; ticks_hi - timeout_hi
        bsf     GIE
        bn      MS_1
        goto    DROP

CFETCHPP_A: bra CFETCHPP
;  .id ( nfa -- )
        dw      L_MS
L_DOTID:
        NF 3,.id
DOTID:
        rcall   CFETCHPP_A
        movf    Sminus, W, A
        movf    Sminus, W, A
        andlw   0xf
        movwf   plusR, A
        bra     DOTID3
DOTID1:
        rcall   CFETCHPP_A
        rcall   TO_PRINTABLE
        rcall    EMIT_A
DOTID3:
        decf    Rrw, F, A
        bc      DOTID1
        movf    Rminus, W, A
        goto    DROP

 ; >pr   c -- c      Filter a character to printable 7-bit ASCII
        dw      L_DOTID
L_TO_PRINTABLE:
        NF 3,>pr
TO_PRINTABLE:
        movf    Sminus, W, A
        movf    Srw, W, A
        bn      TO_PRINTABLE1
        sublw   0x1f
        bn      TO_PRINTABLE2
TO_PRINTABLE1:
        movlw   '.'
        movwf   Srw, A
TO_PRINTABLE2:
        clrf    plusS, A
        return
;;;;;;;;;;;;;;
CMP:
        rcall   TOR_A
        bra     CMP2
CMP1:
        rcall   CFETCHPP_A
        rcall   ROT_A
        rcall   CFETCHPP_A
        rcall   ROT_A
        call    MINUS
        call    ZEROSENSE
        bnz     TWODROPZ
CMP2:
        decf    Rrw, F, A
        bc      CMP1
        bra     TWODROPNZ
;;;;;;;;;;;;;;;;;;;;
LIKEQ:
        rcall   CFETCHPP_A
        rcall   LIT_A
        dw      0xf
        call    AND_
        rcall   SWOP_A
        call    STORE_P
        rcall   SWOP_A
        rcall   CFETCHPP_A
        rcall   ROT_A
        call    OVER
        call    MINUS
        call    ONEPLUS
        rcall   FALSE_
        call    MAX
        rcall   TOR_A
        bra     LIKEQ3
LIKEQ1:
        call    TWODUP
        rcall   FETCH_P
        call    PPLUS
        rcall   SWOP_A
        rcall   CMP
        bz      LIKEQ3
TWODROPNZ:
        bcf     ZERO
        bra     LIKEQ4
LIKEQ3:
        decf    Rrw, F, A
        bc      LIKEQ1
TWODROPZ:
        bsf     ZERO
LIKEQ4:
        movwf   Rminus, A
        movwf   Rminus, A
        bra     DOTS2

;;;;;;;;;;;;;;;
LIKES:
        call    TWODUP
        rcall   LIKEQ
        bz      LIKES1
        rcall   DUP_A
        rcall   DOTID
        call    SPACE_
LIKES1:
        rcall   TWOMINUS
        rcall   FETCH_A
        call    DUPZEROSENSE
        bnz     LIKES
        bra     DOTS2

 ; WORDS    --          list all words in dict.
        dw      L_TO_PRINTABLE
L_WORDS:
        NF 5,words
        rcall   BL
        call    WORD
        rcall   DUP_A
        rcall   LIT_A
        dw      kernellink
        rcall   WORDS1
        rcall   LAST
WORDS1:
        rcall   CR
        bra     LIKES

; .S      --           print stack contents
; : .s sp@ s0 @ 1+ begin 2dup < 0= while @+ u. repeat 2drop ;
        dw      L_WORDS
L_DOTS:
        NF 2,.s
DOTS:
        call    SPFETCH
        rcall   S0
        rcall   FETCH_A
        call    ONEPLUS
DOTS1:
        call    TWODUP
        call    LESS
        call    ZEROSENSE
        bnz     DOTS2
        call    FETCHPP
        call    UDOT
        bra     DOTS1
DOTS2:
        goto    TWODROP

;   DUMP  ADDR U --       DISPLAY MEMORY
        dw      L_DOTS
L_DUMP:
        NF 4,dump
DUMP:
        rcall   LIT_A
        dw      0x10
        call    USLASH
        rcall   TOR_A
        bra     DUMP7
DUMP1:
        rcall   CR
        rcall   DUP_A
        rcall   LIT_A
        dw      4
        call    UDOTR
        rcall   LIT_A
        dw      0x3a
        rcall   EMIT_A
        movlw   0x10
        movwf   plusR, A
DUMP2:
        rcall   CFETCHPP_A
        rcall   LIT_A
        dw      2
        call    UDOTR
        decf    Rrw, F, A
        bnz     DUMP2
        movf    Rminus, W, A

        rcall   LIT_A
        dw      0x10
        call    MINUS
        movlw   0x10
        movwf   plusR, A
DUMP4:
        rcall   CFETCHPP_A
        rcall   TO_PRINTABLE
        rcall   EMIT_A
        decf    Rrw, F, A
        bnz     DUMP4
        movf    Rminus, W, A
DUMP7:
        decf    Rminus, F, A
        movlw   0
        subwfb  Rplus, F, A
        bc      DUMP1
        rcall   RDROP
        goto    DROP

; IALLOT   n --    allocate n bytes in ROM
;       dw      link
;link   set     $
        NF 1,?
IALLOT:
        rcall   IDP
        goto    PLUSSTORE

;***************************************************************
; check that the relative address is within reach of conditional branch
; instructions and leave the clipped relative address on the stack
; br?   ( rel-addr limit -- clipped-rel-addr)
;       2dup 2/ swap
;       abs > (qabort)
;       and 2/ ;
        dw     L_DUMP
L_BRQ:
        NF 3,br?
BRQ:
        call    TWODUP
        call    TWOSLASH
        rcall   SWOP_A          ; rel-addr limit limit' rel-addr
        call    ABS             ; rel-addr limit limit' rel-addr
        call    GREATER
        call    XSQUOTE
        db      3
        STRING  BR?
        rcall   QABORT         ;  ?RANGE ABORT if TRUE
BRQ1:
        call    AND_
        goto    TWOSLASH

; ,?0=    -- addr  Compile ?0= and make make place for a branch instruction
        db      NFA|4,',','?','0','='
        align   2
COMMAZEROSENSE:
        btfsc   BANKMASK(FLAGS1), idup, A
        bra     COMMAZEROSENSE1
        rcall   LIT_A
        dw      ZEROSENSE
        bra     COMMAZEROSENSE2
COMMAZEROSENSE1:
        rcall   IDPMINUS
        rcall   LIT_A
        dw      DUPZEROSENSE
COMMAZEROSENSE2:
        bcf     BANKMASK(FLAGS1), idup, A
        bra     INLINE0

; z, ( -- cc)   Zero
        dw      L_BRQ
L_ZC:
        db      NFA|2
        STRING  z
        db      ','
        align   2
ZC:
        rcall   DOCREATE_A
        dw      0xe0

; nz, ( -- cc)  Not Zero
        dw      L_ZC
L_NZC:
        db      NFA|3
        STRING  nz
        db      ','
        align   2
NZC:
        rcall   DOCREATE_A
        dw      0xe1

; cc, ( -- cc) Carry
        dw      L_NZC
L_NCC:
        db      NFA|3
        STRING  nc
        db      ','
        align   2
NCC:
        rcall   DOCREATE_A
        dw      0xe3

; not, ( cc -- opposite-cc)  Reverse the condition code
        dw      L_NCC
L_NOTC:
        db      NFA|4
        STRING  not
        db      ','
        align   2
NOTC:
        call    ONE
        goto    XOR


; if, ( cc -- here)  Assembler if
        dw      L_NOTC
L_IFC:
        db      NFA|3
        STRING  if
        db      ','
        align   2
IFC:
        rcall   IHERE
        rcall   FALSE_
        call    ICCOMMA
        rcall   SWOP_A
        rcall   NOTC
        goto    ICCOMMA

; then, ( back-addr -- )  Assembler then
        dw      L_IFC
L_THENC:
        db      NFA|5
        STRING  then
        db      ','
        align   2
THENC:
        bsf     BANKMASK(FLAGS1), fTAILC, A  ; Disable tail call optimisation
        rcall   DUP_A
        rcall   FETCH_A         ; back-addr oper
        movf    Srw, W, A
        call    TOR
                              ; bra has longer range
        sublw   0xd0          ; Also DUP
        bz      THENC1
THENC0:
        rcall   LIT_A
        dw      0x1ff         ;  back-addr mask
        bra     THENC2
THENC1:
        rcall   LIT_A
        dw      0x0fff         ; back-addr mask
THENC2:
        rcall   TOR_A           ; back-addr
        rcall   IHERE           ; back-addr ihere
        call    OVER            ; back-addr ihere back-addr
        call    MINUS           ; back-addr rel-addr
        rcall   TWOMINUS        ; back-addr rel-addr
        call    RFROM           ; back-addr rel-addr mask
        rcall   BRQ             ; back-addr rel-addr
        call    RFROM           ; back-addr rel-addr oper
        call    OR              ; back-addr instruction
        rcall   SWOP_A
        goto    STORE

; else, ( back-addr -- here )  Assembler else
        dw      L_THENC
L_ELSEC:
        db      NFA|5
        STRING  else
        db      ','
        align   2
ELSEC:
        rcall   IHERE
        rcall   FALSE_
        call    BRA_
        rcall   SWOP_A
        goto    THENC

; begin,    -- adrs        target for bwd. branch
        dw      L_ELSEC
L_BEGINC:
        db      NFA|6
        STRING  begin
        db      ','
        align   2
BEGINC:
        goto    IHERE

; again,    adrs --      uncond'l backward branch
;   unconditional backward branch
        dw      L_BEGINC
L_AGAINC:
        db      NFA|6
        STRING  again
        db      ','
        align   2
AGAINC:
        rcall   IHERE
        call    MINUS
        rcall   TWOMINUS
        goto    BRA_

; until,    adrs cc --   Branch bakwards if cc
        dw      L_AGAINC
L_UNTILC:
        db      NFA|6
        STRING  until
        db      ','
        align   2
UNTILC:
        bsf     BANKMASK(FLAGS1), fTAILC, A  ; Disable tail call optimisation
        rcall   SWOP_A
        rcall   IHERE
        call    MINUS
        rcall   TWOMINUS
        rcall   LIT_A
        dw      0x1ff
        rcall   BRQ
        call    ICCOMMA
        rcall   NOTC
        goto    ICCOMMA

; IF       -- adrs   conditional forward branch
; Leaves address of branch instruction
; and compiles the condition byte
        dw      L_UNTILC
L_IF:
        NF IMMED|COMPILE|2,if
IF_:
        btfsc   BANKMASK(FLAGS1), izeroeq, A
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   NZC
        btfsc   BANKMASK(FLAGS1), izeroeq, A
        rcall   NOTC
        bcf     BANKMASK(FLAGS1), izeroeq, A
        goto    IFC


; THEN     adrs  --        resolve forward branch
        dw      L_IF
L_THEN:
        NF IMMED|COMPILE|4,then
THEN_:
        goto    THENC

; ELSE     adrs1 -- adrs2    branch for IF..ELSE
; Leave adrs2 of bra instruction and store bz in adrs1
; Leave adress of branch instruction and FALSE flag on stack
        dw      L_THEN
L_ELSE:
        NF IMMED|COMPILE|4,else
ELSE_:
        goto    ELSEC

; BEGIN    -- adrs        target for bwd. branch
        dw      L_ELSE
L_BEGIN:
        NF IMMED|COMPILE|5,begin
BEGIN:
        goto    IHERE

;;; Forget the latest compiled two cell instruction
        NF 1,_
IDPMINUS:
        rcall   LIT_A
        dw      -4
        goto    IALLOT

; UNTIL    adrs --   Branch bakwards if true
        dw      L_BEGIN
L_UNTIL:
        NF IMMED|COMPILE|5,until
UNTIL:
        btfsc   BANKMASK(FLAGS1), izeroeq, A
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   ZC
        btfss   BANKMASK(FLAGS1), izeroeq, A
        rcall   NOTC
        bcf     BANKMASK(FLAGS1), izeroeq, A
        goto    UNTILC

; AGAIN    adrs --      uncond'l backward branch
;   unconditional backward branch
        dw      L_UNTIL
L_AGAIN:
        NF IMMED|COMPILE|5,again
AGAIN:
        goto    AGAINC

; WHILE    addr1 -- addr2 addr1         branch for WHILE loop
; addr1 : address of BEGIN
; addr2 : address where to store bz instruction
        dw      L_AGAIN
L_WHILE:
        NF IMMED|COMPILE|5,while
WHILE_:
        rcall   IF_
        goto    SWOP

; REPEAT   addr2 addr1 --     resolve WHILE loop
        dw      L_WHILE
L_REPEAT:
        NF IMMED|COMPILE|6,repeat
REPEAT_:
        rcall   AGAIN
        goto    THEN_

; INLINE  "name" --    Inline following word
        dw      L_REPEAT
L_INLINE:
        NF IMMED|COMPILE|6,inline
        bcf     BANKMASK(FLAGS1), izeroeq, A
        bcf     BANKMASK(FLAGS1), idup, A
        rcall   TICK
        goto    INLINE0

; in, ( addr -- ) begin @+ dup $12 <> while i, repeat 2drop ;
        dw      L_INLINE
L_INLINEC:
        db      NFA|3,'i','n',','
        align   2
INLINE0:
        call    FETCHPP
        movf    Srw, W, A
        bnz     INLINE2
        movf    Sminus, W, A
        movf    Splus, W, A
        sublw   0x12
        bz      INLINE1
INLINE2:
        call    ICOMMA
        bra     INLINE0
INLINE1:
        goto    TWODROP

; FOR   -- bc-addr bra-addr
; ['] >r cf, ihere ['] (for) cf, ihere
        dw      L_INLINEC
L_FOR:
        NF IMMED|COMPILE|3,for
FOR:
        rcall   DOCOMMAXT_A
        dw      TOR
        rcall   IHERE
        rcall   FALSE_
        call    BRA_
        rcall   IHERE
        goto    SWOP

; NEXT bra-addr bc-addr --
        dw      L_FOR
L_NEXT:
        NF IMMED|COMPILE|4,next
NEXT:
        rcall   THENC
        rcall   LIT_A
        dw      XNEXT
        rcall   INLINE0
        rcall   NCC
        rcall   UNTILC
        rcall   LIT_A
        dw      RDROP
        goto    INLINE0


; (next) decrement top of return stack
XNEXT:
        decf    Rminus, F, A
        movlw   0
        subwfb  Rplus, F, A
        return

; endit clear top of return stack
        dw      L_NEXT
L_ENDIT:
        NF COMPILE|5,endit
ENDIT:
        clrf    Rminus, A
        clrf    Rplus, A
        return

; RDROP compile a pop
        dw      L_ENDIT
L_RDROP:
        NF INLINE|5,rdrop
RDROP:
        movwf    Rminus, A
        movwf    Rminus , A ;status flags are not affected my movwf
        return

; S>D  n -- d
        dw      L_RDROP
L_STOD:
        NF 3,s>d
STOD:
        btfsc   Splus, 7, A
        goto    test_true
        goto    test_false


; DNEGATE  +d -- -d
        dw      L_STOD
L_DNEGATE:
        NF 7,dnegate
DNEGATE:
        rcall   DINVERT
        call    ONE
        goto    MPLUS

; DNEGATE  d -n -- -d
        dw      L_DNEGATE
L_QDNEGATE:
        NF 8,?dnegate
QDNEGATE:
        movf    Sminus, W, A
        movwf   Sminus, A
        bnn     QDNEGATE1
        rcall   DNEGATE
QDNEGATE1:
        return

; DABS  -d -- d
        dw      L_QDNEGATE
L_DABS:
        NF 4,dabs
DABS_:
        btfsc   Srw, 7, A
        rcall   DNEGATE
        return

; D+       d d -- d         add double to double
        dw      L_DABS
L_DPLUS:
        NF 2,d+
DPLUS:
        movlw   7
        subwf   Sp, F, A
        movlw   0
        subwfb  Sbank, F, A

        movlw   4
        movf    SWrw, W, A
        addwf   Splus, F, A

        movlw   4
        movf    SWrw, W, A
        addwfc  Splus, F, A

        movlw   4
        movf    SWrw, W, A
        addwfc  Splus, F, A

        movlw   4
        movf    SWrw, W, A
        addwfc  Srw, F, A

        return

; D-    d1 d2 -- d3        double minus
        dw      L_DPLUS
L_DMINUS:
        NF 2,d-
DMINUS:
        rcall   DNEGATE
        goto    DPLUS

; D2/    d1 -- d2        double divide by 2
        dw      L_DMINUS
L_DTWOSLASH:
        NF 3,d2/
DTWOSLASH:
        bcf     CARRY
        btfsc   Srw, 7, A
        bsf     CARRY
        rrcf    Sminus, F, A
        rrcf    Sminus, F, A
        rrcf    Sminus, F, A
        rrcf    Splus, F, A
        movf    Splus, W, A
        movf    Splus, W, A
        return

; D2*    d1 -- d2        double multiply by 2
        dw      L_DTWOSLASH
L_DTWOSTAR:
        NF 3,d2*
DTWOSTAR:
        movf    Sminus, W, A
        movf    Sminus, W, A
        movf    Sminus, W, A
        bcf     CARRY
        rlcf    Splus, F, A
        rlcf    Splus, F, A
        rlcf    Splus, F, A
        rlcf    Srw, F, A
        return

; DINVERT    d1 -- d2        double invert
        dw      L_DTWOSTAR
L_DINVERT:
        NF 7,dinvert
DINVERT:
        comf    Sminus, F, A
        comf    Sminus, F, A
        comf    Sminus, F, A
        comf    Splus, F, A
        movf    Splus, W, A
        movf    Splus, W, A
        return

; D0=    d1 -- f        double zeroequal
        dw      L_DINVERT
L_DZEROEQUAL:
        NF 3,d0=
DZEROEQUAL:
        movf    Sminus, W, A
        iorwf   Sminus, W, A
        iorwf   Sminus, W, A
        iorwf   Sminus, W, A
        bnz     DZEROLESS_FALSE
DZEROEQUAL_TRUE:
        goto    TRUE_

; D0<    d1 -- f        double zeroless
        dw      L_DZEROEQUAL
L_DZEROLESS:
        db      NFA|3,'d','0','<'
        align   2
DZEROLESS:
        movf    Sminus, W, A
        movf    Sminus, F, A
        movf    Sminus, F, A
        movf    Sminus, F, A
        addlw   0
        bn      DZEROEQUAL_TRUE
DZEROLESS_FALSE:
        goto    FALSE_


; D=    d1 d2 -- f        double equal
        dw      L_DZEROLESS
L_DEQUAL:
        NF 2,d=
DEQUAL:
        rcall   DMINUS
        goto    DZEROEQUAL

; D<    d1 d2 -- f        double less than
        dw      L_DEQUAL
L_DLESS:
        db      NFA|2,'d','<'
        align   2
DLESS:
        rcall   DMINUS
        swapf   Sminus, W, A
        swapf   Sminus, W, A
        goto    LESS2

; D>    d1 d2 -- f        double greater than
        dw      L_DLESS
L_DGREATER:
        NF 2,d>
DGREATER:
        call    TWOSWAP
        goto    DLESS


; UD.       ud --         unsigned double dot
        dw      L_DGREATER
L_UDDOT:
        NF 3,ud.
UDDOT:
        call    LESSNUM
        call    NUMS
        call    NUMGREATER
        call    TYPE
        goto    SPACE_

; D.       d --         signed double dot
        dw      L_UDDOT
L_DDOT:
        NF 2,d.
DDOT:
        call    LESSNUM
        call    DUP
        call    TOR
        rcall   DABS_
        call    NUMS
        call    RFROM
        call    SIGN
        call    NUMGREATER
        call    TYPE
        goto    SPACE_

        dw      L_DDOT
L_MEMHI:
        NF 2,hi
MEMHI:
        call    LIT
        dw      FLASHHI
        call    CSE
        call    PLUS
        goto    FETCH
FLASHHI:
        dw      FLASH_HI
        dw      EEPROM_HI
        dw      RAM_HI
;***************************************************
        dw      L_MEMHI
L_FETCH_P:
        NF 2,@p
FETCH_P:
        movf    BANKMASK(p_lo), W, A
        movwf   plusS, A
        movf    BANKMASK(p_hi), W, A
        movwf   plusS, A
        return
;***************************************************
        dw      L_FETCH_P
L_PCFETCH:
        NF 3,pc@ ; ( -- c ) Fetch char from pointer
PCFETCH:
        movf    BANKMASK(p_lo), W, A
        movwf   plusS, A
        movf    BANKMASK(p_hi), W, A
        movwf   plusS, A
        goto    CFETCH
;***************************************************
        dw      L_PCFETCH
L_PNPLUS:
        NF 3,p++ ; ( n -- ) Add n to p
PNPLUS:
        movf    Sminus, W, A
        movwf   Tp, A
        movf    Sminus, W, A
        addwf   BANKMASK(p_lo), F, A
        movf    Tp, W, A
        addwfc  BANKMASK(p_hi), F, A
        return
;***************************************************
; marker --- name
        dw      0
L_MARKER:
lastword:
        NF 6,marker
MARKER:
        call    ROM
        rcall   CREATE
        call    LIT
        dw      dp_start
        call    HERE
        call    TEN
        call    CMOVE
        call    TEN
        call    ALLOT
        call    FRAM
        rcall   XDOES
        call    DODOES
        rcall   INI
        call    TEN
        goto    CMOVE

;        dw      L_RDROP
L_DOTBASE:
        NF 1,I
DOTBASE:
        call    BASE
        call    FETCH_A
        movf    Sminus, W, A
        movf    Srw, W, A
        xorlw   0x10
        bnz     DOTBASE1
        movlw   '$'
        bra     DOTBASEEND
DOTBASE1:
        xorlw   0x1a
        bnz     DOTBASE2
        movlw   '#'
        bra     DOTBASEEND
DOTBASE2:
        xorlw   0x8
        bnz     DOTBASE3
        movlw   '%'
        bra     DOTBASEEND
DOTBASE3:
        movlw   '?'
DOTBASEEND:
        movwf   Srw, A
        clrf    plusS, A
        return

;;FF_END_CODE code
MEMQADDR_N:
        dw      ROM_N
        dw      EROM_N
        dw      FRAM_N
; M? -- caddr count    current data space string
;        dw      L_DOTBASE
L_MEMQ:
        NF 1,I
MEMQ:
        call    CSE
        call    LIT
        dw      MEMQADDR_N
        call    PLUS
        call    FETCH_A
        call    CFETCHPP
        call    LIT
        dw      NFAmask
        goto    AND_

#ifdef USB_CDC
#include "ff-usbcdc.S"
#endif
#if 0
hexemit:
        swapf Srw, W, A
        rcall hexemit2
        movf  Sminus, W, A
        rcall hexemit2
        swapf Srw, W, A
        rcall hexemit2
        movf  Splus, W, A
        rcall hexemit2
        movlw ' '
        call  asmemit
        return
hexemit2:
        andlw   0xf
        addlw   0xf6
        bn      hexemit3
        addlw   0x27
hexemit3:
        addlw   0x3a
        call  asmemit
        return
#endif
dpcode:
;;****************************************************
        END resetVec
